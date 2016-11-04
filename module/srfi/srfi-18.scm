;;; srfi-18.scm --- Multithreading support

;; Copyright (C) 2008, 2009, 2010, 2012, 2014 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Author: Julian Graham <julian.graham@aya.yale.edu>
;;; Date: 2008-04-11

;;; Commentary:

;; This is an implementation of SRFI-18 (Multithreading support).
;;
;; All procedures defined in SRFI-18, which are not already defined in
;; the Guile core library, are exported.
;;
;; This module is fully documented in the Guile Reference Manual.

;;; Code:

(define-module (srfi srfi-18)
  #:use-module ((ice-9 threads) #:prefix threads:)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-34) #:prefix srfi-34:)
  #:use-module ((srfi srfi-35) #:select (define-condition-type
                                          &error
                                          condition))
  #:export (;; Threads
            make-thread
            thread-name
            thread-specific
            thread-specific-set!
            thread-start!
            thread-yield!
            thread-sleep!
            thread-terminate!
            thread-join!

            ;; Mutexes
            make-mutex
            mutex
            mutex-name
            mutex-specific
            mutex-specific-set!
            mutex-state
            mutex-lock!
            mutex-unlock!

            ;; Condition variables
            make-condition-variable
            condition-variable-name
            condition-variable-specific
            condition-variable-specific-set!
            condition-variable-signal!
            condition-variable-broadcast!
            condition-variable-wait!

            ;; Time
            current-time
            time?
            time->seconds
            seconds->time
 
            current-exception-handler
            with-exception-handler
            join-timeout-exception?
            abandoned-mutex-exception?
            terminated-thread-exception?
            uncaught-exception?
            uncaught-exception-reason)
  #:re-export ((threads:condition-variable? . condition-variable?)
               (threads:current-thread . current-thread)
               (threads:thread? . thread?)
               (srfi-34:raise . raise))
  #:replace (current-time
             make-thread
             make-mutex
             mutex?
             make-condition-variable))

(unless (provided? 'threads)
  (error "SRFI-18 requires Guile with threads support"))

(cond-expand-provide (current-module) '(srfi-18))

(define (check-arg-type pred arg caller)
  (if (pred arg)
      arg
      (scm-error 'wrong-type-arg caller
		 "Wrong type argument: ~S" (list arg) '())))

(define-condition-type &abandoned-mutex-exception &error
  abandoned-mutex-exception?)
(define-condition-type &join-timeout-exception &error
  join-timeout-exception?)
(define-condition-type &terminated-thread-exception &error
  terminated-thread-exception?)
(define-condition-type &uncaught-exception &error
  uncaught-exception?
  (reason uncaught-exception-reason))

(define-record-type <mutex>
  (%make-mutex prim name specific)
  mutex?
  (prim mutex-prim)
  (name mutex-name)
  (specific mutex-specific mutex-specific-set!))

(define object-names (make-weak-key-hash-table))
(define object-specifics (make-weak-key-hash-table))
(define thread-start-conds (make-weak-key-hash-table))
(define thread->exception (make-object-property))
(define thread-mutexes (make-parameter #f))

;; EXCEPTIONS

;; All threads created by SRFI-18 have an initial handler installed that
;; will squirrel away an uncaught exception to allow it to bubble out to
;; joining threads.  However for the main thread and other threads not
;; created by SRFI-18, just let the exception bubble up by passing on
;; doing anything with the exception.
(define (exception-handler-for-foreign-threads obj)
  (values))

(define current-exception-handler
  (make-parameter exception-handler-for-foreign-threads))

(define (with-exception-handler handler thunk)
  (check-arg-type procedure? handler "with-exception-handler")
  (check-arg-type thunk? thunk "with-exception-handler")
  (srfi-34:with-exception-handler
   (let ((prev-handler (current-exception-handler)))
     (lambda (obj)
       (parameterize ((current-exception-handler prev-handler))
         (handler obj))))
   (lambda ()
     (parameterize ((current-exception-handler handler))
       (thunk)))))

;; THREADS

;; Create a new thread and prevent it from starting using a condition variable.
;; Once started, install a top-level exception handler that rethrows any 
;; exceptions wrapped in an uncaught-exception wrapper. 

(define (with-thread-mutex-cleanup thunk)
  (let ((mutexes (make-weak-key-hash-table)))
    (dynamic-wind
      values
      (lambda ()
        (parameterize ((thread-mutexes mutexes))
          (thunk)))
      (lambda ()
        (let ((thread (current-thread)))
          (hash-for-each (lambda (mutex _)
                           (when (eq? (mutex-state mutex) thread)
                             (abandon-mutex! mutex)))
                         mutexes))))))

(define* (make-thread thunk #:optional name)
  (let ((sm (make-mutex 'start-mutex))
        (sc (make-condition-variable 'start-condition-variable)))
    (mutex-lock! sm)
    (let ((t (threads:call-with-new-thread
              (lambda ()
                (catch #t
                  (lambda ()
                    (with-thread-mutex-cleanup
                     (lambda ()
                       (mutex-lock! sm)
                       (threads:signal-condition-variable sc)
                       (mutex-unlock! sm sc)
                       (thunk))))
                  (lambda (key . args)
                    (set! (thread->exception (threads:current-thread))
                      (condition (&uncaught-exception
                                  (reason
                                   (match (cons key args)
                                     (('srfi-34 obj) obj)
                                     (obj obj))))))))))))
      (when name (hashq-set! object-names t name))
      (threads:wait-condition-variable sc (mutex-prim sm))
      (hashq-set! thread-start-conds t (cons sm sc))
      (mutex-unlock! sm)
      t)))

(define (thread-name thread)
  (hashq-ref object-names
             (check-arg-type threads:thread? thread "thread-name")))

(define (thread-specific thread)
  (hashq-ref object-specifics 
	     (check-arg-type threads:thread? thread "thread-specific")))

(define (thread-specific-set! thread obj)
  (hashq-set! object-specifics
	      (check-arg-type threads:thread? thread "thread-specific-set!")
	      obj)
  *unspecified*)

(define (thread-start! thread)
  (match (hashq-ref thread-start-conds
                    (check-arg-type threads:thread? thread "thread-start!"))
    ((smutex . scond)
     (hashq-remove! thread-start-conds thread)
     (mutex-lock! smutex)
     (threads:signal-condition-variable scond)
     (mutex-unlock! smutex))
    (#f #f))
  thread)

(define (thread-yield!) (threads:yield) *unspecified*)

(define (thread-sleep! timeout)
  (let* ((ct (time->seconds (current-time)))
	 (t (cond ((time? timeout) (- (time->seconds timeout) ct))
		  ((number? timeout) (- timeout ct))
		  (else (scm-error 'wrong-type-arg "thread-sleep!"
				   "Wrong type argument: ~S" 
				   (list timeout) 
				   '()))))
	 (secs (inexact->exact (truncate t)))
	 (usecs (inexact->exact (truncate (* (- t secs) 1000000)))))
    (when (> secs 0) (sleep secs))
    (when (> usecs 0) (usleep usecs))
    *unspecified*))

;; Whereas SRFI-34 leaves the continuation of a call to an exception
;; handler unspecified, SRFI-18 has this to say:
;;
;;   When one of the primitives defined in this SRFI raises an exception
;;   defined in this SRFI, the exception handler is called with the same
;;   continuation as the primitive (i.e. it is a tail call to the
;;   exception handler).
;;
;; Therefore arrange for exceptions thrown by SRFI-18 primitives to run
;; handlers with the continuation of the primitive call, for those
;; primitives that throw exceptions.

(define (with-exception-handlers-here thunk)
  (let ((tag (make-prompt-tag)))
    (call-with-prompt tag
      (lambda ()
        (with-exception-handler (lambda (exn) (abort-to-prompt tag exn))
          thunk))
      (lambda (k exn)
        ((current-exception-handler) exn)))))

;; A unique value.
(define %cancel-sentinel (list 'cancelled))
(define (thread-terminate! thread)
  (threads:cancel-thread thread %cancel-sentinel)
  *unspecified*)

;; A unique value.
(define %timeout-sentinel (list 1))
(define* (thread-join! thread #:optional (timeout %timeout-sentinel)
                       (timeoutval %timeout-sentinel))
  (with-exception-handlers-here
   (lambda ()
     (let ((v (if (eq? timeout %timeout-sentinel)
                  (threads:join-thread thread)
                  (threads:join-thread thread timeout %timeout-sentinel))))
       (cond
        ((eq? v %timeout-sentinel)
         (if (eq? timeoutval %timeout-sentinel)
             (srfi-34:raise (condition (&join-timeout-exception)))
             timeoutval))
        ((eq? v %cancel-sentinel)
         (srfi-34:raise (condition (&terminated-thread-exception))))
        ((thread->exception thread) => srfi-34:raise)
        (else v))))))

;; MUTEXES
;; These functions are all pass-thrus to the existing Guile implementations.

(define* (make-mutex #:optional name)
  (%make-mutex (threads:make-mutex 'unchecked-unlock
                                   'allow-external-unlock
                                   'recursive)
               name
               #f))

(define (mutex-state mutex)
  (let* ((prim (mutex-prim mutex))
         (owner (threads:mutex-owner prim)))
    (if owner
        (if (threads:thread-exited? owner) 'abandoned owner)
        (if (> (threads:mutex-level prim) 0) 'not-owned 'not-abandoned))))

(define (abandon-mutex! mutex)
  #t)

(define (mutex-lock! mutex . args) 
  (let ((mutexes (thread-mutexes)))
    (when mutexes
      (hashq-set! mutexes mutex #t)))
  (with-exception-handlers-here
   (lambda ()
     (catch 'abandoned-mutex-error
       (lambda () (apply threads:lock-mutex (mutex-prim mutex) args))
       (lambda (key . args)
         (srfi-34:raise
          (condition (&abandoned-mutex-exception))))))))

(define (mutex-unlock! mutex . args) 
  (apply threads:unlock-mutex (mutex-prim mutex) args))

;; CONDITION VARIABLES
;; These functions are all pass-thrus to the existing Guile implementations.

(define* (make-condition-variable #:optional name)
  (let ((m (threads:make-condition-variable)))
    (when name (hashq-set! object-names m name))
    m))

(define (condition-variable-name condition-variable)
  (hashq-ref object-names (check-arg-type threads:condition-variable? 
					  condition-variable
					  "condition-variable-name")))

(define (condition-variable-specific condition-variable)
  (hashq-ref object-specifics (check-arg-type threads:condition-variable? 
					      condition-variable 
					      "condition-variable-specific")))

(define (condition-variable-specific-set! condition-variable obj)
  (hashq-set! object-specifics
	      (check-arg-type threads:condition-variable? 
			      condition-variable 
			      "condition-variable-specific-set!")
	      obj)
  *unspecified*)

(define (condition-variable-signal! cond) 
  (threads:signal-condition-variable cond) 
  *unspecified*)

(define (condition-variable-broadcast! cond)
  (threads:broadcast-condition-variable cond)
  *unspecified*)

;; TIME

(define current-time gettimeofday)
(define (time? obj)
  (and (pair? obj)
       (let ((co (car obj))) (and (integer? co) (>= co 0)))
       (let ((co (cdr obj))) (and (integer? co) (>= co 0)))))

(define (time->seconds time) 
  (and (check-arg-type time? time "time->seconds")
       (+ (car time) (/ (cdr time) 1000000))))

(define (seconds->time x)
  (and (check-arg-type number? x "seconds->time")
       (let ((fx (truncate x)))
	 (cons (inexact->exact fx)
	       (inexact->exact (truncate (* (- x fx) 1000000)))))))

;; srfi-18.scm ends here
