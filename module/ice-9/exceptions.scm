;;; Exceptions
;;; Copyright (C) 2019 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Definition of the standard exception types.
;;;
;;; Code:


(define-module (ice-9 exceptions)
  #:re-export (&exception
               make-exception
               make-exception-type
               simple-exceptions
               exception?
               exception-type?
               exception-predicate
               exception-accessor)
  #:export (define-exception-type

            &message
            make-exception-with-message
            exception-with-message?
            exception-message

            &warning
            make-warning
            warning?

            &error
            make-error
            error?

            &external-error
	    make-external-error
	    external-error?
	
            &programming-error
	    make-programming-error
	    programming-error?

	    &assertion-failure
	    make-assertion-failure
	    assertion-failure?

	    &irritants
	    make-exception-with-irritants
            exception-with-irritants?
	    exception-irritants

	    &origin
	    make-exception-with-origin
            exception-with-origin?
	    exception-origin

            &non-continuable
            make-non-continuable-error
            non-continuable-error?

            &implementation-restriction
            make-implementation-restriction-error
            implementation-restriction-error?

            &lexical
            make-lexical-error
            lexical-error?

            &syntax
            make-syntax-error
            syntax-error?
            syntax-error-form
            syntax-error-subform

            &undefined-variable
            make-undefined-variable-error
            undefined-variable-error?

            with-exception-handler
            raise-exception
            raise-continuable))

(define-syntax define-exception-type
  (syntax-rules ()
    ((_ exception-type supertype constructor predicate
	(field accessor) ...)
     (begin
       (define exception-type
         (make-record-type 'exception-type '((immutable field) ...)
                           #:parent supertype #:extensible? #t))
       (define constructor (record-constructor exception-type))
       (define predicate (exception-predicate exception-type))
       (define accessor
         (exception-accessor exception-type
                             (record-accessor exception-type 'field)))
       ...))))

(define-exception-type &error &exception
  make-error error?)
(define-exception-type &programming-error &error
  make-programming-error programming-error?)
(define-exception-type &assertion-failure &programming-error
  make-assertion-failure assertion-failure?)

(define-exception-type &message &exception 
  make-exception-with-message exception-with-message? 
  (message exception-message))

(define-exception-type &warning &exception
  make-warning warning?)

(define-exception-type &external-error &error
  make-external-error external-error?)

(define-exception-type &irritants &exception
  make-exception-with-irritants exception-with-irritants?
  (irritants exception-irritants))

(define-exception-type &origin &exception
  make-exception-with-origin exception-with-origin?
  (origin exception-origin))

(define-exception-type &non-continuable &programming-error
  make-non-continuable-error
  non-continuable-error?)

(define-exception-type &implementation-restriction &programming-error
  make-implementation-restriction-error
  implementation-restriction-error?)

(define-exception-type &lexical &programming-error
  make-lexical-error lexical-error?)

(define-exception-type &syntax &programming-error
  make-syntax-error syntax-error?
  (form syntax-error-form)
  (subform syntax-error-subform))

(define-exception-type &undefined-variable &programming-error
  make-undefined-variable-error undefined-variable-error?)

;; When a native guile exception is caught by with-exception-handler, we
;; convert it to a compound exception that includes not only the
;; standard exception objects expected by users of R6RS, SRFI-35, and
;; R7RS, but also a special &guile condition that preserves the original
;; KEY and ARGS passed to the native Guile catch handler.

(define-exception-type &guile &exception
  make-guile-exception guile-exception?
  (key  guile-exception-key)
  (args guile-exception-args))

(define (default-guile-exception-converter key args)
  (make-exception (make-error)
                  (guile-common-exceptions key args)))

(define (guile-common-exceptions key args)
  (apply (case-lambda
          ((subr msg margs . _)
           (make-exception
            (make-exception-with-origin subr)
            (make-exception-with-message msg)
            (make-exception-with-irritants margs)))
          (_ (make-exception-with-irritants args)))
         args))

(define (convert-guile-exception key args)
  (let ((converter (assv-ref guile-exception-converters key)))
    (make-exception (or (and converter (converter key args))
                        (default-guile-exception-converter key args))
                    ;; Preserve the original KEY and ARGS in the R6RS
                    ;; exception object.
                    (make-guile-exception key args))))

;; If an exception handler chooses not to handle a given exception, it
;; will re-raise the exception to pass it on to the next handler.  If
;; the exception was converted from a native Guile exception, we must
;; re-raise using the native Guile facilities and the original exception
;; KEY and ARGS.  We arrange for this in 'raise' so that native Guile
;; exception handlers will continue to work when mixed with
;; with-exception-handler.

(define &raise-object-wrapper
  (make-record-type '&raise-object-wrapper
                    '((immutable obj) (immutable continuation))))
(define make-raise-object-wrapper
  (record-constructor &raise-object-wrapper))
(define raise-object-wrapper?
  (record-predicate &raise-object-wrapper))
(define raise-object-wrapper-obj
  (record-accessor &raise-object-wrapper 'obj))
(define raise-object-wrapper-continuation
  (record-accessor &raise-object-wrapper 'continuation))

(define (raise-exception obj)
  (if (guile-exception? obj)
      (apply throw (guile-exception-key obj) (guile-exception-args obj))
      (throw '%exception (make-raise-object-wrapper obj #f))))

(define (raise-continuable obj)
  (call/cc
   (lambda (k)
     (throw '%exception (make-raise-object-wrapper obj k)))))

(define (with-exception-handler handler thunk)
  (with-throw-handler #t
    thunk
    (lambda (key . args)
      (cond ((not (eq? key '%exception))
             (let ((obj (convert-guile-exception key args)))
               (handler obj)
               (raise-exception (make-non-continuable-error))))
            ((and (not (null? args))
                  (raise-object-wrapper? (car args)))
             (let* ((cargs (car args))
                    (obj (raise-object-wrapper-obj cargs))
                    (continuation (raise-object-wrapper-continuation cargs))
                    (handler-return (handler obj)))
               (if continuation
                   (continuation handler-return)
                   (raise-exception (make-non-continuable-error)))))))))

;;; Exception printing

(define (exception-printer port key args punt)
  (cond ((and (= 1 (length args))
              (raise-object-wrapper? (car args)))
         (let ((obj (raise-object-wrapper-obj (car args))))
           (cond ((exception? obj)
                  (display "ERROR:\n" port)
                  (format-exception port obj))
                 (else
                  (format port "ERROR: `~s'" obj)))))
        (else
         (punt))))

(define (format-exception port exception)
  (let ((components (simple-exceptions exception)))
    (if (null? components)
        (format port "Empty exception object")
        (let loop ((i 1) (components components))
          (cond ((pair? components)
                 (format port "  ~a. " i)
                 (format-simple-exception port (car components))
                 (when (pair? (cdr components))
                   (newline port))
                 (loop (+ i 1) (cdr components))))))))

(define (format-simple-exception port exception)
  (let* ((type (struct-vtable exception))
         (name (record-type-name type))
         (fields (record-type-fields type)))
    (cond
     ((null? fields)
      (format port "~a" name))
     ((null? (cdr fields))
      (format port "~a: ~s" name (struct-ref exception 0)))
     (else
      (format port "~a:\n" name)
      (let lp ((fields fields) (i 0))
        (let ((field (car fields))
              (fields (cdr fields)))
          (format port "      ~a: ~s" field (struct-ref exception i))
          (unless (null? fields)
            (newline port)
            (lp fields (+ i 1)))))))))

(set-exception-printer! '%exception exception-printer)

;; Guile exception converters
;;
;; Each converter is a procedure (converter KEY ARGS) that returns
;; either an exception object or #f.  If #f is returned,
;; 'default-guile-exception-converter' will be used.

(define (guile-syntax-error-converter key args)
  (apply (case-lambda
          ((who what where form subform . extra)
           (make-exception (make-syntax-error form subform)
                           (make-exception-with-origin who)
                           (make-exception-with-message what)))
          (_ #f))
         args))

(define (guile-lexical-error-converter key args)
  (make-exception (make-lexical-error)
                  (guile-common-exceptions key args)))

(define (guile-assertion-failure-converter key args)
  (make-exception (make-assertion-failure)
                  (guile-common-exceptions key args)))

(define (guile-undefined-variable-error-converter key args)
  (make-exception (make-undefined-variable-error)
                  (guile-common-exceptions key args)))

(define (guile-implementation-restriction-converter key args)
  (make-exception (make-implementation-restriction-error)
                  (guile-common-exceptions key args)))

(define (guile-external-error-converter key args)
  (make-exception (make-external-error)
                  (guile-common-exceptions key args)))

(define (guile-system-error-converter key args)
  (apply (case-lambda
          ((subr msg msg-args errno . rest)
           ;; XXX TODO we should return a more specific error
           ;; (usually an I/O error) as expected by R6RS programs.
           ;; Unfortunately this often requires the 'filename' (or
           ;; other?) which is not currently provided by the native
           ;; Guile exceptions.
           (make-exception (make-external-error)
                           (guile-common-exceptions key args)))
          (_ (guile-external-error-converter key args)))
         args))

;; TODO: Arrange to have the needed information included in native
;;       Guile I/O exceptions, and arrange here to convert them to the
;;       proper exceptions.  Remove the earlier exception conversion
;;       mechanism: search for 'with-throw-handler' in the 'rnrs'
;;       tree, e.g. 'with-i/o-filename-exceptions' and
;;       'with-i/o-port-error' in (rnrs io ports).

;; XXX TODO: How should we handle the 'misc-error', 'vm-error', and
;;           'signal' native Guile exceptions?

;; XXX TODO: Should we handle the 'quit' exception specially?

;; An alist mapping native Guile exception keys to converters.
(define guile-exception-converters
  `((read-error                . ,guile-lexical-error-converter)
    (syntax-error              . ,guile-syntax-error-converter)
    (unbound-variable          . ,guile-undefined-variable-error-converter)
    (wrong-number-of-args      . ,guile-assertion-failure-converter)
    (wrong-type-arg            . ,guile-assertion-failure-converter)
    (keyword-argument-error    . ,guile-assertion-failure-converter)
    (out-of-range              . ,guile-assertion-failure-converter)
    (regular-expression-syntax . ,guile-assertion-failure-converter)
    (program-error             . ,guile-assertion-failure-converter)
    (goops-error               . ,guile-assertion-failure-converter)
    (null-pointer-error        . ,guile-assertion-failure-converter)
    (system-error              . ,guile-system-error-converter)
    (host-not-found            . ,guile-external-error-converter)
    (getaddrinfo-error         . ,guile-external-error-converter)
    (no-data                   . ,guile-external-error-converter)
    (no-recovery               . ,guile-external-error-converter)
    (try-again                 . ,guile-external-error-converter)
    (stack-overflow            . ,guile-implementation-restriction-converter)
    (numerical-overflow        . ,guile-implementation-restriction-converter)
    (memory-allocation-error   . ,guile-implementation-restriction-converter)))

(define (set-guile-exception-converter! key proc)
  (set! guile-exception-converters
        (acons key proc guile-exception-converters)))
