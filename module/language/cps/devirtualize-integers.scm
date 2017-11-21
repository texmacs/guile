;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2017 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; Some parts of programs operate on exact integers.  An exact integer
;;; is either a fixnum or a bignum.  It's often the case that if we know
;;; that a number is a fixnum, all operations on it can be unboxed in
;;; terms of s64 operations.  But if there's a series of operations and
;;; each one works on either bignums or fixnums, then the mixing of
;;; fixnums and bignums through that one control and data flow path
;;; makes it impossible for the compiler to specialize operations to
;;; either type.
;;;
;;; This "integer devirtualization" pass tries to duplicate the control
;;; and data flow of exact integers into two flows: one for bignums and
;;; one for fixnums.  This causes code growth, so it's something we need
;;; to be careful about.
;;;
;;; Code:

(define-module (language cps devirtualize-integers)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (language cps)
  #:use-module (language cps effects-analysis)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:export (devirtualize-integers))

;; Compute a map from VAR -> COUNT, where COUNT indicates the number of
;; times in the source program that VAR is used.
(define (compute-use-counts cps)
  (define (add-use use-counts var)
    (let ((count (1+ (intmap-ref use-counts var (lambda (_) 0)))))
      (intmap-add! use-counts var count (lambda (old new) new))))
  (define (add-uses use-counts vars)
    (match vars
      (() use-counts)
      ((var . vars) (add-uses (add-use use-counts var) vars))))
  (persistent-intmap
   (intmap-fold
    (lambda (label cont use-counts)
      (match cont
        (($ $kargs names vars ($ $continue k src exp))
         (match exp
           ((or ($ $const) ($ $prim) ($ $fun) ($ $closure) ($ $rec))
            use-counts)
           (($ $values args)
            (add-uses use-counts args))
           (($ $call proc args)
            (add-uses (add-use use-counts proc) args))
           (($ $callk kfun proc args)
            (add-uses (add-use use-counts proc) args))
           (($ $branch kt ($ $primcall name param args))
            (add-uses use-counts args))
           (($ $primcall name param args)
            (add-uses use-counts args))
           (($ $prompt escape? tag handler)
            (add-use use-counts tag))))
        (_ use-counts)))
    cps
    (transient-intmap))))

(define (peel-trace cps label fx kexit use-counts)
  "For the graph starting at LABEL, try to peel out a trace that uses
the variable FX.  A peelable trace consists of effect-free terms, or
terms that only have &type-check effect but which use FX or some
variable that was defined using FX as an input.  No variable defined in
the trace should be referenced outside of it."
  (let peel-cont ((cps cps) (label label)
                  (live-vars empty-intmap) ;; var -> pending refcount
                  (fresh-vars empty-intmap) ;; old-name -> new name
                  (vars-of-interest (intset-add empty-intset fx))
                  (defs-of-interest? #f))
    (define (fail) (with-cps cps #f))
    (define (add-live-vars live-vars vars)
      (match vars
        (() live-vars)
        ((var . vars)
         (add-live-vars
          (let ((count (intmap-ref use-counts var (lambda (_) 0))))
            (if (zero? count)
                live-vars
                (intmap-add live-vars var count)))
          vars))))
    (define (subtract-uses live-vars vars)
      (match vars
        (() live-vars)
        ((var . vars)
         (subtract-uses
          (let ((count (intmap-ref live-vars var (lambda (_) #f))))
            (cond
             ((not count) live-vars)
             ((= count 1) (intmap-remove live-vars var))
             (else (intmap-replace live-vars var (1- count)))))
          vars))))
    (define (bailout? k)
      (match (intmap-ref cps k)
        (($ $kargs _ _
            ($ $continue _ _
               ($ $primcall (or 'throw 'throw/value 'throw/value+data))))
         #t)
        (_ #f)))
    (match (intmap-ref cps label)
      ;; We know the initial label is a $kargs, and we won't follow the
      ;; graph to get to $kreceive etc, so we can stop with these two
      ;; continuation kinds.
      (($ $ktail) (fail))
      (($ $kargs names vars ($ $continue k src exp))
       (let* ((vars-of-interest
               (if defs-of-interest?
                   (fold1 (lambda (var set) (intset-add set var))
                          vars vars-of-interest)
                   vars-of-interest))
              (live-vars (add-live-vars live-vars vars))
              (fresh-vars (fold (lambda (var fresh-vars)
                                  (intmap-add fresh-vars var (fresh-var)))
                                fresh-vars vars))
              (vars (map (lambda (var) (intmap-ref fresh-vars var)) vars)))
         (define (rename-uses args)
           (map (lambda (arg) (intmap-ref fresh-vars arg (lambda (arg) arg)))
                args))
         (define (any-use-of-interest? args)
           (or-map (lambda (arg) (intset-ref vars-of-interest arg))
                   args))
         (define (continue k live-vars defs-of-interest? can-terminate-trace?
                           exp)
           (define (stitch cps k)
             (with-cps cps
               (letk label* ($kargs names vars ($continue k src ,exp)))
               label*))
           (define (terminate)
             (stitch cps k))
           (with-cps cps
             (let$ k* (peel-cont k live-vars fresh-vars vars-of-interest
                                 defs-of-interest?))
             ($ ((lambda (cps)
                   (cond
                    (k* (stitch cps k*))
                    ((and can-terminate-trace? (eq? live-vars empty-intmap))
                     (terminate))
                    (else (fail))))))))
         (match exp
           (($ $const)
            ;; fine.
            (continue k live-vars #f #f exp))
           (($ $values args)
            (let ((live-vars (subtract-uses live-vars args)))
              (continue k live-vars
                        (any-use-of-interest? args) #f
                        (build-exp ($values ,(rename-uses args))))))
           (($ $primcall name param args)
            ;; exp is effect-free or var of interest in args
            (let* ((fx (expression-effects exp #f))
                   (uses-of-interest? (any-use-of-interest? args))
                   (live-vars (subtract-uses live-vars args)))
              ;; If the primcall uses a value of interest,
              ;; consider it for peeling even if it would cause a
              ;; type check; perhaps the peeling causes the type
              ;; check to go away.
              (if (or (eqv? fx &no-effects)
                      (and uses-of-interest? (eqv? fx &type-check)))
                  (continue k (subtract-uses live-vars args)
                            ;; Primcalls that use values of interest
                            ;; define values of interest.
                            uses-of-interest? #t
                            (build-exp
                              ($primcall name param ,(rename-uses args))))
                  (fail))))
           (($ $branch kt ($ $primcall name param args))
            ;; kt or k is kf; var of interest is in args
            (let* ((live-vars (subtract-uses live-vars args))
                   (uses-of-interest? (any-use-of-interest? args))
                   (defs-of-interest? #f) ;; Branches don't define values.
                   (can-terminate-trace? uses-of-interest?)
                   (exp (build-exp
                          ($primcall name param ,(rename-uses args)))))
              (cond
               ((not (any-use-of-interest? args))
                (fail))
               ((bailout? kt)
                (continue k live-vars defs-of-interest? can-terminate-trace?
                          (build-exp ($branch kt ,exp))))
               ((bailout? k)
                (let ()
                  (define (stitch cps kt)
                    (with-cps cps
                      (letk label*
                            ($kargs names vars
                              ($continue k src ($branch kt ,exp))))
                      label*))
                  (define (terminate)
                    (stitch cps kt))
                  (with-cps cps
                    (let$ kt* (peel-cont kt live-vars fresh-vars
                                         vars-of-interest defs-of-interest?))
                    ($ ((lambda (cps)
                          (cond
                           (kt* (stitch cps kt*))
                           ((and can-terminate-trace? (eq? live-vars empty-intmap))
                            (terminate))
                           (else (fail)))))))))
               (else
                (with-cps cps
                  (letk label*
                        ($kargs names vars
                          ($continue k src ($branch kt ,exp))))
                  label*)))))
           (_ (fail))))))))

(define (peel-traces-in-function cps body use-counts)
  (intset-fold
   (lambda (label cps)
     (match (intmap-ref cps label)
       ;; Traces start with a fixnum? predicate.  We could expand this
       ;; in the future if we wanted to.
       (($ $kargs names vars
           ($ $continue kf src
              ($ $branch kt ($ $primcall 'fixnum? #f (x)))))
        (with-cps cps
          (let$ kt (peel-trace kt x kf use-counts))
          ($ ((lambda (cps)
                (if kt
                    (with-cps cps
                      (setk label
                            ($kargs names vars
                              ($continue kf src
                                ($branch kt ($primcall 'fixnum? #f (x)))))))
                    cps))))))
       (_ cps)))
   body
   cps))

(define (devirtualize-integers cps)
  (let ((use-counts (compute-use-counts cps)))
    (with-fresh-name-state cps
      (intmap-fold
       (lambda (kfun body cps)
         (peel-traces-in-function cps body use-counts))
       (compute-reachable-functions cps)
       cps))))
