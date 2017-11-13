;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015, 2017 Free Software Foundation, Inc.

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
;;; Some bytecode operations can encode an immediate as an operand.
;;; This pass tranforms generic primcalls to these specialized
;;; primcalls, if possible.
;;;
;;; Code:

(define-module (language cps specialize-primcalls)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:export (specialize-primcalls))

(define (specialize-primcalls conts)
  (let ((constants (compute-constant-values conts)))
    (define (uint? var)
      (let ((val (intmap-ref constants var (lambda (_) #f))))
        (and (exact-integer? val) (<= 0 val))))
    (define (u64? var)
      (let ((val (intmap-ref constants var (lambda (_) #f))))
        (and (exact-integer? val) (<= 0 val #xffffFFFFffffFFFF))))
    (define (num? var)
      (number? (intmap-ref constants var (lambda (_) #f))))
    (define (s64? var)
      (let ((val (intmap-ref constants var (lambda (_) #f))))
        (and (exact-integer? val)
             (<= (- #x8000000000000000) val #x7fffFFFFffffFFFF))))
    (define (f64? var)
      (let ((val (intmap-ref constants var (lambda (_) #f))))
        (and (number? val) (inexact? val) (real? val))))
    (define (specialize-primcall name param args)
      (define (rename name)
        (build-exp ($primcall name param args)))
      (define-syntax-rule (specialize-case (pat (op c (arg ...))) ...)
        (match (cons name args)
          (pat
           (let ((c (intmap-ref constants c)))
             (build-exp ($primcall 'op c (arg ...)))))
          ...
          (_ #f)))
      (specialize-case
        (('make-vector (? uint? n) init) (make-vector/immediate n (init)))
        (('vector-ref v (? uint? n)) (vector-ref/immediate n (v)))
        (('vector-set! v (? uint? n) x) (vector-set!/immediate n (v x)))
        (('allocate-struct v (? uint? n)) (allocate-struct/immediate n (v)))
        (('struct-ref s (? uint? n)) (struct-ref/immediate n (s)))
        (('struct-set! s (? uint? n) x) (struct-set!/immediate n (s x)))
        (('add x (? num? y)) (add/immediate y (x)))
        (('add (? num? y) x) (add/immediate y (x)))
        (('sub x (? num? y)) (sub/immediate y (x)))
        (('uadd x (? uint? y)) (uadd/immediate y (x)))
        (('uadd (? uint? y) x) (uadd/immediate y (x)))
        (('usub x (? uint? y)) (usub/immediate y (x)))
        (('umul x (? uint? y)) (umul/immediate y (x)))
        (('umul (? uint? y) x) (umul/immediate y (x)))
        (('ursh x (? uint? y)) (ursh/immediate y (x)))
        (('srsh x (? uint? y)) (srsh/immediate y (x)))
        (('ulsh x (? uint? y)) (ulsh/immediate y (x)))
        (('scm->f64 (? f64? var)) (load-f64 var ()))
        (('scm->u64 (? u64? var)) (load-u64 var ()))
        (('scm->u64/truncate (? u64? var)) (load-u64 var ()))
        (('scm->s64 (? s64? var)) (load-s64 var ()))))
    (intmap-map
     (lambda (label cont)
       (match cont
         (($ $kargs names vars ($ $continue k src ($ $primcall name param args)))
          (let ((exp* (specialize-primcall name param args)))
            (if exp*
                (build-cont
                  ($kargs names vars ($continue k src ,exp*)))
                cont)))
         (_ cont)))
     conts)))

;;; Local Variables:
;;; eval: (put 'specialize-case 'scheme-indent-function 0)
;;; End:
