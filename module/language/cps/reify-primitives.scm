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
;;; A pass to reify lone $prim's that were never folded into a
;;; $primcall, and $primcall's to primitives that don't have a
;;; corresponding VM op.
;;;
;;; Code:

(define-module (language cps reify-primitives)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps primitives)
  #:use-module (language cps intmap)
  #:use-module (language bytecode)
  #:export (reify-primitives))

(define (module-box cps src module name public? bound? val-proc)
  (with-cps cps
    (letv box)
    (let$ body (val-proc box))
    (letk kbox ($kargs ('box) (box) ,body))
    (build-term ($continue kbox src
                  ($primcall 'cached-module-box
                             (list module name public? bound?) ())))))

(define (primitive-module name)
  (case name
    ((bytevector?
      bytevector-length

      bytevector-u8-ref bytevector-u8-set!
      bytevector-s8-ref bytevector-s8-set!

      bytevector-u16-ref bytevector-u16-set!
      bytevector-u16-native-ref bytevector-u16-native-set!
      bytevector-s16-ref bytevector-s16-set!
      bytevector-s16-native-ref bytevector-s16-native-set!

      bytevector-u32-ref bytevector-u32-set!
      bytevector-u32-native-ref bytevector-u32-native-set!
      bytevector-s32-ref bytevector-s32-set!
      bytevector-s32-native-ref bytevector-s32-native-set!

      bytevector-u64-ref bytevector-u64-set!
      bytevector-u64-native-ref bytevector-u64-native-set!
      bytevector-s64-ref bytevector-s64-set!
      bytevector-s64-native-ref bytevector-s64-native-set!

      bytevector-ieee-single-ref bytevector-ieee-single-set!
      bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
      bytevector-ieee-double-ref bytevector-ieee-double-set!
      bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!)
     '(rnrs bytevectors))
    ((atomic-box?
      make-atomic-box atomic-box-ref atomic-box-set!
      atomic-box-swap! atomic-box-compare-and-swap!)
     '(ice-9 atomic))
    ((current-thread) '(ice-9 threads))
    ((class-of) '(oop goops))
    ((u8vector-ref
      u8vector-set! s8vector-ref s8vector-set!
      u16vector-ref u16vector-set! s16vector-ref s16vector-set!
      u32vector-ref u32vector-set! s32vector-ref s32vector-set!
      u64vector-ref u64vector-set! s64vector-ref s64vector-set!
      f32vector-ref f32vector-set! f64vector-ref f64vector-set!)
     '(srfi srfi-4))
    (else '(guile))))

(define (primitive-ref cps name k src)
  (module-box cps src (primitive-module name) name #f #t
              (lambda (cps box)
                (with-cps cps
                  (build-term
                    ($continue k src ($primcall 'box-ref #f (box))))))))

(define (builtin-ref cps idx k src)
  (with-cps cps
    (build-term
      ($continue k src ($primcall 'builtin-ref idx ())))))

(define (reify-clause cps ktail)
  (with-cps cps
    (letv throw)
    (let$ throw-body
          (with-cps-constants ((wna 'wrong-number-of-args)
                               (false #f)
                               (str "Wrong number of arguments")
                               (eol '()))
            (build-term
              ($continue ktail #f
                ($call throw (wna false str eol false))))))
    (letk kthrow ($kargs ('throw) (throw) ,throw-body))
    (let$ body (primitive-ref 'throw kthrow #f))
    (letk kbody ($kargs () () ,body))
    (letk kclause ($kclause ('() '() #f '() #f) kbody #f))
    kclause))

;; A $kreceive continuation should have only one predecessor.
(define (uniquify-receive cps k)
  (match (intmap-ref cps k)
    (($ $kreceive ($ $arity req () rest () #f) kargs)
     (with-cps cps
       (letk k ($kreceive req rest kargs))
       k))
    (_
     (with-cps cps k))))

(define (reify-primitives cps)
  (define (visit-cont label cont cps)
    (define (resolve-prim cps name k src)
      (cond
       ((builtin-name->index name)
        => (lambda (idx) (builtin-ref cps idx k src)))
       (else
        (primitive-ref cps name k src))))
    (match cont
      (($ $kfun src meta self tail #f)
       (with-cps cps
         (let$ clause (reify-clause tail))
         (setk label ($kfun src meta self tail clause))))
      (($ $kargs names vars ($ $continue k src ($ $prim name)))
       (with-cps cps
         (let$ k (uniquify-receive k))
         (let$ body (resolve-prim name k src))
         (setk label ($kargs names vars ,body))))
      (($ $kargs names vars
          ($ $continue k src ($ $primcall 'call-thunk/no-inline #f (proc))))
       (with-cps cps
         (setk label ($kargs names vars ($continue k src ($call proc ()))))))
      (($ $kargs names vars ($ $continue k src ($ $primcall name param args)))
       (cond
        ((or (prim-instruction name) (branching-primitive? name))
         ;; Assume arities are correct.
         (let ()
           (define (u6? val) (and (exact-integer? val) (<= 0 val 63)))
           (define (u8? val) (and (exact-integer? val) (<= 0 val 255)))
           (define-syntax-rule (reify-constants wrap
                                ((op (pred? c) in ...) (op* out ...))
                                ...
                                (_ default))
             (match name
               ('op
                (if (pred? param)
                    cps
                    (match args
                      ((in ...)
                       (with-cps cps
                         (letv c)
                         (letk kconst ($kargs ('c) (c)
                                        ($continue k src
                                          ($primcall 'op* #f (out ...)))))
                         (setk label
                               ($kargs names vars
                                 ($continue kconst src wrap))))))))
               ...
               (_ default)))
           (define-syntax-rule (reify-scm-constants clause ...)
             (reify-constants ($const param) clause ...))
           (define-syntax-rule (reify-u64-constants clause ...)
             (reify-constants ($primcall 'load-u64 param ()) clause ...))
           (reify-scm-constants
            ((add/immediate (u8? y) x) (add x y))
            ((sub/immediate (u8? y) x) (sub x y))
            (_
             (reify-u64-constants
              ((make-vector/immediate (u8? size) init) (make-vector size init))
              ((vector-ref/immediate (u8? idx) v) (vector-ref v idx))
              ((vector-set!/immediate (u8? idx) v val) (vector-set! v idx val))
              ((allocate-struct/immediate (u8? size) vt) (allocate-struct vt size))
              ((struct-ref/immediate (u8? idx) s) (struct-ref s idx))
              ((struct-set!/immediate (u8? idx) s val) (struct-set! s idx val))
              ((uadd/immediate (u8? y) x) (uadd x y))
              ((usub/immediate (u8? y) x) (usub x y))
              ((umul/immediate (u8? y) x) (umul x y))
              ((rsh/immediate (u6? y) x) (rsh x y))
              ((lsh/immediate (u6? y) x) (lsh x y))
              ((ursh/immediate (u6? y) x) (ursh x y))
              ((ulsh/immediate (u6? y) x) (ulsh x y))
              (_ cps))))))
        (param (error "unexpected param to reified primcall" name))
        (else
         (with-cps cps
           (letv proc)
           (let$ k (uniquify-receive k))
           (letk kproc ($kargs ('proc) (proc)
                               ($continue k src ($call proc args))))
           (let$ body (resolve-prim name kproc src))
           (setk label ($kargs names vars ,body))))))
      (($ $kargs names vars ($ $continue k src ($ $call proc args)))
       (with-cps cps
         (let$ k (uniquify-receive k))
         (setk label ($kargs names vars
                       ($continue k src ($call proc args))))))
      (($ $kargs names vars ($ $continue k src ($ $callk k* proc args)))
       (with-cps cps
         (let$ k (uniquify-receive k))
         (setk label ($kargs names vars
                       ($continue k src ($callk k* proc args))))))
      (_ cps)))

  (with-fresh-name-state cps
    (persistent-intmap (intmap-fold visit-cont cps cps))))
