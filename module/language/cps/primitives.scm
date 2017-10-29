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
;;; Information about named primitives, as they appear in $prim and
;;; $primcall.
;;;
;;; Code:

(define-module (language cps primitives)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-26)
  #:use-module (language bytecode)
  #:export (prim-instruction
            branching-primitive?
            heap-type-predicate?
            prim-arity
            ))

(define *instruction-aliases*
  '((+ . add)
    (- . sub)
    (* . mul)
    (/ . div)
    (quotient . quo) (remainder . rem)
    (modulo . mod)
    (variable-ref . box-ref)
    (variable-set! . box-set!)
    (bytevector-length . bv-length)
    (bytevector-u8-ref . bv-u8-ref)
    (bytevector-u16-native-ref . bv-u16-ref)
    (bytevector-u32-native-ref . bv-u32-ref)
    (bytevector-u64-native-ref . bv-u64-ref)
    (bytevector-s8-ref . bv-s8-ref)
    (bytevector-s16-native-ref . bv-s16-ref)
    (bytevector-s32-native-ref . bv-s32-ref)
    (bytevector-s64-native-ref . bv-s64-ref)
    (bytevector-ieee-single-native-ref . bv-f32-ref)
    (bytevector-ieee-double-native-ref . bv-f64-ref)
    (bytevector-u8-set! . bv-u8-set!)
    (bytevector-u16-native-set! . bv-u16-set!)
    (bytevector-u32-native-set! . bv-u32-set!)
    (bytevector-u64-native-set! . bv-u64-set!)
    (bytevector-s8-set! . bv-s8-set!)
    (bytevector-s16-native-set! . bv-s16-set!)
    (bytevector-s32-native-set! . bv-s32-set!)
    (bytevector-s64-native-set! . bv-s64-set!)
    (bytevector-ieee-single-native-set! . bv-f32-set!)
    (bytevector-ieee-double-native-set! . bv-f64-set!)))

(define *macro-instruction-arities*
  '((cache-current-module! . (0 . 2))
    (cached-toplevel-box . (1 . 3))
    (cached-module-box . (1 . 4))))

(define *immediate-predicates*
  '(fixnum?
    char?
    eq-nil?
    eq-eol?
    eq-false?
    eq-true?
    unspecified?
    undefined?
    eof-object?
    null?  ;; '() or #nil
    false? ;; #f or #nil
    nil?   ;; #f or '() or #nil
    heap-object?))

;; All of the following tests must be dominated by heap-object?.
(define *heap-type-predicates*
  '(pair?
    struct?
    symbol?
    variable?
    vector?
    string?
    keyword?
    bytevector?
    bitvector?
    heap-number?))

;; FIXME: Support these.
(define *other-predicates*
  '(weak-vector?
    hash-table?
    pointer?
    fluid?
    stringbuf?
    dynamic-state?
    frame?
    syntax?
    program?
    vm-continuation?
    weak-set?
    weak-table?
    array?
    port?
    smob?
    bignum?
    flonum?
    complex?
    fraction?))

(define (heap-type-predicate? name)
  "Is @var{name} a predicate that needs guarding by @code{heap-object?}
before it is lowered to CPS?"
  (and (memq name *heap-type-predicates*) #t))

(define *comparisons*
  '(eq?
    heap-numbers-equal?
    <
    <=
    =
    u64-<
    u64-<=
    u64-=
    s64-<
    s64-<=
    s64-=
    f64-=
    f64-<
    f64-<=

    ;; FIXME: Expand these.
    logtest
    u64-<-scm
    u64-<=-scm
    u64-=-scm

    ;; FIXME: Remove these.
    >
    >=
    u64->
    u64->=
    s64->
    s64->=
    u64->=-scm
    u64->-scm
    f64->
    f64->=))

(define *branching-primcall-arities* (make-hash-table))
(for-each (lambda (x) (hashq-set! *branching-primcall-arities* x '(1 . 1)))
          *immediate-predicates*)
(for-each (lambda (x) (hashq-set! *branching-primcall-arities* x '(1 . 1)))
          *heap-type-predicates*)
(for-each (lambda (x) (hashq-set! *branching-primcall-arities* x '(1 . 2)))
          *comparisons*)

(define (compute-prim-instructions)
  (let ((table (make-hash-table)))
    (for-each
     (match-lambda ((inst . _) (hashq-set! table inst inst)))
     (instruction-list))
    (for-each
     (match-lambda ((prim . inst) (hashq-set! table prim inst)))
     *instruction-aliases*)
    (for-each
     (match-lambda ((inst . arity) (hashq-set! table inst inst)))
     *macro-instruction-arities*)
    table))

(define *prim-instructions* (delay (compute-prim-instructions)))

;; prim -> instruction | #f
(define (prim-instruction name)
  (hashq-ref (force *prim-instructions*) name))

(define (branching-primitive? name)
  (and (hashq-ref *branching-primcall-arities* name) #t))

(define *prim-arities* (make-hash-table))

(define (prim-arity name)
  (or (hashq-ref *prim-arities* name)
      (let ((arity (cond
                    ((prim-instruction name) => instruction-arity)
                    ((hashq-ref *branching-primcall-arities* name))
                    (else
                     (error "Primitive of unknown arity" name)))))
        (hashq-set! *prim-arities* name arity)
        arity)))
