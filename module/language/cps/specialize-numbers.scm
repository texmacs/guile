;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2015, 2016, 2017 Free Software Foundation, Inc.

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
;;; Some arithmetic operations have multiple implementations: one
;;; polymorphic implementation that works on all kinds of numbers, like
;;; `add', and one or more specialized variants for unboxed numbers of
;;; some kind, like `fadd'.  If we can replace a polymorphic
;;; implementation with a monomorphic implementation, we should do so --
;;; it will speed up the runtime and avoid boxing numbers.
;;;
;;; A polymorphic operation can be specialized if its result is
;;; specialized.  To specialize an operation, we manually unbox its
;;; arguments and box its return value, relying on CSE to remove boxes
;;; where possible.
;;;
;;; We also want to specialize phi variables.  A phi variable is bound
;;; by a continuation with more than one predecessor.  For example in
;;; this code:
;;;
;;;   (+ 1.0 (if a 2.0 3.0))
;;;
;;; We want to specialize this code to:
;;;
;;;   (f64->scm (fl+ (scm->f64 1.0) (if a (scm->f64 2.0) (scm->f64 3.0))))
;;;
;;; Hopefully later passes will remove the conversions.  In any case,
;;; specialization will likely result in a lower heap-number allocation
;;; rate, and that cost is higher than the extra opcodes to do
;;; conversions.  This transformation is especially important for loop
;;; variables.
;;;
;;; Code:

(define-module (language cps specialize-numbers)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps renumber)
  #:use-module (language cps types)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:export (specialize-numbers))

(define (specialize-f64-unop cps k src op a b)
  (cond
   ((eq? op 'sub/immediate)
    (specialize-f64-unop cps k src 'add/immediate a (- b)))
   (else
    (let ((fop (match op
                 ('add/immediate 'fadd/immediate)
                 ('mul/immediate 'fmul/immediate))))
      (with-cps cps
        (letv f64-a result)
        (letk kbox ($kargs ('result) (result)
                     ($continue k src
                       ($primcall 'f64->scm #f (result)))))
        (letk kop ($kargs ('f64-a) (f64-a)
                    ($continue kbox src
                      ($primcall fop b (f64-a)))))
        (build-term
          ($continue kop src
            ($primcall 'scm->f64 #f (a)))))))))

(define* (specialize-u64-unop cps k src op a b #:key
                               (unbox-a 'scm->u64))
  (let ((uop (match op
               ('add/immediate 'uadd/immediate)
               ('sub/immediate 'usub/immediate)
               ('mul/immediate 'umul/immediate)
               ('rsh/immediate 'ursh/immediate)
               ('lsh/immediate 'ulsh/immediate))))
    (with-cps cps
      (letv u64-a result)
      (letk kbox ($kargs ('result) (result)
                   ($continue k src
                     ($primcall 'u64->scm #f (result)))))
      (letk kop ($kargs ('u64-a) (u64-a)
                  ($continue kbox src
                    ($primcall uop b (u64-a)))))
      (build-term
        ($continue kop src
          ($primcall unbox-a #f (a)))))))

(define (specialize-f64-binop cps k src op a b)
  (let ((fop (match op
               ('add 'fadd)
               ('sub 'fsub)
               ('mul 'fmul)
               ('div 'fdiv))))
    (with-cps cps
      (letv f64-a f64-b result)
      (letk kbox ($kargs ('result) (result)
                   ($continue k src
                     ($primcall 'f64->scm #f (result)))))
      (letk kop ($kargs ('f64-b) (f64-b)
                  ($continue kbox src
                    ($primcall fop #f (f64-a f64-b)))))
      (letk kunbox-b ($kargs ('f64-a) (f64-a)
                       ($continue kop src
                         ($primcall 'scm->f64 #f (b)))))
      (build-term
        ($continue kunbox-b src
          ($primcall 'scm->f64 #f (a)))))))

(define* (specialize-u64-binop cps k src op a b #:key
                               (unbox-a 'scm->u64)
                               (unbox-b 'scm->u64))
  (let ((uop (match op
               ('add 'uadd)
               ('sub 'usub)
               ('mul 'umul)
               ('logand 'ulogand)
               ('logior 'ulogior)
               ('logxor 'ulogxor)
               ('logsub 'ulogsub))))
    (with-cps cps
      (letv u64-a u64-b result)
      (letk kbox ($kargs ('result) (result)
                   ($continue k src
                     ($primcall 'u64->scm #f (result)))))
      (letk kop ($kargs ('u64-b) (u64-b)
                  ($continue kbox src
                    ($primcall uop #f (u64-a u64-b)))))
      (letk kunbox-b ($kargs ('u64-a) (u64-a)
                       ($continue kop src
                         ($primcall unbox-b #f (b)))))
      (build-term
        ($continue kunbox-b src
          ($primcall unbox-a #f (a)))))))

(define* (specialize-u64-shift cps k src op a b #:key
                               (unbox-a 'scm->u64))
  (let ((uop (match op
               ('rsh 'ursh)
               ('lsh 'ulsh))))
    (with-cps cps
      (letv u64-a result)
      (letk kbox ($kargs ('result) (result)
                   ($continue k src
                     ($primcall 'u64->scm #f (result)))))
      (letk kop ($kargs ('u64-a) (u64-a)
                       ($continue kbox src
                         ($primcall uop #f (u64-a b)))))
      (build-term
        ($continue kop src
          ($primcall unbox-a #f (a)))))))

(define (truncate-u64 cps k src scm)
  (with-cps cps
    (letv u64)
    (letk kbox ($kargs ('u64) (u64)
                 ($continue k src
                   ($primcall 'u64->scm #f (u64)))))
    (build-term
      ($continue kbox src
        ($primcall 'scm->u64/truncate #f (scm))))))

(define (specialize-u64-comparison cps kf kt src op a b)
  (let ((op (symbol-append 'u64- op)))
    (with-cps cps
      (letv u64-a u64-b)
      (letk kop ($kargs ('u64-b) (u64-b)
                  ($continue kf src
                    ($branch kt ($primcall op #f (u64-a u64-b))))))
      (letk kunbox-b ($kargs ('u64-a) (u64-a)
                       ($continue kop src
                         ($primcall 'scm->u64 #f (b)))))
      (build-term
        ($continue kunbox-b src
          ($primcall 'scm->u64 #f (a)))))))

(define (specialize-s64-comparison cps kf kt src op a b)
  (let ((op (symbol-append 's64- op)))
    (with-cps cps
      (letv s64-a s64-b)
      (letk kop ($kargs ('s64-b) (s64-b)
                  ($continue kf src
                    ($branch kt ($primcall op #f (s64-a s64-b))))))
      (letk kunbox-b ($kargs ('s64-a) (s64-a)
                       ($continue kop src
                         ($primcall 'scm->s64 #f (b)))))
      (build-term
        ($continue kunbox-b src
          ($primcall 'scm->s64 #f (a)))))))

(define (specialize-s64-scm-comparison cps kf kt src op a-s64 b-scm)
  (let ((s64-op (match op ('= 's64-=) ('< 's64-<))))
    (with-cps cps
      (letv a b sunk)
      (letk kheap ($kargs ('sunk) (sunk)
                    ($continue kf src
                      ($branch kt ($primcall op #f (sunk b-scm))))))
      ;; Re-box the variable.  FIXME: currently we use a specially
      ;; marked s64->scm to avoid CSE from hoisting the allocation
      ;; again.  Instaed we should just use a-s64 directly and implement
      ;; an allocation sinking pass that should handle this..
      (letk kretag ($kargs () ()
                     ($continue kheap src
                       ($primcall 's64->scm/unlikely #f (a)))))
      (letk kb ($kargs ('b) (b)
                 ($continue kf src
                   ($branch kt ($primcall s64-op #f (a b))))))
      (letk kfix ($kargs () ()
                   ($continue kb src
                     ($primcall 'untag-fixnum #f (b-scm)))))
      (letk ka ($kargs ('a) (a)
                 ($continue kretag src
                   ($branch kfix ($primcall 'fixnum? #f (b-scm))))))
      (build-term
        ($continue ka src
          ($primcall 'scm->s64 #f (a-s64)))))))

(define (specialize-scm-s64-comparison cps kf kt src op a-scm b-s64)
  (match op
    ('= (specialize-s64-scm-comparison cps kf kt src op b-s64 a-scm))
    ('<
     (with-cps cps
       (letv a b sunk)
       (letk kheap ($kargs ('sunk) (sunk)
                     ($continue kf src
                       ($branch kt ($primcall '< #f (a-scm sunk))))))
       ;; Re-box the variable.  FIXME: currently we use a specially
       ;; marked s64->scm to avoid CSE from hoisting the allocation
       ;; again.  Instaed we should just use a-s64 directly and implement
       ;; an allocation sinking pass that should handle this..
       (letk kretag ($kargs () ()
                      ($continue kheap src
                        ($primcall 's64->scm/unlikely #f (b)))))
       (letk ka ($kargs ('a) (a)
                  ($continue kf src
                    ($branch kt ($primcall 's64-< #f (a b))))))
       (letk kfix ($kargs () ()
                    ($continue ka src
                      ($primcall 'untag-fixnum #f (a-scm)))))
       (letk kb ($kargs ('b) (b)
                  ($continue kretag src
                    ($branch kfix ($primcall 'fixnum? #f (a-scm))))))
       (build-term
         ($continue kb src
           ($primcall 'scm->s64 #f (b-s64))))))))

(define (specialize-f64-comparison cps kf kt src op a b)
  (let ((op (symbol-append 'f64- op)))
    (with-cps cps
      (letv f64-a f64-b)
      (letk kop ($kargs ('f64-b) (f64-b)
                  ($continue kf src
                    ($branch kt ($primcall op #f (f64-a f64-b))))))
      (letk kunbox-b ($kargs ('f64-a) (f64-a)
                       ($continue kop src
                         ($primcall 'scm->f64 #f (b)))))
      (build-term
        ($continue kunbox-b src
          ($primcall 'scm->f64 #f (a)))))))

(define (sigbits-union x y)
  (and x y (logior x y)))

(define (sigbits-intersect x y)
  (cond
   ((not x) y)
   ((not y) x)
   (else (logand x y))))

(define (sigbits-intersect3 a b c)
  (sigbits-intersect a (sigbits-intersect b c)))

(define (next-power-of-two n)
  (let lp ((out 1))
    (if (< n out)
        out
        (lp (ash out 1)))))

(define (range->sigbits min max)
  (cond
   ((or (< min 0) (> max #xffffFFFFffffFFFF)) #f)
   ((eqv? min max) min)
   (else (1- (next-power-of-two max)))))

(define (inferred-sigbits types label var)
  (call-with-values (lambda () (lookup-pre-type types label var))
    (lambda (type min max)
      (and (type<=? type (logior &exact-integer &u64 &s64))
           (range->sigbits min max)))))

(define significant-bits-handlers (make-hash-table))
(define-syntax-rule (define-significant-bits-handler
                      ((primop label types out def ...) arg ...)
                      body ...)
  (hashq-set! significant-bits-handlers 'primop
              (lambda (label types out param args defs)
                (match args ((arg ...) (match defs ((def ...) body ...)))))))

(define-significant-bits-handler ((logand label types out res) a b)
  (let ((sigbits (sigbits-intersect3 (inferred-sigbits types label a)
                                     (inferred-sigbits types label b)
                                     (intmap-ref out res (lambda (_) 0)))))
    (intmap-add (intmap-add out a sigbits sigbits-union)
                b sigbits sigbits-union)))

(define (significant-bits-handler primop)
  (hashq-ref significant-bits-handlers primop))

(define (compute-significant-bits cps types kfun)
  "Given the locally inferred types @var{types}, compute a map of VAR ->
BITS indicating the significant bits needed for a variable.  BITS may be
#f to indicate all bits, or a non-negative integer indicating a bitmask."
  (let ((preds (invert-graph (compute-successors cps kfun))))
    (let lp ((worklist (intmap-keys preds)) (visited empty-intset)
             (out empty-intmap))
      (match (intset-prev worklist)
        (#f out)
        (label
         (let ((worklist (intset-remove worklist label))
               (visited* (intset-add visited label)))
           (define (continue out*)
             (if (and (eq? out out*) (eq? visited visited*))
                 (lp worklist visited out)
                 (lp (intset-union worklist (intmap-ref preds label))
                     visited* out*)))
           (define (add-def out var)
             (intmap-add out var 0 sigbits-union))
           (define (add-defs out vars)
             (match vars
               (() out)
               ((var . vars) (add-defs (add-def out var) vars))))
           (define (add-unknown-use out var)
             (intmap-add out var (inferred-sigbits types label var)
                         sigbits-union))
           (define (add-unknown-uses out vars)
             (match vars
               (() out)
               ((var . vars)
                (add-unknown-uses (add-unknown-use out var) vars))))
           (continue
            (match (intmap-ref cps label)
              (($ $kfun src meta self)
               (add-def out self))
              (($ $kargs names vars ($ $continue k src exp))
               (let ((out (add-defs out vars)))
                 (match exp
                   ((or ($ $const) ($ $prim) ($ $fun) ($ $closure) ($ $rec))
                    ;; No uses, so no info added to sigbits.
                    out)
                   (($ $values args)
                    (match (intmap-ref cps k)
                      (($ $kargs _ vars)
                       (if (intset-ref visited k)
                           (fold (lambda (arg var out)
                                   (intmap-add out arg (intmap-ref out var)
                                               sigbits-union))
                                 out args vars)
                           out))
                      (($ $ktail)
                       (add-unknown-uses out args))))
                   (($ $call proc args)
                    (add-unknown-use (add-unknown-uses out args) proc))
                   (($ $callk label proc args)
                    (add-unknown-use (add-unknown-uses out args) proc))
                   (($ $branch kt ($ $primcall name param args))
                    (add-unknown-uses out args))
                   (($ $primcall name param args)
                    (let ((h (significant-bits-handler name)))
                      (if h
                          (match (intmap-ref cps k)
                            (($ $kargs _ defs)
                             (h label types out param args defs)))
                          (add-unknown-uses out args))))
                   (($ $prompt escape? tag handler)
                    (add-unknown-use out tag)))))
              (_ out)))))))))

(define (specialize-operations cps)
  (define (visit-cont label cont cps types sigbits)
    (define (operand-in-range? var &type &min &max)
      (call-with-values (lambda ()
                          (lookup-pre-type types label var))
        (lambda (type min max)
          (and (type<=? type &type) (<= &min min max &max)))))
    (define (u64-operand? var)
      (operand-in-range? var &exact-integer 0 #xffffffffffffffff))
    (define (s64-operand? var)
      (operand-in-range? var &exact-integer
                         (- #x8000000000000000) #x7fffffffffffffff))
    (define (all-u64-bits-set? var)
      (operand-in-range? var &exact-integer
                         #xffffffffffffffff
                         #xffffffffffffffff))
    (define (only-u64-bits-used? var)
      (let ((bits (intmap-ref sigbits var)))
        (and bits (= bits (logand bits #xffffFFFFffffFFFF)))))
    (define (u64-result? result)
      (or (only-u64-bits-used? result)
          (call-with-values
              (lambda ()
                (lookup-post-type types label result 0))
            (lambda (type min max)
              (and (type<=? type &exact-integer)
                   (<= 0 min max #xffffffffffffffff))))))
    (define (f64-operands? vara varb)
      (let-values (((typea mina maxa) (lookup-pre-type types label vara))
                   ((typeb minb maxb) (lookup-pre-type types label varb)))
        (and (zero? (logand (logior typea typeb) (lognot &real)))
             (or (eqv? typea &flonum)
                 (eqv? typeb &flonum)))))
    (match cont
      (($ $kfun)
       (let ((types (infer-types cps label)))
         (values cps types (compute-significant-bits cps types label))))
      (($ $kargs names vars
          ($ $continue k src
             ($ $primcall (and op (or 'add 'sub 'mul 'div)) #f (a b))))
       (match (intmap-ref cps k)
         (($ $kargs (_) (result))
          (call-with-values (lambda ()
                              (lookup-post-type types label result 0))
            (lambda (type min max)
              (values
               (cond
                ((eqv? type &flonum)
                 (with-cps cps
                   (let$ body (specialize-f64-binop k src op a b))
                   (setk label ($kargs names vars ,body))))
                ((and (type<=? type &exact-integer)
                      (or (<= 0 min max #xffffffffffffffff)
                          (only-u64-bits-used? result))
                      (u64-operand? a) (u64-operand? b)
                      (not (eq? op 'div)))
                 (with-cps cps
                   (let$ body (specialize-u64-binop k src op a b))
                   (setk label ($kargs names vars ,body))))
                (else
                 cps))
               types
               sigbits))))))
      (($ $kargs names vars
          ($ $continue k src
             ($ $primcall (and op
                               (or 'add/immediate 'sub/immediate
                                   'mul/immediate
                                   'rsh/immediate 'lsh/immediate))
                b (a))))
       (match (intmap-ref cps k)
         (($ $kargs (_) (result))
          (call-with-values (lambda ()
                              (lookup-post-type types label result 0))
            (lambda (type min max)
              (values
               (cond
                ((eqv? type &flonum)
                 (with-cps cps
                   (let$ body (specialize-f64-unop k src op a b))
                   (setk label ($kargs names vars ,body))))
                ((and (type<=? type &exact-integer)
                      (or (<= 0 min max #xffffffffffffffff)
                          (only-u64-bits-used? result))
                      (u64-operand? a) (<= 0 b #xffffFFFFffffFFFF))
                 (with-cps cps
                   (let$ body (specialize-u64-unop k src op a b))
                   (setk label ($kargs names vars ,body))))
                (else
                 cps))
               types
               sigbits))))))
      (($ $kargs names vars
          ($ $continue k src ($ $primcall 'ash #f (a b))))
       (match (intmap-ref cps k)
         (($ $kargs (_) (result))
          (call-with-values (lambda ()
                              (lookup-pre-type types label b))
            (lambda (b-type b-min b-max)
              (values
               (cond
                ((or (not (u64-result? result))
                     (not (u64-operand? a))
                     (not (type<=? b-type &exact-integer))
                     (< b-min 0 b-max)
                     (<= b-min -64)
                     (<= 64 b-max))
                 cps)
                ((= b-min b-max)
                 (if (< b-min 0)
                     (with-cps cps
                       (let$ body
                             (specialize-u64-unop k src
                                                  'rsh/immediate a (- b-min)))
                       (setk label ($kargs names vars ,body)))
                     (with-cps cps
                       (let$ body
                             (specialize-u64-unop k src
                                                  'lsh/immediate a b-min))
                       (setk label ($kargs names vars ,body)))))
                ((< b-min 0)
                 (with-cps cps
                   (let$ body
                         (with-cps-constants ((zero 0))
                           (letv count ucount)
                           (let$ body
                                 (specialize-u64-shift k src 'rsh a ucount))
                           (letk kucount ($kargs ('ucount) (ucount) ,body))
                           (letk kcount ($kargs ('count) (count)
                                          ($continue kucount src
                                            ($primcall 'scm->u64 #f (count)))))
                           (build-term ($continue kcount src
                                         ($primcall 'sub #f (zero b))))))
                   (setk label ($kargs names vars ,body))))
                (else
                 (with-cps cps
                   (letv ucount)
                   (let$ body (specialize-u64-shift k src 'lsh a ucount))
                   (letk kunbox ($kargs ('ucount) (ucount) ,body))
                   (setk label ($kargs names vars
                                 ($continue kunbox src
                                   ($primcall 'scm->u64 #f (b))))))))
               types
               sigbits))))))
      (($ $kargs names vars
          ($ $continue k src
             ($ $primcall (and op (or 'logand 'logior 'logsub 'logxor)) #f (a b))))
       (match (intmap-ref cps k)
         (($ $kargs (_) (result))
          (values
           (cond
            ((u64-result? result)
             ;; Given that we know the result can be unboxed to a u64,
             ;; any out-of-range bits won't affect the result and so we
             ;; can unconditionally project the operands onto u64.
             (cond
              ((and (eq? op 'logand) (all-u64-bits-set? a))
               (with-cps cps
                 (let$ body (truncate-u64 k src b))
                 (setk label ($kargs names vars ,body))))
              ((and (eq? op 'logand) (all-u64-bits-set? b))
               (with-cps cps
                 (let$ body (truncate-u64 k src a))
                 (setk label ($kargs names vars ,body))))
              (else
               (with-cps cps
                 (let$ body (specialize-u64-binop k src op a b
                                                  #:unbox-a
                                                  'scm->u64/truncate
                                                  #:unbox-b
                                                  'scm->u64/truncate))
                 (setk label ($kargs names vars ,body))))))
            (else cps))
           types sigbits))))
      (($ $kargs names vars
          ($ $continue k src
             ($ $branch kt ($ $primcall (and op (or '< '=)) #f (a b)))))
       (values
        (cond
         ((f64-operands? a b)
          (with-cps cps
            (let$ body (specialize-f64-comparison k kt src op a b))
            (setk label ($kargs names vars ,body))))
         ((and (u64-operand? a) (u64-operand? b))
          (with-cps cps
            (let$ body (specialize-u64-comparison k kt src op a b))
            (setk label ($kargs names vars ,body))))
         ((s64-operand? a)
          (let ((specialize (if (s64-operand? b)
                                specialize-s64-comparison
                                specialize-s64-scm-comparison)))
            (with-cps cps
              (let$ body (specialize k kt src op a b))
              (setk label ($kargs names vars ,body)))))
         ((s64-operand? b)
          (with-cps cps
            (let$ body (specialize-scm-s64-comparison k kt src op a b))
            (setk label ($kargs names vars ,body))))
         (else cps))
        types
        sigbits))
      (_ (values cps types sigbits))))

  (values (intmap-fold visit-cont cps cps #f #f)))

;; Compute a map from VAR -> LABEL, where LABEL indicates the cont that
;; binds VAR.
(define (compute-defs conts labels)
  (intset-fold
   (lambda (label defs)
     (match (intmap-ref conts label)
       (($ $kfun src meta self tail clause)
        (intmap-add defs self label))
       (($ $kargs names vars)
        (fold1 (lambda (var defs)
                 (intmap-add defs var label))
               vars defs))
       (_ defs)))
   labels empty-intmap))

;; Compute vars whose definitions are all unboxable and whose uses
;; include an unbox operation.
(define (compute-specializable-vars cps body preds defs
                                    exp-result-unboxable?
                                    unbox-ops)
  ;; Compute a map of VAR->LABEL... indicating the set of labels that
  ;; define VAR with unboxable values, given the set of vars
  ;; UNBOXABLE-VARS which is known already to be unboxable.
  (define (collect-unboxable-def-labels unboxable-vars)
    (define (add-unboxable-def unboxable-defs var label)
      (intmap-add unboxable-defs var (intset label) intset-union))
    (intset-fold (lambda (label unboxable-defs)
                   (match (intmap-ref cps label)
                     (($ $kargs _ _ ($ $continue k _ exp))
                      (match exp
                        ((? exp-result-unboxable?)
                         (match (intmap-ref cps k)
                           (($ $kargs (_) (def))
                            (add-unboxable-def unboxable-defs def label))))
                        (($ $values vars)
                         (match (intmap-ref cps k)
                           (($ $kargs _ defs)
                            (fold
                             (lambda (var def unboxable-defs)
                               (if (intset-ref unboxable-vars var)
                                   (add-unboxable-def unboxable-defs def label)
                                   unboxable-defs))
                             unboxable-defs vars defs))
                           ;; Could be $ktail for $values.
                           (_ unboxable-defs)))
                        (_ unboxable-defs)))
                     (_ unboxable-defs)))
                 body empty-intmap))

  ;; Compute the set of vars which are always unboxable.
  (define (compute-unboxable-defs)
    (fixpoint
     (lambda (unboxable-vars)
       (intmap-fold
        (lambda (def unboxable-pred-labels unboxable-vars)
          (if (and (not (intset-ref unboxable-vars def))
                   ;; Are all defining expressions unboxable?
                   (and-map (lambda (pred)
                              (intset-ref unboxable-pred-labels pred))
                            (intmap-ref preds (intmap-ref defs def))))
              (intset-add unboxable-vars def)
              unboxable-vars))
        (collect-unboxable-def-labels unboxable-vars)
        unboxable-vars))
     empty-intset))

  ;; Compute the set of vars that may ever be unboxed.
  (define (compute-unbox-uses unboxable-defs)
    (intset-fold
     (lambda (label unbox-uses)
       (match (intmap-ref cps label)
         (($ $kargs _ _ ($ $continue k _ exp))
          (match exp
            (($ $primcall (? (lambda (op) (memq op unbox-ops))) #f (var))
             (intset-add unbox-uses var))
            (($ $values vars)
             (match (intmap-ref cps k)
               (($ $kargs _ defs)
                (fold (lambda (var def unbox-uses)
                        (if (intset-ref unboxable-defs def)
                            (intset-add unbox-uses var)
                            unbox-uses))
                      unbox-uses vars defs))
               (($ $ktail)
                ;; Assume return is rare and that any unboxable def can
                ;; be reboxed when leaving the procedure.
                (fold (lambda (var unbox-uses)
                        (intset-add unbox-uses var))
                      unbox-uses vars))))
            (_ unbox-uses)))
         (_ unbox-uses)))
     body empty-intset))

  (let ((unboxable-defs (compute-unboxable-defs)))
    (intset-intersect unboxable-defs (compute-unbox-uses unboxable-defs))))

;; Compute vars whose definitions are all inexact reals and whose uses
;; include an unbox operation.
(define (compute-specializable-f64-vars cps body preds defs)
  ;; Can the result of EXP definitely be unboxed as an f64?
  (define (exp-result-f64? exp)
    (match exp
      ((or ($ $primcall 'f64->scm #f (_))
           ($ $const (and (? number?) (? inexact?) (? real?))))
       #t)
      (_ #f)))
  (compute-specializable-vars cps body preds defs exp-result-f64? '(scm->f64)))

;; Compute vars whose definitions are all exact integers in the u64
;; range and whose uses include an unbox operation.
(define (compute-specializable-u64-vars cps body preds defs)
  ;; Can the result of EXP definitely be unboxed as a u64?
  (define (exp-result-u64? exp)
    (match exp
      ((or ($ $primcall 'u64->scm #f (_))
           ($ $primcall 'u64->scm/unlikely #f (_))
           ($ $const (and (? number?) (? exact-integer?)
                          (? (lambda (n) (<= 0 n #xffffffffffffffff))))))
       #t)
      (_ #f)))

  (compute-specializable-vars cps body preds defs exp-result-u64?
                              '(scm->u64 'scm->u64/truncate)))

(define (compute-phi-vars cps preds)
  (intmap-fold (lambda (label preds phis)
                 (match preds
                   (() phis)
                   ((_) phis)
                   (_
                    (match (intmap-ref cps label)
                      (($ $kargs names vars)
                       (fold1 (lambda (var phis)
                                (intset-add phis var))
                              vars phis))
                      (_ phis)))))
               preds empty-intset))

;; Compute the set of variables which have more than one definition,
;; whose definitions are always f64-valued or u64-valued, and which have
;; at least one use that is an unbox operation.
(define (compute-specializable-phis cps body preds defs)
  (let ((f64-vars (compute-specializable-f64-vars cps body preds defs))
        (u64-vars (compute-specializable-u64-vars cps body preds defs))
        (phi-vars (compute-phi-vars cps preds)))
    (unless (eq? empty-intset (intset-intersect f64-vars u64-vars))
      (error "expected f64 and u64 vars to be disjoint sets"))
    (intset-fold (lambda (var out) (intmap-add out var 'u64))
                 (intset-intersect u64-vars phi-vars)
                 (intset-fold (lambda (var out) (intmap-add out var 'f64))
                              (intset-intersect f64-vars phi-vars)
                              empty-intmap))))

;; Each definition of an f64/u64 variable should unbox that variable.
;; The cont that binds the variable should re-box it under its original
;; name, and rely on CSE to remove the boxing as appropriate.
(define (apply-specialization cps kfun body preds defs phis)
  (define (compute-unbox-labels)
    (intmap-fold (lambda (phi kind labels)
                   (fold1 (lambda (pred labels)
                            (intset-add labels pred))
                          (intmap-ref preds (intmap-ref defs phi))
                          labels))
                 phis empty-intset))
  (define (unbox-op var)
    (match (intmap-ref phis var)
      ('f64 'scm->f64)
      ('u64 'scm->u64)))
  (define (box-op var)
    (match (intmap-ref phis var)
      ('f64 'f64->scm)
      ('u64 'u64->scm)))
  (define (unbox-operands)
    (define (unbox-arg cps arg def-var have-arg)
      (if (intmap-ref phis def-var (lambda (_) #f))
          (with-cps cps
            (letv unboxed)
            (let$ body (have-arg unboxed))
            (letk kunboxed ($kargs ('unboxed) (unboxed) ,body))
            (build-term
              ($continue kunboxed #f ($primcall (unbox-op def-var) #f (arg)))))
          (have-arg cps arg)))
    (define (unbox-args cps args def-vars have-args)
      (match args
        (() (have-args cps '()))
        ((arg . args)
         (match def-vars
           ((def-var . def-vars)
            (unbox-arg cps arg def-var
                       (lambda (cps arg)
                         (unbox-args cps args def-vars
                                     (lambda (cps args)
                                       (have-args cps (cons arg args)))))))))))
    (intset-fold
     (lambda (label cps)
       (match (intmap-ref cps label)
         (($ $kargs names vars ($ $continue k src exp))
          (match (intmap-ref cps k)
            (($ $kargs _ defs)
             (match exp
               ;; For expressions that define a single value, we know we need
               ;; to unbox that value.  For $values though we might have to
               ;; unbox just a subset of values.
               (($ $values args)
                (with-cps cps
                  (let$ term (unbox-args
                              args defs
                              (lambda (cps args)
                                (with-cps cps
                                  (build-term
                                    ($continue k src ($values args)))))))
                  (setk label ($kargs names vars ,term))))
               (_
                (match defs
                  ((def)
                   (with-cps cps
                     (letv boxed)
                     (letk kunbox ($kargs ('boxed) (boxed)
                                    ($continue k src
                                      ($primcall (unbox-op def) #f (boxed)))))
                     (setk label ($kargs names vars
                                   ($continue kunbox src ,exp)))))))))))))
     (compute-unbox-labels)
     cps))
  (define (compute-box-labels)
    (intmap-fold (lambda (phi kind labels)
                   (intset-add labels (intmap-ref defs phi)))
                 phis empty-intset))
  (define (box-results cps)
    (intset-fold
     (lambda (label cps)
       (match (intmap-ref cps label)
         (($ $kargs names vars term)
          (let* ((boxed (fold1 (lambda (var boxed)
                                 (if (intmap-ref phis var (lambda (_) #f))
                                     (intmap-add boxed var (fresh-var))
                                     boxed))
                               vars empty-intmap))
                 (bound-vars (map (lambda (var)
                                    (intmap-ref boxed var (lambda (var) var)))
                                  vars)))
            (define (box-var cps name var done)
              (let ((unboxed (intmap-ref boxed var (lambda (_) #f))))
                (if unboxed
                    (with-cps cps
                      (let$ term (done))
                      (letk kboxed ($kargs (name) (var) ,term))
                      (build-term
                        ($continue kboxed #f
                          ($primcall (box-op var) #f (unboxed)))))
                    (done cps))))
            (define (box-vars cps names vars done)
              (match vars
                (() (done cps))
                ((var . vars)
                 (match names
                   ((name . names)
                    (box-var cps name var
                             (lambda (cps)
                               (box-vars cps names vars done))))))))
            (with-cps cps
              (let$ box-term (box-vars names vars
                                       (lambda (cps)
                                         (with-cps cps term))))
              (setk label ($kargs names bound-vars ,box-term)))))))
     (compute-box-labels)
     cps))
  (box-results (unbox-operands)))

(define (specialize-phis cps)
  (intmap-fold
   (lambda (kfun body cps)
     (let* ((preds (compute-predecessors cps kfun #:labels body))
            (defs (compute-defs cps body))
            (phis (compute-specializable-phis cps body preds defs)))
       (if (eq? phis empty-intmap)
           cps
           (apply-specialization cps kfun body preds defs phis))))
   (compute-reachable-functions cps)
   cps))

(define (specialize-numbers cps)
  ;; Type inference wants a renumbered graph; OK.
  (let ((cps (renumber cps)))
    (with-fresh-name-state cps
      (specialize-phis (specialize-operations cps)))))
