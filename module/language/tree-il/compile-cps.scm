;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015, 2017, 2018 Free Software Foundation, Inc.

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
;;; This pass converts Tree-IL to the continuation-passing style (CPS)
;;; language.
;;;
;;; CPS is a lower-level representation than Tree-IL.  Converting to
;;; CPS, beyond adding names for all control points and all values,
;;; simplifies expressions in the following ways, among others:
;;;
;;;   * Fixing the order of evaluation.
;;;
;;;   * Converting assigned variables to boxed variables.
;;;
;;;   * Requiring that Scheme's <letrec> has already been lowered to
;;;     <fix>.
;;;
;;;   * Inlining default-value initializers into lambda-case
;;;     expressions.
;;;
;;;   * Inlining prompt bodies.
;;;
;;;   * Turning toplevel and module references into primcalls.  This
;;;     involves explicitly modelling the "scope" of toplevel lookups
;;;     (indicating the module with respect to which toplevel bindings
;;;     are resolved).
;;;
;;; The utility of CPS is that it gives a name to everything: every
;;; intermediate value, and every control point (continuation).  As such
;;; it is more verbose than Tree-IL, but at the same time more simple as
;;; the number of concepts is reduced.
;;;
;;; Code:

(define-module (language tree-il compile-cps)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold filter-map))
  #:use-module (srfi srfi-26)
  #:use-module ((system foreign) #:select (make-pointer pointer->scm))
  #:use-module (system base target)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language tree-il cps-primitives)
  #:use-module (language tree-il analyze)
  #:use-module (language tree-il optimize)
  #:use-module (language tree-il)
  #:use-module (language cps intmap)
  #:export (compile-cps))

(define (convert-primcall/default cps k src op param . args)
  (with-cps cps
    (build-term
      ($continue k src ($primcall op param args)))))

(define (convert-indexed-getter cps k src op param obj idx)
  (with-cps cps
    (letv idx')
    (letk k' ($kargs ('idx) (idx')
               ($continue k src ($primcall op param (obj idx')))))
    (build-term ($continue k' src ($primcall 'scm->u64 #f (idx))))))

(define (convert-indexed-setter cps k src op param obj idx val)
  (with-cps cps
    (letv idx')
    (letk k' ($kargs ('idx) (idx')
               ($continue k src ($primcall op param (obj idx' val)))))
    (build-term ($continue k' src ($primcall 'scm->u64 #f (idx))))))

(define (convert-indexed-getter/tag cps k src op param obj idx tag-result)
  (with-cps cps
    (letv res')
    (letk k' ($kargs ('res) (res')
               ($continue k src ($primcall tag-result #f (res')))))
    ($ (convert-indexed-getter k' src op param obj idx))))

(define (convert-indexed-setter/untag cps k src op param obj idx val untag-val)
  (with-cps cps
    (letv val')
    (let$ body (convert-indexed-setter k src op param obj idx val'))
    (letk k' ($kargs ('val) (val') ,body))
    (build-term ($continue k' src ($primcall untag-val #f (val))))))

(define convert-scm-u64->scm-primcall convert-indexed-getter)
(define convert-scm-u64-scm-primcall convert-indexed-setter)

(define (convert-u64-scm->scm-primcall cps k src op param len init)
  (with-cps cps
    (letv len')
    (letk k' ($kargs ('len) (len')
               ($continue k src ($primcall op param (len' init)))))
    (build-term ($continue k' src ($primcall 'scm->u64 #f (len))))))

(define (convert-scm-u64->f64-primcall cps k src op param obj idx)
  (convert-indexed-getter/tag cps k src op param obj idx 'f64->scm))
(define (convert-scm-u64-f64-primcall cps k src op param obj idx val)
  (convert-indexed-setter/untag cps k src op param obj idx val 'scm->f64))

(define (convert-scm-u64->u64-primcall cps k src op param obj idx)
  (convert-indexed-getter/tag cps k src op param obj idx 'u64->scm))
(define (convert-scm-u64-u64-primcall cps k src op param obj idx val)
  (convert-indexed-setter/untag cps k src op param obj idx val 'scm->u64))

(define (convert-scm-u64->s64-primcall cps k src op param obj idx)
  (convert-indexed-getter/tag cps k src op param obj idx 's64->scm))
(define (convert-scm-u64-s64-primcall cps k src op param obj idx val)
  (convert-indexed-setter/untag cps k src op param obj idx val 'scm->s64))

(define (convert-*->u64-primcall cps k src op param . args)
  (with-cps cps
    (letv res')
    (letk k' ($kargs ('res) (res')
               ($continue k src ($primcall 'u64->scm #f (res')))))
    (build-term ($continue k' src ($primcall op param args)))))
(define convert-scm->u64-primcall convert-*->u64-primcall)
(define (convert-u64->scm-primcall cps k src op param arg)
  (with-cps cps
    (letv arg')
    (letk k' ($kargs ('arg) (arg')
               ($continue k src ($primcall op param (arg')))))
    (build-term ($continue k' src ($primcall 'scm->u64 #f (arg))))))

(define *primcall-converters* (make-hash-table))
(define-syntax-rule (define-primcall-converter name proc)
  (hashq-set! *primcall-converters* 'name proc))
(define-syntax define-primcall-converters
  (lambda (x)
    (define (spec->convert spec)
      (string->symbol
       (string-join
        (append '("convert") (map symbol->string spec) '("primcall"))
        "-")))
    (define (compute-converter spec)
      (datum->syntax #'here (spec->convert (syntax->datum spec))))
    (syntax-case x ()
      ((_ (op . spec) ...)
       (with-syntax (((cvt ...) (map compute-converter #'(spec ...))))
         #'(begin (define-primcall-converter op cvt) ...))))))

(define-primcall-converters
  (char->integer scm >u64)
  (integer->char u64 >scm)

  (string-length scm >u64)
  (string-ref scm u64 >scm) (string-set! scm u64 scm)

  (make-vector u64 scm >scm)
  (vector-length scm >u64)
  (vector-ref scm u64 >scm) (vector-set! scm u64 scm)

  (allocate-struct scm u64 >scm)
  (struct-ref scm u64 >scm) (struct-set! scm u64 scm)

  (bv-length scm >u64)
  (bv-f32-ref scm u64 >f64) (bv-f32-set! scm u64 f64)
  (bv-f64-ref scm u64 >f64) (bv-f64-set! scm u64 f64)
  (bv-u8-ref scm u64 >u64)  (bv-u8-set! scm u64 u64)
  (bv-u16-ref scm u64 >u64) (bv-u16-set! scm u64 u64)
  (bv-u32-ref scm u64 >u64) (bv-u32-set! scm u64 u64)
  (bv-u64-ref scm u64 >u64) (bv-u64-set! scm u64 u64)
  (bv-s8-ref  scm u64 >s64) (bv-s8-set!  scm u64 s64)
  (bv-s16-ref scm u64 >s64) (bv-s16-set! scm u64 s64)
  (bv-s32-ref scm u64 >s64) (bv-s32-set! scm u64 s64)
  (bv-s64-ref scm u64 >s64) (bv-s64-set! scm u64 s64)

  (rsh scm u64 >scm)
  (lsh scm u64 >scm))

(define (convert-primcall* cps k src op param args)
  (let ((proc (hashq-ref *primcall-converters* op convert-primcall/default)))
    (apply proc cps k src op param args)))

(define (convert-primcall cps k src op param . args)
  (convert-primcall* cps k src op param args))

;;; Guile's semantics are that a toplevel lambda captures a reference on
;;; the current module, and that all contained lambdas use that module
;;; to resolve toplevel variables.  This parameter tracks whether or not
;;; we are in a toplevel lambda.  If we are in a lambda, the parameter
;;; is bound to a fresh name identifying the module that was current
;;; when the toplevel lambda is defined.
;;;
;;; This is more complicated than it need be.  Ideally we should resolve
;;; all toplevel bindings to bindings from specific modules, unless the
;;; binding is unbound.  This is always valid if the compilation unit
;;; sets the module explicitly, as when compiling a module, but it
;;; doesn't work for files auto-compiled for use with `load'.
;;;
(define current-topbox-scope (make-parameter #f))
(define scope-counter (make-parameter #f))

(define (fresh-scope-id)
  (let ((scope-id (scope-counter)))
    (scope-counter (1+ scope-id))
    scope-id))

(define (toplevel-box cps src name bound? val-proc)
  (define (lookup cps k)
    (match (current-topbox-scope)
      (#f
       (with-cps cps
         ;; FIXME: Resolve should take name as immediate.
         ($ (with-cps-constants ((name name))
              ($ (convert-primcall k src 'resolve (list bound?) name))))))
      (scope
       (with-cps cps
         ($ (convert-primcall k src 'cached-toplevel-box
                              (list scope name bound?)))))))
  (with-cps cps
    (letv box)
    (let$ body (val-proc box))
    (letk kbox ($kargs ('box) (box) ,body))
    ($ (lookup kbox))))

(define (module-box cps src module name public? bound? val-proc)
  (with-cps cps
    (letv box)
    (let$ body (val-proc box))
    (letk kbox ($kargs ('box) (box) ,body))
    ($ (convert-primcall kbox src 'cached-module-box
                         (list module name public? bound?)))))

(define (capture-toplevel-scope cps src scope-id k)
  (with-cps cps
    (letv module)
    (let$ body (convert-primcall k src 'cache-current-module!
                                 (list scope-id) module))
    (letk kmodule ($kargs ('module) (module) ,body))
    ($ (convert-primcall kmodule src 'current-module #f))))

(define (fold-formals proc seed arity gensyms inits)
  (match arity
    (($ $arity req opt rest kw allow-other-keys?)
     (let ()
       (define (fold-req names gensyms seed)
         (match names
           (() (fold-opt opt gensyms inits seed))
           ((name . names)
            (proc name (car gensyms) #f
                  (fold-req names (cdr gensyms) seed)))))
       (define (fold-opt names gensyms inits seed)
         (match names
           (() (fold-rest rest gensyms inits seed))
           ((name . names)
            (proc name (car gensyms) (car inits)
                  (fold-opt names (cdr gensyms) (cdr inits) seed)))))
       (define (fold-rest rest gensyms inits seed)
         (match rest
           (#f (fold-kw kw gensyms inits seed))
           (name (proc name (car gensyms) #f
                       (fold-kw kw (cdr gensyms) inits seed)))))
       (define (fold-kw kw gensyms inits seed)
         (match kw
           (()
            (unless (null? gensyms)
              (error "too many gensyms"))
            (unless (null? inits)
              (error "too many inits"))
            seed)
           (((key name var) . kw)
            ;; Could be that var is not a gensym any more.
            (when (symbol? var)
              (unless (eq? var (car gensyms))
                (error "unexpected keyword arg order")))
            (proc name (car gensyms) (car inits)
                  (fold-kw kw (cdr gensyms) (cdr inits) seed)))))
       (fold-req req gensyms seed)))))

(define (init-default-value cps name sym subst init body)
  (match (hashq-ref subst sym)
    ((orig-var subst-var box?)
     (let ((src (tree-il-src init)))
       (define (maybe-box cps k make-body)
         (if box?
             (with-cps cps
               (letv phi)
               (let$ body (convert-primcall k src 'box #f phi))
               (letk kbox ($kargs (name) (phi) ,body))
               ($ (make-body kbox)))
             (make-body cps k)))
       (with-cps cps
         (letk knext ($kargs (name) (subst-var) ,body))
         ($ (maybe-box
             knext
             (lambda (cps k)
               (with-cps cps
                 (letk kbound ($kargs () () ($continue k src
                                              ($values (orig-var)))))
                 (letv val rest)
                 (letk krest ($kargs (name 'rest) (val rest)
                               ($continue k src ($values (val)))))
                 (letk kreceive ($kreceive (list name) 'rest krest))
                 (let$ init (convert init kreceive subst))
                 (letk kunbound ($kargs () () ,init))
                 (build-term
                   ($branch kbound kunbound src
                     'undefined? #f (orig-var))))))))))))

(define (build-list cps k src vals)
  (match vals
    (()
     (with-cps cps
       (build-term ($continue k src ($const '())))))
    ((v . vals)
     (with-cps cps
       (letv tail)
       (letk ktail ($kargs ('tail) (tail)
                     ($continue k src ($primcall 'cons #f (v tail)))))
       ($ (build-list ktail src vals))))))

;;; The conversion from Tree-IL to CPS essentially wraps every
;;; expression in a $kreceive, which models the Tree-IL semantics that
;;; extra values are simply truncated.  In CPS, this means that the
;;; $kreceive has a rest argument after the required arguments, if any,
;;; and that the rest argument is unused.
;;;
;;; All CPS expressions that can return a variable number of values
;;; (i.e., $call and $abort) must continue to $kreceive, which checks
;;; the return arity and on success passes the parsed values along to a
;;; $kargs.  If the $call or $abort is in tail position they continue to
;;; $ktail instead, and then the values are parsed by the $kreceive of
;;; the non-tail caller.
;;;
;;; Other CPS terms like $values, $const, and the like all have a
;;; specific return arity, and must continue to $kargs instead of
;;; $kreceive or $ktail.  This allows the compiler to reason precisely
;;; about their result values.  To make sure that this is the case,
;;; whenever the CPS conversion would reify one of these terms it needs
;;; to ensure that the continuation actually accepts the return arity of
;;; the primcall.
;;;
;;; Some Tree-IL primcalls residualize CPS primcalls that return zero
;;; values, for example box-set!.  In this case the Tree-IL semantics
;;; are that the result of the expression is the undefined value.  That
;;; is to say, the result of this expression is #t:
;;;
;;;   (let ((x 30)) (eq? (set! x 10) (if #f #f)))
;;;
;;; So in the case that the continuation expects a value but the
;;; primcall produces zero values, we insert the "unspecified" value.
;;;
(define (adapt-arity cps k src nvals)
  (match nvals
    (0
     ;; As mentioned above, in the Tree-IL semantics the primcall
     ;; produces the unspecified value, but in CPS it produces no
     ;; values.  Therefore we plug the unspecified value into the
     ;; continuation.
     (match (intmap-ref cps k)
       (($ $ktail)
        (with-cps cps
          (let$ body (with-cps-constants ((unspecified *unspecified*))
                       (build-term
                         ($continue k src ($values (unspecified))))))
          (letk kvoid ($kargs () () ,body))
          kvoid))
       (($ $kargs ()) (with-cps cps k))
       (($ $kreceive arity kargs)
        (match arity
          (($ $arity () () (not #f) () #f)
           (with-cps cps
             (letk kvoid ($kargs () () ($continue kargs src ($const '()))))
             kvoid))
          (($ $arity (_) () #f () #f)
           (with-cps cps
             (letk kvoid ($kargs () ()
                           ($continue kargs src ($const *unspecified*))))
             kvoid))
          (($ $arity (_) () _ () #f)
           (with-cps cps
             (let$ void (with-cps-constants ((unspecified *unspecified*)
                                             (rest '()))
                          (build-term
                            ($continue kargs src
                              ($values (unspecified rest))))))
             (letk kvoid ($kargs () () ,void))
             kvoid))
          (_
           ;; Arity mismatch.  Serialize a values call.
           (with-cps cps
             (letv values)
             (let$ void (with-cps-constants ((unspecified *unspecified*))
                          (build-term
                            ($continue k src
                              ($call values (unspecified))))))
             (letk kvoid ($kargs ('values) (values) ,void))
             (letk kvalues ($kargs () ()
                             ($continue kvoid src ($prim 'values))))
             kvalues))))))
    (1
     (match (intmap-ref cps k)
       (($ $ktail)
        (with-cps cps
          (letv val)
          (letk kval ($kargs ('val) (val)
                       ($continue k src ($values (val)))))
          kval))
       (($ $kargs (_)) (with-cps cps k))
       (($ $kreceive arity kargs)
        (match arity
          (($ $arity () () (not #f) () #f)
           (with-cps cps
             (letv val)
             (let$ body (with-cps-constants ((nil '()))
                          ($ (convert-primcall kargs src 'cons #f
                                               val nil))))
             (letk kval ($kargs ('val) (val) ,body))
             kval))
          (($ $arity (_) () #f () #f)
           (with-cps cps
             kargs))
          (($ $arity (_) () _ () #f)
           (with-cps cps
             (letv val)
             (let$ body (with-cps-constants ((rest '()))
                          (build-term
                            ($continue kargs src ($values (val rest))))))
             (letk kval ($kargs ('val) (val) ,body))
             kval))
          (_
           ;; Arity mismatch.  Serialize a values call.
           (with-cps cps
             (letv val values)
             (letk kvalues ($kargs ('values) (values)
                             ($continue k src
                               ($call values (val)))))
             (letk kval ($kargs ('val) (val)
                          ($continue kvalues src ($prim 'values))))
             kval))))))))

;; cps exp k-name alist -> cps term
(define (convert cps exp k subst)
  (define (zero-valued? exp)
    (match exp
      ((or ($ <module-set>) ($ <toplevel-set>) ($ <toplevel-define>)
           ($ <lexical-set>))
       #t)
      (($ <let> src names syms vals body) (zero-valued? body))
      ;; Can't use <fix> here as the hack that <fix> uses to convert its
      ;; functions relies on continuation being single-valued.
      ;; (($ <fix> src names syms vals body) (zero-valued? body))
      (($ <let-values> src exp body) (zero-valued? body))
      (($ <seq> src head tail) (zero-valued? tail))
      (($ <primcall> src 'values args) (= (length args) 0))
      (($ <primcall> src name args)
       (match (tree-il-primitive->cps-primitive+nargs+nvalues name)
         (#f #f)
         (#(cps-prim nargs nvalues)
          (and (eqv? nvalues 0)
               (eqv? nargs (length args))))))
      (_ #f)))
  (define (single-valued? exp)
    (match exp
      ((or ($ <void>) ($ <const>) ($ <primitive-ref>) ($ <module-ref>)
           ($ <toplevel-ref>) ($ <lambda>))
       #t)
      (($ <let> src names syms vals body) (single-valued? body))
      (($ <fix> src names syms vals body) (single-valued? body))
      (($ <let-values> src exp body) (single-valued? body))
      (($ <seq> src head tail) (single-valued? tail))
      (($ <primcall> src 'values args) (= (length args) 1))
      (($ <primcall> src name args)
       (match (tree-il-primitive->cps-primitive+nargs+nvalues name)
         (#f #f)
         (#(cps-prim nargs nvalues)
          (and (eqv? nvalues 1)
               (eqv? nargs (length args))))))
      (_ #f)))
  ;; exp (v-name -> term) -> term
  (define (convert-arg cps exp k)
    (match exp
      (($ <lexical-ref> src name sym)
       (match (hashq-ref subst sym)
         ((orig-var box #t)
          (with-cps cps
            (letv unboxed)
            (let$ body (k unboxed))
            (letk kunboxed ($kargs ('unboxed) (unboxed) ,body))
            (build-term ($continue kunboxed src
                          ($primcall 'scm-ref/immediate '(box . 1) (box))))))
         ((orig-var subst-var #f) (k cps subst-var))
         (var (k cps var))))
      ((? single-valued?)
       (with-cps cps
         (letv arg)
         (let$ body (k arg))
         (letk karg ($kargs ('arg) (arg) ,body))
         ($ (convert exp karg subst))))
      (_
       (with-cps cps
         (letv arg rest)
         (let$ body (k arg))
         (letk karg ($kargs ('arg 'rest) (arg rest) ,body))
         (letk kreceive ($kreceive '(arg) 'rest karg))
         ($ (convert exp kreceive subst))))))
  ;; (exp ...) ((v-name ...) -> term) -> term
  (define (convert-args cps exps k)
    (match exps
      (() (k cps '()))
      ((exp . exps)
       (convert-arg cps exp
         (lambda (cps name)
           (convert-args cps exps
             (lambda (cps names)
               (k cps (cons name names)))))))))
  (define (box-bound-var cps name sym body)
    (match (hashq-ref subst sym)
      ((orig-var subst-var #t)
       (with-cps cps
         (letk k ($kargs (name) (subst-var) ,body))
         ($ (convert-primcall k #f 'box #f orig-var))))
      (else
       (with-cps cps body))))
  (define (box-bound-vars cps names syms body)
    (match (vector names syms)
      (#((name . names) (sym . syms))
       (with-cps cps
         (let$ body (box-bound-var name sym body))
         ($ (box-bound-vars names syms body))))
      (#(() ()) (with-cps cps body))))
  (define (bound-var sym)
    (match (hashq-ref subst sym)
      ((var . _) var)
      ((? exact-integer? var) var)))

  (match exp
    (($ <lexical-ref> src name sym)
     (with-cps cps
       (let$ k (adapt-arity k src 1))
       (rewrite-term (hashq-ref subst sym)
         ((orig-var box #t) ($continue k src
                              ($primcall 'scm-ref/immediate '(box . 1) (box))))
         ((orig-var subst-var #f) ($continue k src ($values (subst-var))))
         (var ($continue k src ($values (var)))))))

    (($ <void> src)
     (with-cps cps
       (let$ k (adapt-arity k src 1))
       (build-term ($continue k src ($const *unspecified*)))))

    (($ <const> src exp)
     (with-cps cps
       (let$ k (adapt-arity k src 1))
       (build-term ($continue k src ($const exp)))))

    (($ <primitive-ref> src name)
     (with-cps cps
       (let$ k (adapt-arity k src 1))
       (build-term ($continue k src ($prim name)))))

    (($ <lambda> fun-src meta body)
     (let ()
       (define (convert-clauses cps body ktail)
         (match body
           (#f (values cps #f))
           (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
            (let* ((arity (make-$arity req (or opt '()) rest
                                       (map (match-lambda
                                              ((kw name sym) 
                                               (list kw name (bound-var sym))))
                                            (if kw (cdr kw) '()))
                                       (and kw (car kw))))
                   (names (fold-formals (lambda (name sym init names)
                                          (cons name names))
                                        '()
                                        arity gensyms inits)))
              (define (fold-formals* cps f seed arity gensyms inits)
                (match (fold-formals
                        (lambda (name sym init cps+seed)
                          (match cps+seed
                            ((cps . seed)
                             (call-with-values (lambda ()
                                                 (f cps name sym init seed))
                               (lambda (cps seed) (cons cps seed))))))
                        (cons cps seed) arity gensyms inits)
                  ((cps . seed) (values cps seed))))
              (with-cps cps
                (let$ kalt (convert-clauses alternate ktail))
                (let$ body (convert body ktail subst))
                (let$ body
                      (fold-formals*
                       (lambda (cps name sym init body)
                         (if init
                             (init-default-value cps name sym subst init body)
                             (box-bound-var cps name sym body)))
                       body arity gensyms inits))
                (letk kargs ($kargs names (map bound-var gensyms) ,body))
                (letk kclause ($kclause ,arity kargs kalt))
                kclause)))))
       (if (current-topbox-scope)
           (with-cps cps
             (letv self)
             (letk ktail ($ktail))
             (let$ kclause (convert-clauses body ktail))
             (letk kfun ($kfun fun-src meta self ktail kclause))
             (let$ k (adapt-arity k fun-src 1))
             (build-term ($continue k fun-src ($fun kfun))))
           (let ((scope-id (fresh-scope-id)))
             (with-cps cps
               (let$ body ((lambda (cps)
                             (parameterize ((current-topbox-scope scope-id))
                               (convert cps exp k subst)))))
               (letk kscope ($kargs () () ,body))
               ($ (capture-toplevel-scope fun-src scope-id kscope)))))))

    (($ <module-ref> src mod name public?)
     (module-box
      cps src mod name public? #t
      (lambda (cps box)
        (with-cps cps
          (let$ k (adapt-arity k src 1))
          (build-term ($continue k src
                        ($primcall 'scm-ref/immediate '(box . 1) (box))))))))

    (($ <module-set> src mod name public? exp)
     (convert-arg cps exp
       (lambda (cps val)
         (module-box
          cps src mod name public? #t
          (lambda (cps box)
            (with-cps cps
              (let$ k (adapt-arity k src 0))
              (build-term
                ($continue k src
                  ($primcall 'scm-set!/immediate '(box . 1) (box val))))))))))

    (($ <toplevel-ref> src name)
     (toplevel-box
      cps src name #t
      (lambda (cps box)
        (with-cps cps
          (let$ k (adapt-arity k src 1))
          (build-term
            ($continue k src
              ($primcall 'scm-ref/immediate '(box . 1) (box))))))))

    (($ <toplevel-set> src name exp)
     (convert-arg cps exp
       (lambda (cps val)
         (toplevel-box
          cps src name #f
          (lambda (cps box)
            (with-cps cps
              (let$ k (adapt-arity k src 0))
              (build-term
                ($continue k src
                  ($primcall 'scm-set!/immediate '(box . 1) (box val))))))))))

    (($ <toplevel-define> src name exp)
     (convert-arg cps exp
       (lambda (cps val)
         (with-cps cps
           (let$ k (adapt-arity k src 0))
           (letv box)
           (letk kset ($kargs ('box) (box)
                        ($continue k src
                          ($primcall 'scm-set!/immediate '(box . 1) (box val)))))
           ($ (with-cps-constants ((name name))
                (build-term
                  ($continue kset src ($primcall 'define! #f (name))))))))))

    (($ <call> src proc args)
     (convert-args cps (cons proc args)
       (match-lambda*
         ((cps (proc . args))
          (with-cps cps
            (build-term ($continue k src ($call proc args))))))))

    (($ <primcall> src name args)
     (cond
      ((eq? name 'throw)
       (let ()
         (define (fallback)
           (convert-args cps args
             (lambda (cps args)
               (match args
                 ((key . args)
                  (with-cps cps
                    (letv arglist)
                    (letk kargs ($kargs ('arglist) (arglist)
                                  ($throw src 'throw #f (key arglist))))
                    ($ (build-list kargs src args))))))))
         (define (specialize op param . args)
           (convert-args cps args
             (lambda (cps args)
               (with-cps cps
                 (build-term
                   ($throw src op param args))))))
         (match args
           ((($ <const> _ key) ($ <const> _ subr) ($ <const> _ msg) args data)
            ;; Specialize `throw' invocations corresponding to common
            ;; "error" invocations.
            (let ()
              (match (vector args data)
                (#(($ <primcall> _ 'cons (x ($ <const> _ ())))
                   ($ <primcall> _ 'cons (x ($ <const> _ ()))))
                 (specialize 'throw/value+data `#(,key ,subr ,msg) x))
                (#(($ <primcall> _ 'cons (x ($ <const> _ ()))) ($ <const> _ #f))
                 (specialize 'throw/value `#(,key ,subr ,msg) x))
                (_ (fallback)))))
           (_ (fallback)))))
      ((eq? name 'values)
       (convert-args cps args
         (lambda (cps args)
           (match (intmap-ref cps k)
             (($ $ktail)
              (with-cps cps
                (build-term
                  ($continue k src ($values args)))))
             (($ $kargs names)
              ;; Can happen if continuation already saw we produced the
              ;; right number of values.
              (with-cps cps
                (build-term
                  ($continue k src ($values args)))))
             (($ $kreceive ($ $arity req () rest () #f) kargs)
              (cond
               ((and (not rest) (= (length args) (length req)))
                (with-cps cps
                  (build-term
                    ($continue kargs src ($values args)))))
               ((and rest (>= (length args) (length req)))
                (with-cps cps
                  (letv rest)
                  (letk krest ($kargs ('rest) (rest)
                                ($continue kargs src
                                  ($values ,(append (list-head args (length req))
                                                    (list rest))))))
                  ($ (build-list krest src (list-tail args (length req))))))
               (else
                ;; Number of values mismatch; reify a values call.
                (with-cps cps
                  (letv val values)
                  (letk kvalues ($kargs ('values) (values)
                                  ($continue k src ($call values args))))
                  (build-term ($continue kvalues src ($prim 'values)))))))))))
      ((tree-il-primitive->cps-primitive+nargs+nvalues name)
       =>
       (match-lambda
        (#(cps-prim nargs nvalues)
         (define (cvt cps k src op args)
           (define (default)
             (convert-args cps args
               (lambda (cps args)
                 (with-cps cps
                   ($ (convert-primcall* k src op #f args))))))
           (define-syntax-rule (specialize-case (pat (op c (arg ...))) ...
                                                (_ def))
             (match (cons cps-prim args)
               (pat
                (convert-args cps (list arg ...)
                  (lambda (cps args)
                    (with-cps cps
                      ($ (convert-primcall* k src 'op c args))))))
               ...
               (_ def)))
           (define (uint? val) (and (exact-integer? val) (<= 0 val)))
           (define (negint? val) (and (exact-integer? val) (< val 0)))
           ;; FIXME: Add case for mul
           (specialize-case
            (('make-vector ($ <const> _ (? uint? n)) init)
             (make-vector/immediate n (init)))
            (('vector-ref v ($ <const> _ (? uint? n)))
             (vector-ref/immediate n (v)))
            (('vector-set! v ($ <const> _ (? uint? n)) x)
             (vector-set!/immediate n (v x)))
            (('allocate-struct v ($ <const> _ (? uint? n)))
             (allocate-struct/immediate n (v)))
            (('struct-ref s ($ <const> _ (? uint? n)))
             (struct-ref/immediate n (s)))
            (('struct-set! s ($ <const> _ (? uint? n)) x)
             (struct-set!/immediate n (s x)))
            (('add x ($ <const> _ (? number? y)))
             (add/immediate y (x)))
            (('add ($ <const> _ (? number? y)) x)
             (add/immediate y (x)))
            (('sub x ($ <const> _ (? number? y)))
             (sub/immediate y (x)))
            (('lsh x ($ <const> _ (? uint? y)))
             (lsh/immediate y (x)))
            (('rsh x ($ <const> _ (? uint? y)))
             (rsh/immediate y (x)))
            (_
             (default))))
         ;; Tree-IL primcalls are sloppy, in that it could be that
         ;; they are called with too many or too few arguments.  In
         ;; CPS we are more strict and only residualize a $primcall
         ;; if the argument count matches.
         (if (= nargs (length args))
             (with-cps cps
               (let$ k (adapt-arity k src nvalues))
               ($ (cvt k src cps-prim args)))
             (convert-args cps args
               (lambda (cps args)
                 (with-cps cps
                   (letv prim)
                   (letk kprim ($kargs ('prim) (prim)
                                 ($continue k src ($call prim args))))
                   (build-term ($continue kprim src ($prim name))))))))))
      (else
       ;; We have something that's a primcall for Tree-IL but not for
       ;; CPS; compile as a call.
       (convert-args cps args
         (lambda (cps args)
           (with-cps cps
             (letv prim)
             (letk kprim ($kargs ('prim) (prim)
                           ($continue k src ($call prim args))))
             (build-term ($continue kprim src ($prim name)))))))))

    ;; Prompts with inline handlers.
    (($ <prompt> src escape-only? tag body
        ($ <lambda> hsrc hmeta
           ($ <lambda-case> _ hreq #f hrest #f () hsyms hbody #f)))
     ;; Handler:
     ;;   khargs: check args returned to handler, -> khbody
     ;;   khbody: the handler, -> k
     ;;
     ;; Post-body:
     ;;   krest: collect return vals from body to list, -> kpop
     ;;   kpop: pop the prompt, -> kprim
     ;;   kprim: load the values primitive, -> kret
     ;;   kret: (apply values rvals), -> k
     ;;
     ;; Escape prompts evaluate the body with the continuation of krest.
     ;; Otherwise we do a no-inline call to body, continuing to krest.
     (convert-arg cps tag
       (lambda (cps tag)
         (let ((hnames (append hreq (if hrest (list hrest) '())))
               (bound-vars (map bound-var hsyms)))
           (define (convert-body cps khargs krest)
             (if escape-only?
                 (with-cps cps
                   (let$ body (convert body krest subst))
                   (letk kbody ($kargs () () ,body))
                   (build-term ($prompt kbody khargs src #t tag)))
                 (convert-arg cps body
                   (lambda (cps thunk)
                     (with-cps cps
                       (letk kbody ($kargs () ()
                                     ($continue krest (tree-il-src body)
                                       ($primcall 'call-thunk/no-inline #f
                                                  (thunk)))))
                       (build-term ($prompt kbody khargs (tree-il-src body)
                                     #f tag)))))))
           (with-cps cps
             (letv prim vals apply)
             (let$ hbody (convert hbody k subst))
             (let$ hbody (box-bound-vars hnames hsyms hbody))
             (letk khbody ($kargs hnames bound-vars ,hbody))
             (letk khargs ($kreceive hreq hrest khbody))
             (letk kapp ($kargs ('apply) (apply)
                          ($continue k src ($call apply (prim vals)))))
             (letk kprim ($kargs ('prim) (prim)
                           ($continue kapp src ($prim 'apply))))
             (letk kret ($kargs () ()
                          ($continue kprim src ($prim 'values))))
             (letk kpop ($kargs ('rest) (vals)
                          ($continue kret src ($primcall 'unwind #f ()))))
             ;; FIXME: Attach hsrc to $kreceive.
             (letk krest ($kreceive '() 'rest kpop))
             ($ (convert-body khargs krest)))))))

    (($ <abort> src tag args ($ <const> _ ()))
     (convert-args cps (cons tag args)
       (lambda (cps args*)
         (with-cps cps
           (letv abort)
           (letk kabort ($kargs ('abort) (abort)
                          ($continue k src ($call abort args*))))
           (build-term
             ($continue kabort src ($prim 'abort-to-prompt)))))))

    (($ <abort> src tag args tail)
     (convert-args cps
         (append (list (make-primitive-ref #f 'apply)
                       (make-primitive-ref #f 'abort-to-prompt)
                       tag)
                 args
                 (list tail))
       (lambda (cps args*)
         (match args*
           ((apply . apply-args)
            (with-cps cps
              (build-term ($continue k src ($call apply apply-args)))))))))

    (($ <conditional> src test consequent alternate)
     (define (convert-test cps test kt kf)
       (match test
         (($ <primcall> src (? branching-primitive? name) args)
          (convert-args cps args
            (lambda (cps args)
              (if (heap-type-predicate? name)
                  (with-cps cps
                    (letk kt* ($kargs () ()
                                ($branch kf kt src name #f args)))
                    (build-term
                      ($branch kf kt* src 'heap-object? #f args)))
                  (with-cps cps
                    (build-term ($branch kf kt src name #f args)))))))
         (($ <conditional> src test consequent alternate)
          (with-cps cps
            (let$ t (convert-test consequent kt kf))
            (let$ f (convert-test alternate kt kf))
            (letk kt* ($kargs () () ,t))
            (letk kf* ($kargs () () ,f))
            ($ (convert-test test kt* kf*))))
         (($ <const> src c)
          (with-cps cps
            (build-term ($continue (if c kt kf) src ($values ())))))
         (_ (convert-arg cps test
              (lambda (cps test)
                (with-cps cps
                  (build-term ($branch kt kf src 'false? #f (test)))))))))
     (with-cps cps
       (let$ t (convert consequent k subst))
       (let$ f (convert alternate k subst))
       (letk kt ($kargs () () ,t))
       (letk kf ($kargs () () ,f))
       ($ (convert-test test kt kf))))

    (($ <lexical-set> src name gensym exp)
     (convert-arg cps exp
       (lambda (cps exp)
         (match (hashq-ref subst gensym)
           ((orig-var box #t)
            (with-cps cps
              (let$ k (adapt-arity k src 0))
              (build-term
                ($continue k src
                  ($primcall 'scm-set!/immediate '(box . 1) (box exp))))))))))

    (($ <seq> src head tail)
     (if (zero-valued? head)
         (with-cps cps
           (let$ tail (convert tail k subst))
           (letk kseq ($kargs () () ,tail))
           ($ (convert head kseq subst)))
         (with-cps cps
           (let$ tail (convert tail k subst))
           (letv vals)
           (letk kseq ($kargs ('vals) (vals) ,tail))
           (letk kreceive ($kreceive '() 'vals kseq))
           ($ (convert head kreceive subst)))))

    (($ <let> src names syms vals body)
     (let lp ((cps cps) (names names) (syms syms) (vals vals))
       (match (list names syms vals)
         ((() () ()) (convert cps body k subst))
         (((name . names) (sym . syms) (val . vals))
          (with-cps cps
            (let$ body (lp names syms vals))
            (let$ body (box-bound-var name sym body))
            ($ ((lambda (cps)
                  (if (single-valued? val)
                      (with-cps cps
                        (letk klet ($kargs (name) ((bound-var sym)) ,body))
                        ($ (convert val klet subst)))
                      (with-cps cps
                        (letv rest)
                        (letk klet ($kargs (name 'rest) ((bound-var sym) rest) ,body))
                        (letk kreceive ($kreceive (list name) 'rest klet))
                        ($ (convert val kreceive subst))))))))))))

    (($ <fix> src names gensyms funs body)
     ;; Some letrecs can be contified; that happens later.
     (define (convert-funs cps funs)
       (match funs
         (()
          (with-cps cps '()))
         ((fun . funs)
          (with-cps cps
            (let$ fun (convert fun k subst))
            (let$ funs (convert-funs funs))
            (cons (match fun
                    (($ $continue _ _ (and fun ($ $fun)))
                     fun))
                  funs)))))
     (if (current-topbox-scope)
         (let ((vars (map bound-var gensyms)))
           (with-cps cps
             (let$ body (convert body k subst))
             (letk krec ($kargs names vars ,body))
             (let$ funs (convert-funs funs))
             (build-term ($continue krec src ($rec names vars funs)))))
         (let ((scope-id (fresh-scope-id)))
           (with-cps cps
             (let$ body ((lambda (cps)
                           (parameterize ((current-topbox-scope scope-id))
                             (convert cps exp k subst)))))
             (letk kscope ($kargs () () ,body))
             ($ (capture-toplevel-scope src scope-id kscope))))))

    (($ <let-values> src exp
        ($ <lambda-case> lsrc req #f rest #f () syms body #f))
     (let ((names (append req (if rest (list rest) '())))
           (bound-vars (map bound-var syms)))
       (with-cps cps
         (let$ body (convert body k subst))
         (let$ body (box-bound-vars names syms body))
         (letk kargs ($kargs names bound-vars ,body))
         (letk kreceive ($kreceive req rest kargs))
         ($ (convert exp kreceive subst)))))))

(define (build-subst exp)
  "Compute a mapping from lexical gensyms to CPS variable indexes.  CPS
uses small integers to identify variables, instead of gensyms.

This subst table serves an additional purpose of mapping variables to
replacements.  The usual reason to replace one variable by another is
assignment conversion.  Default argument values is the other reason.

The result is a hash table mapping symbols to substitutions (in the case
that a variable is substituted) or to indexes.  A substitution is a list
of the form:

  (ORIG-INDEX SUBST-INDEX BOXED?)

A true value for BOXED?  indicates that the replacement variable is in a
box.  If a variable is not substituted, the mapped value is a small
integer."
  (let ((table (make-hash-table)))
    (define (down exp)
      (match exp
        (($ <lexical-set> src name sym exp)
         (match (hashq-ref table sym)
           ((orig subst #t) #t)
           ((orig subst #f) (hashq-set! table sym (list orig subst #t)))
           ((? number? idx) (hashq-set! table sym (list idx (fresh-var) #t)))))
        (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
         (fold-formals (lambda (name sym init seed)
                         (hashq-set! table sym
                                     (if init
                                         (list (fresh-var) (fresh-var) #f)
                                         (fresh-var))))
                       #f
                       (make-$arity req (or opt '()) rest
                                    (if kw (cdr kw) '()) (and kw (car kw)))
                       gensyms
                       inits))
        (($ <let> src names gensyms vals body)
         (for-each (lambda (sym)
                     (hashq-set! table sym (fresh-var)))
                   gensyms))
        (($ <fix> src names gensyms vals body)
         (for-each (lambda (sym)
                     (hashq-set! table sym (fresh-var)))
                   gensyms))
        (_ #t))
      (values))
    (define (up exp) (values))
    ((make-tree-il-folder) exp down up)
    table))

(define (cps-convert/thunk exp)
  (parameterize ((label-counter 0)
                 (var-counter 0)
                 (scope-counter 0))
    (with-cps empty-intmap
      (letv init)
      ;; Allocate kinit first so that we know that the entry point's
      ;; label is zero.  This simplifies data flow in the compiler if we
      ;; can just pass around the program as a map of continuations and
      ;; know that the entry point is label 0.
      (letk kinit ,#f)
      (letk ktail ($ktail))
      (let$ body (convert exp ktail (build-subst exp)))
      (letk kbody ($kargs () () ,body))
      (letk kclause ($kclause ('() '() #f '() #f) kbody #f))
      ($ ((lambda (cps)
            (let ((init (build-cont
                          ($kfun (tree-il-src exp) '() init ktail kclause))))
              (with-cps (persistent-intmap (intmap-replace! cps kinit init))
                kinit))))))))

(define *comp-module* (make-fluid))

(define %warning-passes
  `((unused-variable             . ,unused-variable-analysis)
    (unused-toplevel             . ,unused-toplevel-analysis)
    (unbound-variable            . ,unbound-variable-analysis)
    (macro-use-before-definition . ,macro-use-before-definition-analysis)
    (arity-mismatch              . ,arity-analysis)
    (format                      . ,format-analysis)))

(define (optimize-tree-il x e opts)
  (define warnings
    (or (and=> (memq #:warnings opts) cadr)
        '()))

  ;; Go through the warning passes.
  (let ((analyses (filter-map (lambda (kind)
                                (assoc-ref %warning-passes kind))
                              warnings)))
    (analyze-tree analyses x e))

  (optimize x e opts))

(define (canonicalize exp)
  (define-syntax-rule (with-lexical src id . body)
    (let ((k (lambda (id) . body)))
      (match id
        (($ <lexical-ref>) (k id))
        (_
         (let ((v (gensym "v ")))
           (make-let src (list 'v) (list v) (list id)
                     (k (make-lexical-ref src 'v v))))))))
  (define-syntax with-lexicals
    (syntax-rules ()
      ((with-lexicals src () . body) (let () . body))
      ((with-lexicals src (id . ids) . body)
       (with-lexical src id (with-lexicals src ids . body)))))
  (define (reduce-conditional exp)
    (match exp
      (($ <conditional> src
          ($ <conditional> _ test ($ <const> _ t) ($ <const> _ f))
          consequent alternate)
       (cond
        ((and t (not f))
         (reduce-conditional (make-conditional src test consequent alternate)))
        ((and (not t) f)
         (reduce-conditional (make-conditional src test alternate consequent)))
        (else
         exp)))
      (_ exp)))
  (define (evaluate-args-eagerly-if-needed src inits k)
    ;; Some macros generate calls to "vector" or "list" with like 300
    ;; arguments.  Since we eventually compile to lower-level operations
    ;; like make-vector and vector-set! or cons, it reduces live
    ;; variable pressure to sink initializers if we can, if we can prove
    ;; that the initializer can't capture the continuation.  (More on
    ;; that caveat here:
    ;; http://wingolog.org/archives/2013/11/02/scheme-quiz-time).
    ;;
    ;; Normally we would do this transformation in the optimizer, but
    ;; it's quite tricky there and quite easy here, so we do it here.
    (match inits
      (() (k '()))
      ((init . inits)
       (match init
         ((or ($ <const>) ($ <void>) ($ <lambda>) ($ <lexical-ref>))
          (evaluate-args-eagerly-if-needed
           src inits (lambda (inits) (k (cons init inits)))))
         (_
          (with-lexical
           src init
           (evaluate-args-eagerly-if-needed
            src inits (lambda (inits) (k (cons init inits))))))))))
  (post-order
   (lambda (exp)
     (match exp
       (($ <conditional>)
        (reduce-conditional exp))

       (($ <primcall> src 'exact-integer? (x))
        ;; Both fixnum? and bignum? are branching primitives.
        (with-lexicals src (x)
          (make-conditional
           src (make-primcall src 'fixnum? (list x))
           (make-const src #t)
           (make-conditional src (make-primcall src 'bignum? (list x))
                             (make-const src #t)
                             (make-const src #f)))))

       (($ <primcall> src '<= (a b))
        ;; No need to reduce as <= is a branching primitive.
        (make-conditional src (make-primcall src '<= (list a b))
                          (make-const src #t)
                          (make-const src #f)))

       (($ <primcall> src '>= (a b))
        ;; No need to reduce as < is a branching primitive.
        (make-conditional src (make-primcall src '<= (list b a))
                          (make-const src #t)
                          (make-const src #f)))

       (($ <primcall> src '> (a b))
        ;; No need to reduce as < is a branching primitive.
        (make-conditional src (make-primcall src '< (list b a))
                          (make-const src #t)
                          (make-const src #f)))

       (($ <primcall> src (? branching-primitive? name) args)
        ;; No need to reduce because test is not reducible: reifying
        ;; #t/#f is the right thing.
        (make-conditional src exp
                          (make-const src #t)
                          (make-const src #f)))

       (($ <primcall> src 'not (x))
        (reduce-conditional
         (make-conditional src x
                           (make-const src #f)
                           (make-const src #t))))

       (($ <primcall> src (or 'eqv? 'equal?) (a b))
        (let ()
          (define-syntax-rule (primcall name . args)
            (make-primcall src 'name (list . args)))
          (define-syntax primcall-chain
            (syntax-rules ()
              ((_ x) x)
              ((_ x . y)
               (make-conditional src (primcall . x) (primcall-chain . y)
                                 (make-const src #f)))))
          (define-syntax-rule (bool x)
            (make-conditional src x (make-const src #t) (make-const src #f)))
          (with-lexicals src (a b)
            (make-conditional
             src
             (primcall eq? a b)
             (make-const src #t)
             (match (primcall-name exp)
               ('eqv?
                ;; Completely inline.
                (primcall-chain (heap-number? a)
                                (heap-number? b)
                                (bool (primcall heap-numbers-equal? a b))))
               ('equal?
                ;; Partially inline.
                (primcall-chain (heap-object? a)
                                (heap-object? b)
                                (primcall equal? a b))))))))

       (($ <primcall> src 'vector args)
        ;; Expand to "make-vector" + "vector-set!".
        (evaluate-args-eagerly-if-needed
         src args
         (lambda (args)
           (define-syntax-rule (primcall name . args)
             (make-primcall src 'name (list . args)))
           (define-syntax-rule (const val)
             (make-const src val))
           (let ((v (primcall make-vector (const (length args)) (const #f))))
             (with-lexicals src (v)
               (list->seq
                src
                (append (map (lambda (idx arg)
                               (primcall vector-set! v (const idx) arg))
                             (iota (length args))
                             args)
                        (list v))))))))

       (($ <primcall> src 'list args)
        ;; Expand to "cons".
        (evaluate-args-eagerly-if-needed
         src args
         (lambda (args)
           (define-syntax-rule (primcall name . args)
             (make-primcall src 'name (list . args)))
           (define-syntax-rule (const val)
             (make-const src val))
           (fold (lambda (arg tail) (primcall cons arg tail))
                 (const '())
                 (reverse args)))))

       (($ <primcall> src 'struct-set! (struct index value))
        ;; Unhappily, and undocumentedly, struct-set! returns the value
        ;; that was set.  There is code that relies on this.  Hackety
        ;; hack...
        (with-lexicals src (value)
          (make-seq src
                    (make-primcall src 'struct-set!
                                   (list struct index value))
                    value)))

       ;; Lower (logand x (lognot y)) to (logsub x y).  We do it here
       ;; instead of in CPS because it gets rid of the lognot entirely;
       ;; if type folding can't prove Y to be an exact integer, then DCE
       ;; would have to leave it in the program for its possible
       ;; effects.
       (($ <primcall> src 'logand (x ($ <primcall> _ 'lognot (y))))
        (make-primcall src 'logsub (list x y)))
       (($ <primcall> src 'logand (($ <primcall> _ 'lognot (y)) x))
        (make-primcall src 'logsub (list x y)))

       (($ <primcall> src 'throw ())
        (make-call src (make-primitive-ref src 'throw) '()))

       (($ <prompt> src escape-only? tag body
           ($ <lambda> hsrc hmeta
              ($ <lambda-case> _ hreq #f hrest #f () hsyms hbody #f)))
        exp)

       (($ <primcall> src 'ash (a b))
        (match b
          (($ <const> src2 (? exact-integer? n))
           (if (< n 0)
               (make-primcall src 'rsh (list a (make-const src2 (- n))))
               (make-primcall src 'lsh (list a b))))
          (_
           (with-lexicals src (a b)
             (make-conditional
              src
              (make-primcall src '< (list b (make-const src 0)))
              (let ((n (make-primcall src '- (list (make-const src 0) b))))
                (make-primcall src 'rsh (list a n)))
              (make-primcall src 'lsh (list a b)))))))

       ;; Eta-convert prompts without inline handlers.
       (($ <prompt> src escape-only? tag body handler)
        (let ((h (gensym "h "))
              (args (gensym "args ")))
          (define-syntax-rule (primcall name . args)
            (make-primcall src 'name (list . args)))
          (define-syntax-rule (const val)
            (make-const src val))
          (with-lexicals src (handler)
            (make-conditional
             src
             (primcall procedure? handler)
             (make-prompt
              src escape-only? tag body
              (make-lambda
               src '()
               (make-lambda-case
                src '() #f 'args #f '() (list args)
                (primcall apply handler (make-lexical-ref #f 'args args))
                #f)))
             (primcall throw
                       (const 'wrong-type-arg)
                       (const "call-with-prompt")
                       (const "Wrong type (expecting procedure): ~S")
                       (primcall cons handler (const '()))
                       (primcall cons handler (const '())))))))
       (_ exp)))
   exp))

(define (compile-cps exp env opts)
  (values (cps-convert/thunk
           (canonicalize (optimize-tree-il exp env opts)))
          env
          env))

;;; Local Variables:
;;; eval: (put 'convert-arg 'scheme-indent-function 2)
;;; eval: (put 'convert-args 'scheme-indent-function 2)
;;; eval: (put 'with-lexicals 'scheme-indent-function 2)
;;; End:
