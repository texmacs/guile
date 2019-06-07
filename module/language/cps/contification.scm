;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013-2019 Free Software Foundation, Inc.

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
;;; Contification is a pass that turns $fun instances into $cont
;;; instances if all calls to the $fun return to the same continuation.
;;; This is a more rigorous variant of our old "fixpoint labels
;;; allocation" optimization.
;;;
;;; See Kennedy's "Compiling with Continuations, Continued", and Fluet
;;; and Weeks's "Contification using Dominators".
;;;
;;; Code:

(define-module (language cps contification)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (language cps)
  #:use-module (language cps renumber)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps with-cps)
  #:export (contify))

(define (compute-singly-referenced-labels conts)
  "Compute the set of labels in CONTS that have exactly one
predecessor."
  (define (add-ref label cont single multiple)
    (define (ref k single multiple)
      (if (intset-ref single k)
          (values single (intset-add! multiple k))
          (values (intset-add! single k) multiple)))
    (define (ref0) (values single multiple))
    (define (ref1 k) (ref k single multiple))
    (define (ref2 k k*)
      (if k*
          (let-values (((single multiple) (ref k single multiple)))
            (ref k* single multiple))
          (ref1 k)))
    (match cont
      (($ $kreceive arity k) (ref1 k))
      (($ $kfun src meta self ktail kclause) (ref2 ktail kclause))
      (($ $ktail) (ref0))
      (($ $kclause arity kbody kalt) (ref2 kbody kalt))
      (($ $kargs names syms ($ $continue k)) (ref1 k))
      (($ $kargs names syms ($ $branch kf kt)) (ref2 kf kt))
      (($ $kargs names syms ($ $prompt k kh)) (ref2 k kh))
      (($ $kargs names syms ($ $throw)) (ref0))))
  (let*-values (((single multiple) (values empty-intset empty-intset))
                ((single multiple) (intmap-fold add-ref conts single multiple)))
    (intset-subtract (persistent-intset single)
                     (persistent-intset multiple))))

(define (compute-functions conts)
  "Compute a map from $kfun label to bound variable names for all
functions in CONTS.  Functions have two bound variable names: their self
binding, and the name they are given in their continuation.  If their
continuation has more than one predecessor, then the bound variable name
doesn't uniquely identify the function, so we exclude that function from
the set."
  (define (function-self label)
    (match (intmap-ref conts label)
      (($ $kfun src meta self) self)))
  (let ((single (compute-singly-referenced-labels conts)))
    (intmap-fold (lambda (label cont functions)
                   (match cont
                     (($ $kargs _ _ ($ $continue k src ($ $fun kfun)))
                      (if (intset-ref single k)
                          (match (intmap-ref conts k)
                            (($ $kargs (name) (var))
                             (intmap-add functions kfun
                                         (intset var (function-self kfun)))))
                          functions))
                     (($ $kargs _ _ ($ $continue k src
                                       ($ $rec _ vars (($ $fun kfuns) ...))))
                      (if (intset-ref single k)
                          (fold (lambda (var kfun functions)
                                  (intmap-add functions kfun
                                              (intset var (function-self kfun))))
                                functions vars kfuns)
                          functions))
                     (_ functions)))
                 conts
                 empty-intmap)))

(define (compute-arities conts functions)
  "Given the map FUNCTIONS whose keys are $kfun labels, return a map
from label to arities."
  (define (clause-arities clause)
    (if clause
        (match (intmap-ref conts clause)
          (($ $kclause arity body alt)
           (cons arity (clause-arities alt))))
        '()))
  (intmap-map (lambda (label vars)
                 (match (intmap-ref conts label)
                   (($ $kfun src meta self tail clause)
                    (clause-arities clause))))
              functions))

;; For now, we don't contify functions with optional, keyword, or rest
;; arguments.
(define (contifiable-arity? arity)
  (match arity
    (($ $arity req () #f () aok?)
     #t)
    (_
     #f)))

(define (arity-matches? arity nargs)
  (match arity
    (($ $arity req () #f () aok?)
     (= nargs (length req)))
    (_
     #f)))

(define (compute-contification-candidates conts)
  "Compute and return a label -> (variable ...) map describing all
functions with known uses that are only ever used as the operator of a
$call, and are always called with a compatible arity."
  (let* ((functions (compute-functions conts))
         (vars (intmap-fold (lambda (label vars out)
                              (intset-fold (lambda (var out)
                                             (intmap-add out var label))
                                           vars out))
                            functions
                            empty-intmap))
         (arities (compute-arities conts functions)))
    (define (restrict-arity functions proc nargs)
      (match (intmap-ref vars proc (lambda (_) #f))
        (#f functions)
        (label
         (let lp ((arities (intmap-ref arities label)))
           (match arities
             (() (intmap-remove functions label))
             ((arity . arities)
              (cond
               ((not (contifiable-arity? arity)) (lp '()))
               ((arity-matches? arity nargs) functions)
               (else (lp arities)))))))))
    (define (visit-cont label cont functions)
      (define (exclude-var functions var)
        (match (intmap-ref vars var (lambda (_) #f))
          (#f functions)
          (label (intmap-remove functions label))))
      (define (exclude-vars functions vars)
        (match vars
          (() functions)
          ((var . vars)
           (exclude-vars (exclude-var functions var) vars))))
      (match cont
        (($ $kargs _ _ ($ $continue _ _ exp))
         (match exp
           ((or ($ $const) ($ $prim) ($ $const-fun) ($ $code) ($ $fun) ($ $rec))
            functions)
           (($ $values args)
            (exclude-vars functions args))
           (($ $call proc args)
            (let ((functions (exclude-vars functions args)))
              ;; Note that this contification algorithm is happy to
              ;; contify the `lp' in this example into a shared tail
              ;; between clauses:
              ;;
              ;; (letrec ((lp (lambda () (lp))))
              ;;   (case-lambda
              ;;     ((a) (lp))
              ;;     ((a b) (lp))))
              ;;
              ;; This can cause cross-clause jumps.  The rest of the
              ;; compiler handles this fine though, so we allow it.
              (restrict-arity functions proc (length args))))
           (($ $callk k proc args)
            (exclude-vars functions (if proc (cons proc args) args)))
           (($ $primcall name param args)
            (exclude-vars functions args))))
        (($ $kargs _ _ ($ $branch kf kt src op param args))
         (exclude-vars functions args))
        (($ $kargs _ _ ($ $prompt k kh src escape? tag))
         (exclude-var functions tag))
        (($ $kargs _ _ ($ $throw src op param args))
         (exclude-vars functions args))
        (_ functions)))
    (intmap-fold visit-cont conts functions)))

(define (compute-call-graph conts labels vars)
  "Given the set of contifiable functions LABELS and associated bound
variables VARS, compute and return two values: a map
LABEL->LABEL... indicating the contifiable functions called by a
function, and a map LABEL->LABEL... indicating the return continuations
for a function.  The first return value also has an entry
0->LABEL... indicating all contifiable functions called by
non-contifiable functions.  We assume that 0 is not in the contifiable
function set."
  (let ((bodies
         ;; label -> fun-label for all labels in bodies of contifiable
         ;; functions
         (intset-fold (lambda (fun-label bodies)
                        (intset-fold (lambda (label bodies)
                                       (intmap-add bodies label fun-label))
                                     (compute-function-body conts fun-label)
                                     bodies))
                      labels
                      empty-intmap)))
    (when (intset-ref labels 0)
      (error "internal error: label 0 should not be contifiable"))
    (intmap-fold
     (lambda (label cont calls returns)
       (match cont
         (($ $kargs _ _ ($ $continue k src ($ $call proc)))
          (match (intmap-ref vars proc (lambda (_) #f))
            (#f (values calls returns))
            (callee
             (let ((caller (intmap-ref bodies label (lambda (_) 0))))
               (values (intmap-add calls caller callee intset-add)
                       (intmap-add returns callee k intset-add))))))
         (_ (values calls returns))))
     conts
     (intset->intmap (lambda (label) empty-intset) (intset-add labels 0))
     (intset->intmap (lambda (label) empty-intset) labels))))

(define (tail-label conts label)
  (match (intmap-ref conts label)
    (($ $kfun src meta self tail body)
     tail)))

(define (compute-return-labels labels tails returns return-substs)
  (define (subst k)
    (match (intmap-ref return-substs k (lambda (_) #f))
      (#f k)
      (k (subst k))))
  ;; Compute all return labels, then subtract tail labels of the
  ;; functions in question.
  (intset-subtract
   ;; Return labels for all calls to these labels.
   (intset-fold (lambda (label out)
                  (intset-fold (lambda (k out)
                                 (intset-add out (subst k)))
                               (intmap-ref returns label)
                               out))
                labels
                empty-intset)
   (intset-fold (lambda (label out)
                  (intset-add out (intmap-ref tails label)))
                labels
                empty-intset)))

(define (intmap->intset map)
  (define (add-key label cont labels)
    (intset-add labels label))
  (intmap-fold add-key map empty-intset))

(define (filter-contifiable contified groups)
  (intmap-fold (lambda (id labels groups)
                 (let ((labels (intset-subtract labels contified)))
                   (if (eq? empty-intset labels)
                       groups
                       (intmap-add groups id labels))))
               groups
               empty-intmap))

(define (trivial-set set)
  (let ((first (intset-next set)))
    (and first
         (not (intset-next set (1+ first)))
         first)))

(define (compute-contification conts)
  (let*-values
      (;; label -> (var ...)
       ((candidates) (compute-contification-candidates conts))
       ((labels) (intmap->intset candidates))
       ;; var -> label
       ((vars) (intmap-fold (lambda (label vars out)
                              (intset-fold (lambda (var out)
                                             (intmap-add out var label))
                                           vars out))
                            candidates
                            empty-intmap))
       ;; caller-label -> callee-label..., callee-label -> return-label...
       ((calls returns) (compute-call-graph conts labels vars))
       ;; callee-label -> tail-label
       ((tails) (intset-fold
                 (lambda (label tails)
                   (intmap-add tails label (tail-label conts label)))
                 labels
                 empty-intmap))
       ;; Strongly connected components, allowing us to contify mutually
       ;; tail-recursive functions.  Since `compute-call-graph' added on
       ;; a synthetic 0->LABEL... entry for contifiable functions called
       ;; by non-contifiable functions, we need to remove that entry
       ;; from the partition.  It will be in its own component, as it
       ;; has no predecessors.
       ;;
       ;; id -> label...
       ((groups) (intmap-remove
                  (compute-strongly-connected-components calls 0)
                  0)))
    ;; todo: thread groups through contification
    (define (attempt-contification labels contified return-substs)
      (let ((returns (compute-return-labels labels tails returns
                                            return-substs)))
        (cond
         ((trivial-set returns)
          => (lambda (k)
               ;; Success!
               (values (intset-union contified labels)
                       (intset-fold (lambda (label return-substs)
                                      (let ((tail (intmap-ref tails label)))
                                        (intmap-add return-substs tail k)))
                                    labels return-substs))))
         ((trivial-set labels)
          ;; Single-label SCC failed to contify.
          (values contified return-substs))
         (else
          ;; Multi-label SCC failed to contify.  Try instead to contify
          ;; each one.
          (intset-fold
           (lambda (label contified return-substs)
             (let ((labels (intset-add empty-intset label)))
               (attempt-contification labels contified return-substs)))
           labels contified return-substs)))))
    (call-with-values
        (lambda ()
          (fixpoint
           (lambda (contified return-substs)
             (intmap-fold
              (lambda (id group contified return-substs)
                (attempt-contification group contified return-substs))
              (filter-contifiable contified groups)
              contified
              return-substs))
           empty-intset
           empty-intmap))
      (lambda (contified return-substs)
        (values (intset-fold (lambda (label call-substs)
                               (intset-fold
                                (lambda (var call-substs)
                                  (intmap-add call-substs var label))
                                (intmap-ref candidates label)
                                call-substs))
                             contified
                             empty-intmap)
                return-substs)))))

(define (apply-contification conts call-substs return-substs)
  (define (call-subst proc)
    (intmap-ref call-substs proc (lambda (_) #f)))
  (define (return-subst k)
    (intmap-ref return-substs k (lambda (_) #f)))
  (define (find-body kfun nargs)
    (match (intmap-ref conts kfun)
      (($ $kfun src meta self tail clause)
       (let lp ((clause clause))
         (match (intmap-ref conts clause)
           (($ $kclause arity body alt)
            (if (arity-matches? arity nargs)
                body
                (lp alt))))))))
  (define (inline-return cps k* kargs src nreq rest vals)
    (define (build-list cps k src vals)
      (match vals
        (()
         (with-cps cps
           (build-term ($continue k src ($const '())))))
        ((v . vals)
         (with-cps cps
           (letv pair tail)
           (letk kdone ($kargs () () ($continue k src ($values (pair)))))
           (letk ktail
                 ($kargs () ()
                   ($continue kdone src
                     ($primcall 'scm-set!/immediate '(pair . 1) (pair tail)))))
           (letk khead
                 ($kargs ('pair) (pair)
                   ($continue ktail src
                     ($primcall 'scm-set!/immediate '(pair . 0) (pair v)))))
           (letk ktail
                 ($kargs ('tail) (tail)
                   ($continue khead src
                     ($primcall 'allocate-words/immediate '(pair . 2) ()))))
           ($ (build-list ktail src vals))))))
    (cond
     ((and (not rest) (eqv? (length vals) nreq))
      (with-cps cps
        (build-term ($continue kargs src ($values vals)))))
     ((and rest (<= nreq (length vals)))
      (with-cps cps
        (letv rest)
        (letk krest ($kargs ('rest) (rest)
                      ($continue kargs src
                        ($values ,(append (list-head vals nreq)
                                          (list rest))))))
        ($ (build-list krest src (list-tail vals nreq)))))
     (else
      ;; Fallback case if values don't match.
      (with-cps cps
        (letv prim)
        (letk kprim ($kargs ('prim) (prim)
                      ($continue k* src ($call prim vals))))
        (build-term ($continue kprim src ($prim 'values)))))))
  (define (continue cps k src exp)
    (define (lookup-return-cont k)
      (match (return-subst k)
        (#f k)
        (k (lookup-return-cont k))))
    (let ((k* (lookup-return-cont k)))
      (if (eq? k k*)
          (with-cps cps (build-term ($continue k src ,exp)))
          ;; We are contifying this return.  It must be a call or a
          ;; $values expression.  k* will be either a $ktail or a
          ;; $kreceive continuation.
          (match (intmap-ref conts k*)
            (($ $kreceive ($ $arity req () rest () #f) kargs)
             (match exp
               (($ $call)
                (with-cps cps (build-term ($continue k* src ,exp))))
               ;; We need to punch through the $kreceive; otherwise we'd
               ;; have to rewrite as a call to the 'values primitive.
               (($ $values vals)
                (inline-return cps k* kargs src (length req) rest vals))))
            (($ $ktail)
             (with-cps cps (build-term ($continue k* src ,exp))))))))
  (define (visit-exp cps k src exp)
    (match exp
      (($ $call proc args)
       ;; If proc is contifiable, replace call with jump.
       (match (call-subst proc)
         (#f (continue cps k src exp))
         (kfun
          (let ((body (find-body kfun (length args))))
            (with-cps cps
              (build-term ($continue body src ($values args))))))))
      (($ $fun kfun)
       ;; If the function's tail continuation has been
       ;; substituted, that means it has been contified.
       (if (return-subst (tail-label conts kfun))
           (continue cps k src (build-exp ($values ())))
           (continue cps k src exp)))
      (($ $rec names vars funs)
       (match (filter (match-lambda ((n v f) (not (call-subst v))))
                      (map list names vars funs))
         (() (continue cps k src (build-exp ($values ()))))
         (((names vars funs) ...)
          (continue cps k src (build-exp ($rec names vars funs))))))
      (_ (continue cps k src exp))))
  (define (visit-term cps term)
    (match term
      (($ $continue k src exp)
       (visit-exp cps k src exp))
      ((or ($ $branch) ($ $prompt) ($ $throw))
       (with-cps cps term))))

  ;; Renumbering is not strictly necessary but some passes may not be
  ;; equipped to deal with stale $kfun nodes whose bodies have been
  ;; wired into other functions.
  (renumber
   (with-fresh-name-state conts
     (intmap-fold
      (lambda (label cont out)
        (match cont
          (($ $kargs names vars term)
           ;; Remove bindings for functions that have been contified.
           (match (filter (match-lambda ((name var) (not (call-subst var))))
                          (map list names vars))
             (((names vars) ...)
              (with-cps out
                (let$ term (visit-term term))
                (setk label ($kargs names vars ,term))))))
          (_ out)))
      conts
      conts))))

(define (contify conts)
  ;; FIXME: Renumbering isn't really needed but dead continuations may
  ;; cause compute-singly-referenced-labels to spuriously mark some
  ;; conts as irreducible.  For now we punt and renumber so that there
  ;; are only live conts.
  (let ((conts (renumber conts)))
    (let-values (((call-substs return-substs) (compute-contification conts)))
      (apply-contification conts call-substs return-substs))))
