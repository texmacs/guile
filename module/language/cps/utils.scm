;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015, 2017, 2018, 2019 Free Software Foundation, Inc.

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
;;; Helper facilities for working with CPS.
;;;
;;; Code:

(define-module (language cps utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (language cps)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:use-module (language cps graphs)
  #:export (;; Fresh names.
            label-counter var-counter
            fresh-label fresh-var
            with-fresh-name-state compute-max-label-and-var
            let-fresh

            ;; Graphs.
            compute-function-body
            compute-reachable-functions
            compute-successors
            compute-predecessors
            compute-idoms
            compute-dom-edges)
  #:re-export (fold1 fold2
               trivial-intset
               intmap-map
               intmap-keys
               invert-bijection invert-partition
               intset->intmap
               worklist-fold
               fixpoint

               ;; Flow analysis.
               invert-graph
               compute-reverse-post-order
               compute-strongly-connected-components
               compute-sorted-strongly-connected-components
               solve-flow-equations))

(define label-counter (make-parameter #f))
(define var-counter (make-parameter #f))

(define (fresh-label)
  (let ((count (or (label-counter)
                   (error "fresh-label outside with-fresh-name-state"))))
    (label-counter (1+ count))
    count))

(define (fresh-var)
  (let ((count (or (var-counter)
                   (error "fresh-var outside with-fresh-name-state"))))
    (var-counter (1+ count))
    count))

(define-syntax-rule (let-fresh (label ...) (var ...) body ...)
  (let* ((label (fresh-label)) ...
         (var (fresh-var)) ...)
    body ...))

(define-syntax-rule (with-fresh-name-state fun body ...)
  (call-with-values (lambda () (compute-max-label-and-var fun))
    (lambda (max-label max-var)
      (parameterize ((label-counter (1+ max-label))
                     (var-counter (1+ max-var)))
        body ...))))

(define (compute-max-label-and-var conts)
  (values (or (intmap-prev conts) -1)
          (intmap-fold (lambda (k cont max-var)
                         (match cont
                           (($ $kargs names syms body)
                            (apply max max-var syms))
                           (($ $kfun src meta (and self (not #f)))
                            (max max-var self))
                           (_ max-var)))
                       conts
                       -1)))

(define (compute-function-body conts kfun)
  (persistent-intset
   (let visit-cont ((label kfun) (labels empty-intset))
     (cond
      ((intset-ref labels label) labels)
      (else
       (let ((labels (intset-add! labels label)))
         (match (intmap-ref conts label)
           (($ $kreceive arity k) (visit-cont k labels))
           (($ $kfun src meta self ktail kclause)
            (let ((labels (visit-cont ktail labels)))
              (if kclause
                  (visit-cont kclause labels)
                  labels)))
           (($ $ktail) labels)
           (($ $kclause arity kbody kalt)
            (if kalt
                (visit-cont kalt (visit-cont kbody labels))
                (visit-cont kbody labels)))
           (($ $kargs names syms term)
            (match term
              (($ $continue k)
               (visit-cont k labels))
              (($ $branch kf kt)
               (visit-cont kf (visit-cont kt labels)))
              (($ $prompt k kh)
               (visit-cont k (visit-cont kh labels)))
              (($ $throw)
               labels))))))))))

(define* (compute-reachable-functions conts #:optional (kfun 0))
  "Compute a mapping LABEL->LABEL..., where each key is a reachable
$kfun and each associated value is the body of the function, as an
intset."
  (define (intset-cons i set) (intset-add set i))
  (define (visit-fun kfun body to-visit)
    (intset-fold
     (lambda (label to-visit)
       (define (return kfun*) (fold intset-cons to-visit kfun*))
       (define (return1 kfun) (intset-add to-visit kfun))
       (define (return0) to-visit)
       (match (intmap-ref conts label)
         (($ $kargs _ _ ($ $continue _ _ exp))
          (match exp
            (($ $fun label) (return1 label))
            (($ $rec _ _ (($ $fun labels) ...)) (return labels))
            (($ $const-fun label) (return1 label))
            (($ $code label) (return1 label))
            (($ $callk label) (return1 label))
            (_ (return0))))
         (_ (return0))))
     body
     to-visit))
  (let lp ((to-visit (intset kfun)) (visited empty-intmap))
    (let ((to-visit (intset-subtract to-visit (intmap-keys visited))))
      (if (eq? to-visit empty-intset)
          visited
          (call-with-values
              (lambda ()
                (intset-fold
                 (lambda (kfun to-visit visited)
                   (let ((body (compute-function-body conts kfun)))
                     (values (visit-fun kfun body to-visit)
                             (intmap-add visited kfun body))))
                 to-visit
                 empty-intset
                 visited))
            lp)))))

(define* (compute-successors conts #:optional (kfun (intmap-next conts)))
  (define (visit label succs)
    (let visit ((label kfun) (succs empty-intmap))
      (define (propagate0)
        (intmap-add! succs label empty-intset))
      (define (propagate1 succ)
        (visit succ (intmap-add! succs label (intset succ))))
      (define (propagate2 succ0 succ1)
        (let ((succs (intmap-add! succs label (intset succ0 succ1))))
          (visit succ1 (visit succ0 succs))))
      (if (intmap-ref succs label (lambda (_) #f))
          succs
          (match (intmap-ref conts label)
            (($ $kargs names vars term)
             (match term
               (($ $continue k) (propagate1 k))
               (($ $branch kf kt) (propagate2 kf kt))
               (($ $prompt k kh) (propagate2 k kh))
               (($ $throw) (propagate0))))
            (($ $kreceive arity k)
             (propagate1 k))
            (($ $kfun src meta self tail clause)
             (if clause
                 (propagate2 clause tail)
                 (propagate1 tail)))
            (($ $kclause arity kbody kalt)
             (if kalt
                 (propagate2 kbody kalt)
                 (propagate1 kbody)))
            (($ $ktail) (propagate0))))))
  (persistent-intmap (visit kfun empty-intmap)))

(define* (compute-predecessors conts kfun #:key
                               (labels (compute-function-body conts kfun)))
  (define (meet cdr car)
    (cons car cdr))
  (define (add-preds label preds)
    (define (add-pred k preds)
      (intmap-add! preds k label meet))
    (match (intmap-ref conts label)
      (($ $kreceive arity k)
       (add-pred k preds))
      (($ $kfun src meta self ktail kclause)
       (add-pred ktail (if kclause (add-pred kclause preds) preds)))
      (($ $ktail)
       preds)
      (($ $kclause arity kbody kalt)
       (add-pred kbody (if kalt (add-pred kalt preds) preds)))
      (($ $kargs names syms term)
       (match term
         (($ $continue k)   (add-pred k preds))
         (($ $branch kf kt) (add-pred kf (add-pred kt preds)))
         (($ $prompt k kh)  (add-pred k (add-pred kh preds)))
         (($ $throw)        preds)))))
  (persistent-intmap
   (intset-fold add-preds labels
                (intset->intmap (lambda (label) '()) labels))))

;; Precondition: For each function in CONTS, the continuation names are
;; topologically sorted.
(define (compute-idoms conts kfun)
  ;; This is the iterative O(n^2) fixpoint algorithm, originally from
  ;; Allen and Cocke ("Graph-theoretic constructs for program flow
  ;; analysis", 1972).  See the discussion in Cooper, Harvey, and
  ;; Kennedy's "A Simple, Fast Dominance Algorithm", 2001.
  (let ((preds-map (compute-predecessors conts kfun)))
    (define (compute-idom idoms preds)
      (define (idom-ref label)
        (intmap-ref idoms label (lambda (_) #f)))
      (match preds
        (() -1)
        ((pred) pred)                   ; Shortcut.
        ((pred . preds)
         (define (common-idom d0 d1)
           ;; We exploit the fact that a reverse post-order is a
           ;; topological sort, and so the idom of a node is always
           ;; numerically less than the node itself.
           (let lp ((d0 d0) (d1 d1))
             (cond
              ;; d0 or d1 can be false on the first iteration.
              ((not d0) d1)
              ((not d1) d0)
              ((= d0 d1) d0)
              ((< d0 d1) (lp d0 (idom-ref d1)))
              (else (lp (idom-ref d0) d1)))))
         (fold1 common-idom preds pred))))
    (define (adjoin-idom label preds idoms)
      (let ((idom (compute-idom idoms preds)))
        ;; Don't use intmap-add! here.
        (intmap-add idoms label idom (lambda (old new) new))))
    (fixpoint (lambda (idoms)
                (intmap-fold adjoin-idom preds-map idoms))
              empty-intmap)))

;; Precondition: For each function in CONTS, the continuation names are
;; topologically sorted.
(define (compute-idoms conts kfun)
  ;; This is the iterative O(n^2) fixpoint algorithm, originally from
  ;; Allen and Cocke ("Graph-theoretic constructs for program flow
  ;; analysis", 1972).  See the discussion in Cooper, Harvey, and
  ;; Kennedy's "A Simple, Fast Dominance Algorithm", 2001.
  (let ((preds-map (compute-predecessors conts kfun)))
    (define (compute-idom idoms preds)
      (define (idom-ref label)
        (intmap-ref idoms label (lambda (_) #f)))
      (match preds
        (() -1)
        ((pred) pred)                   ; Shortcut.
        ((pred . preds)
         (define (common-idom d0 d1)
           ;; We exploit the fact that a reverse post-order is a
           ;; topological sort, and so the idom of a node is always
           ;; numerically less than the node itself.
           (let lp ((d0 d0) (d1 d1))
             (cond
              ;; d0 or d1 can be false on the first iteration.
              ((not d0) d1)
              ((not d1) d0)
              ((= d0 d1) d0)
              ((< d0 d1) (lp d0 (idom-ref d1)))
              (else (lp (idom-ref d0) d1)))))
         (fold1 common-idom preds pred))))
    (define (adjoin-idom label preds idoms)
      (let ((idom (compute-idom idoms preds)))
        ;; Don't use intmap-add! here.
        (intmap-add idoms label idom (lambda (old new) new))))
    (fixpoint (lambda (idoms)
                (intmap-fold adjoin-idom preds-map idoms))
              empty-intmap)))

;; Compute a vector containing, for each node, a list of the nodes that
;; it immediately dominates.  These are the "D" edges in the DJ tree.
(define (compute-dom-edges idoms)
  (define (snoc cdr car) (cons car cdr))
  (persistent-intmap
   (intmap-fold (lambda (label idom doms)
                  (let ((doms (intmap-add! doms label '())))
                    (cond
                     ((< idom 0) doms) ;; No edge to entry.
                     (else (intmap-add! doms idom label snoc)))))
                idoms
                empty-intmap)))

