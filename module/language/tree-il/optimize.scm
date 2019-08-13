;;; Tree-il optimizer

;; Copyright (C) 2009, 2010-2015, 2018 Free Software Foundation, Inc.

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

;;; Code:

(define-module (language tree-il optimize)
  #:use-module (language tree-il)
  #:use-module (language tree-il primitives)
  #:use-module (language tree-il peval)
  #:use-module (language tree-il fix-letrec)
  #:use-module (language tree-il debug)
  #:use-module (ice-9 match)
  #:export (optimize
            tree-il-optimizations))

(define (kw-arg-ref args kw default)
  (match (memq kw args)
    ((_ val . _) val)
    (_ default)))

(define *debug?* #f)

(define (maybe-verify x)
  (if *debug?*
      (verify-tree-il x)
      x))

(define (optimize x env opts)
  (define-syntax-rule (run-pass pass kw default)
    (when (kw-arg-ref opts kw default)
      (set! x (maybe-verify (pass x)))))
  (define (resolve* x) (resolve-primitives x env))
  (define (peval* x) (peval x env))
  (maybe-verify x)
  (run-pass resolve*           #:resolve-primitives? #t)
  (run-pass expand-primitives  #:expand-primitives?  #t)
  (set! x (fix-letrec x))
  (run-pass peval*             #:partial-eval?       #t)
  x)

(define (tree-il-optimizations)
  ;; Avoid resolve-primitives until -O2, when CPS optimizations kick in.
  ;; Otherwise, inlining the primcalls during Tree-IL->CPS compilation
  ;; will result in a lot of code that will never get optimized nicely.
  '((#:resolve-primitives? 2)
    (#:expand-primitives? 1)
    (#:partial-eval? 1)))
