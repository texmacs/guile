;;; Optimization flags

;; Copyright (C) 2018 Free Software Foundation, Inc.

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

(define-module (system base optimize)
  #:use-module (language tree-il optimize)
  #:use-module (language cps optimize)
  #:use-module (ice-9 match)
  #:export (available-optimizations
            pass-optimization-level
            optimizations-for-level))

(define (available-optimizations)
  (append (tree-il-optimizations) (cps-optimizations)))

(define (pass-optimization-level kw)
  (match (assq kw (available-optimizations))
    ((kw level) level)
    (_ (error "unknown optimization" kw))))

;; Turn on all optimizations unless -O0.
(define (optimizations-for-level level)
  (let lp ((options (available-optimizations)))
    (match options
      (() '())
      (((kw at-level) . options)
       (cons* kw (<= at-level level) (lp options))))))
