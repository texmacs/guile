;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2016, 2017, 2018 Free Software Foundation, Inc.

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
;;; A pass to add "handle-interrupts" primcalls before calls, loop
;;; back-edges, and returns.
;;;
;;; Code:

(define-module (language cps handle-interrupts)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps renumber)
  #:export (add-handle-interrupts))

(define (compute-safepoints cps)
  (define (maybe-add-safepoint label k safepoints)
    "Add K to safepoints if it is a target of a backward branch."
    (if (<= k label)
        (intset-add! safepoints k)
        safepoints))
  (define (visit-cont label cont safepoints)
    (match cont
      (($ $kargs names vars ($ $continue k src exp))
       (let ((safepoints (maybe-add-safepoint label k safepoints)))
         (if (match exp
               (($ $call) #t)
               (($ $callk) #t)
               (($ $values)
                (match (intmap-ref cps k)
                  (($ $ktail) #t)
                  (_ #f)))
               (_ #f))
             (intset-add! safepoints label)
             safepoints)))
      (($ $kargs names vars ($ $branch kf kt))
       (maybe-add-safepoint label kf
                            (maybe-add-safepoint label kt safepoints)))
      (_ safepoints)))
  (persistent-intset (intmap-fold visit-cont cps empty-intset)))

(define (add-handle-interrupts cps)
  (define (add-safepoint label cps)
    (match (intmap-ref cps label)
      (($ $kargs names vars term)
       (with-cps cps
         (letk k ($kargs () () ,term))
         (setk label
               ($kargs names vars
                 ($continue k #f
                   ($primcall 'handle-interrupts #f ()))))))))
  (let* ((cps (renumber cps))
         (safepoints (compute-safepoints cps)))
    (with-fresh-name-state cps
      (persistent-intmap (intset-fold add-safepoint safepoints cps)))))
