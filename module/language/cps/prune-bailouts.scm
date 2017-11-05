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
;;; A pass that prunes successors of expressions that bail out.
;;;
;;; Code:

(define-module (language cps prune-bailouts)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:export (prune-bailouts))

(define (compute-tails conts)
  "For each LABEL->CONT entry in the intmap CONTS, compute a
LABEL->TAIL-LABEL indicating the tail continuation of each expression's
containing function.  In some cases TAIL-LABEL might not be available,
for example if there is a stale $kfun pointing at a body, or for
unreferenced terms.  In that case TAIL-LABEL is either absent or #f."
  (intmap-fold
   (lambda (label cont out)
     (match cont
       (($ $kfun src meta self tail clause)
        (intset-fold (lambda (label out)
                       (intmap-add out label tail (lambda (old new) #f)))
                     (compute-function-body conts label)
                     out))
       (_ out)))
   conts
   empty-intmap))

(define (prune-bailouts conts)
  (let ((tails (compute-tails conts)))
    (with-fresh-name-state conts
      (persistent-intmap
       (intmap-fold
        (lambda (label cont out)
          (match cont
            (($ $kargs names vars
                ($ $continue k src
                   (and exp ($ $primcall
                               (or 'throw 'throw/value 'throw/value+data)))))
             (match (intmap-ref tails k (lambda (_) #f))
               (#f out)
               (ktail
                (with-cps out
                  (letk knil ($kargs () ()
                               ($continue ktail src ($values ()))))
                  (setk label ($kargs names vars
                                ($continue knil src ,exp)))))))
            (_ out)))
        conts
        conts)))))
