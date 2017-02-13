;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 1999, 2001, 2004, 2006, 2017 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (ice-9 arrays)
  #:export (array-copy))

; This is actually defined in boot-9.scm, apparently for b.c.
;; (define (array-shape a)
;;   (map (lambda (ind) (if (number? ind) (list 0 (+ -1 ind)) ind))
;;        (array-dimensions a)))

; FIXME writes over the array twice if (array-type) is #t
(define (array-copy a)
  (let ((b (apply make-typed-array (array-type a) *unspecified* (array-shape a))))
    (array-copy! a b)
    b))

