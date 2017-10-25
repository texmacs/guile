;;; Details on internal value representation.
;;; Copyright (C) 2014, 2015, 2017 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (system base types internal)
  #:export (;; Immediate tags.
            %tc2-inum
            %tc3-imm24
            %tc3-heap-object
            %tc8-char
            %tc8-flag
            %tc16-false
            %tc16-nil
            %tc16-eol
            %tc16-true
            %tc16-unspecified
            %tc16-undefined
            %tc16-eof

            ;; Heap object tags (cell types).
            %tc1-pair
            %tc3-struct
            %tc7-symbol
            %tc7-variable
            %tc7-vector
            %tc7-wvect
            %tc7-string
            %tc7-number
            %tc7-hashtable
            %tc7-pointer
            %tc7-fluid
            %tc7-stringbuf
            %tc7-dynamic-state
            %tc7-frame
            %tc7-keyword
            %tc7-syntax
            %tc7-program
            %tc7-vm-continuation
            %tc7-bytevector
            %tc7-weak-set
            %tc7-weak-table
            %tc7-array
            %tc7-bitvector
            %tc7-port
            %tc7-smob
            %tc16-bignum
            %tc16-real
            %tc16-complex
            %tc16-fraction))

;;; Commentary:
;;;
;;; Tag values used to represent Scheme values, internally to Guile.
;;;
;;; Code:


;;;
;;; Tags---keep in sync with libguile/tags.h!
;;;

;; Immediate tags.
(eval-when (expand load eval)
  (define %tc2-inum #b10)
  (define %tc3-imm24 #b100)
  (define %tc3-heap-object #b000)

  (define %tc8-flag (+ %tc3-imm24 0))
  (define %tc8-char (+ %tc3-imm24 8))

  (define %tc16-false       (+ (ash #b0000 8) %tc8-flag))
  (define %tc16-nil         (+ (ash #b0001 8) %tc8-flag))
  (define %tc16-eol         (+ (ash #b0011 8) %tc8-flag))
  (define %tc16-true        (+ (ash #b0100 8) %tc8-flag))
  (define %tc16-unspecified (+ (ash #b1000 8) %tc8-flag))
  (define %tc16-undefined   (+ (ash #b1001 8) %tc8-flag))
  (define %tc16-eof         (+ (ash #b1010 8) %tc8-flag)))

;; See discussion in tags.h and boolean.h.
(eval-when (expand)
  (let ()
    (define (exactly-one-bit-set? x)
      (and (not (zero? x)) (zero? (logand x (1- x)))))
    (define (exactly-two-bits-set? x)
      (exactly-one-bit-set? (logand x (1- x))))
    (define (bits-differ-in-exactly-one-bit-position? a b)
      (exactly-one-bit-set? (logxor a b)))
    (define (bits-differ-in-exactly-two-bit-positions? a b)
      (exactly-two-bits-set? (logxor a b)))

    (unless (bits-differ-in-exactly-one-bit-position? %tc16-eol %tc16-nil)
      (error "expected #nil and '() to differ in exactly one bit position"))
    (unless (bits-differ-in-exactly-one-bit-position? %tc16-false %tc16-nil)
      (error "expected #f and '() to differ in exactly one bit position"))
    (unless (bits-differ-in-exactly-two-bit-positions? %tc16-false %tc16-eol)
      (error "expected #f and '() to differ in exactly two bit positions"))))

;; Heap object tags (cell types).
(define %tc1-pair #b0)
(define %tc3-struct #x01)
(define %tc7-symbol #x05)
(define %tc7-variable #x07)
(define %tc7-vector #x0d)
(define %tc7-wvect #x0f)
(define %tc7-string #x15)
(define %tc7-number #x17)
(define %tc7-hashtable #x1d)
(define %tc7-pointer #x1f)
(define %tc7-fluid #x25)
(define %tc7-stringbuf #x27)
(define %tc7-dynamic-state #x2d)
(define %tc7-frame #x2f)
(define %tc7-keyword #x35)
(define %tc7-syntax #x3d)
(define %tc7-program #x45)
(define %tc7-vm-continuation #x47)
(define %tc7-bytevector #x4d)
(define %tc7-weak-set #x55)
(define %tc7-weak-table #x57)
(define %tc7-array #x5d)
(define %tc7-bitvector #x5f)
(define %tc7-port #x7d)
(define %tc7-smob #x77)

(define %tc16-bignum (+ %tc7-number (* 1 256)))
(define %tc16-real (+ %tc7-number (* 2 256)))
(define %tc16-complex (+ %tc7-number (* 3 256)))
(define %tc16-fraction (+ %tc7-number (* 4 256)))
