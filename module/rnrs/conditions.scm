;;; conditions.scm --- The R6RS conditions library

;;      Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA


(library (rnrs conditions (6))
  (export &condition
	  condition
	  simple-conditions
	  condition?
	  condition-predicate
	  condition-accessor
	  define-condition-type
	  
	  &message
	  make-message-condition
	  message-condition?
	  condition-message

	  &warning
	  make-warning
	  warning?

	  &serious
	  make-serious-condition
	  serious-condition?

	  &error
	  make-error
	  error?

	  &violation
	  make-violation
	  violation?

	  &assertion
	  make-assertion-violation
	  assertion-violation?

	  &irritants
	  make-irritants-condition
	  irritants-condition?
	  condition-irritants

	  &who
	  make-who-condition
	  who-condition?
	  condition-who

	  &non-continuable
	  make-non-continuable-violation
	  non-continuable-violation?

	  &implementation-restriction
	  make-implementation-restriction-violation
	  implementation-restriction-violation?

	  &lexical
	  make-lexical-violation
	  lexical-violation?

	  &syntax
	  make-syntax-violation
	  syntax-violation?
	  syntax-violation-form
	  syntax-violation-subform

	  &undefined
	  make-undefined-violation
	  undefined-violation?)
  (import (only (guile)
                and=>
                make-record-type
                record-constructor
                record-predicate
                record-accessor)
	  (rnrs base (6))
	  (rnrs lists (6)))

  (define &condition (make-record-type '&condition '() #:extensible? #t))
  (define simple-condition? (record-predicate &condition))

  (define &compound-condition (make-record-type '&compound-condition
                                                '((immutable components))))
  (define compound-condition? (record-predicate &compound-condition))
  (define make-compound-condition (record-constructor &compound-condition))

  (define simple-conditions
    (let ((compound-ref (record-accessor &compound-condition 'components)))
      (lambda (condition)
        (cond ((compound-condition? condition)
               (compound-ref condition))
              ((simple-condition? condition)
               (list condition))
              (else
               (assertion-violation 'simple-conditions
                                    "not a condition"
                                    condition))))))

  (define (condition? obj) 
    (or (compound-condition? obj) (simple-condition? obj)))

  (define condition
    (lambda conditions
      (define (flatten cond)
	(if (compound-condition? cond) (simple-conditions cond) (list cond)))
      (or (for-all condition? conditions)
	  (assertion-violation 'condition "non-condition argument" conditions))
      (if (or (null? conditions) (> (length conditions) 1))
	  (make-compound-condition (apply append (map flatten conditions)))
	  (car conditions))))
  
  (define (condition-predicate rtd)
    (let ((rtd-predicate (record-predicate rtd)))
      (lambda (obj)
	(cond ((compound-condition? obj) 
	       (exists rtd-predicate (simple-conditions obj)))
	      ((simple-condition? obj) (rtd-predicate obj))
	      (else #f)))))

  (define (condition-accessor rtd proc)
    (let ((rtd-predicate (record-predicate rtd)))
      (lambda (obj)
	(cond ((rtd-predicate obj) (proc obj))
	      ((compound-condition? obj) 
	       (and=> (find rtd-predicate (simple-conditions obj)) proc))
	      (else #f)))))

  (define-syntax define-condition-type
    (syntax-rules ()
      ((_ condition-type supertype constructor predicate
	  (field accessor) ...)
       (begin
         (define condition-type
           (make-record-type 'condition-type '((immutable field) ...)
                             #:parent supertype #:extensible? #t))
         (define constructor (record-constructor condition-type))
         (define predicate (condition-predicate condition-type))
         (define accessor
           (condition-accessor condition-type
                               (record-accessor condition-type 'field)))
         ...))))

  (define-condition-type &serious &condition
    make-serious-condition serious-condition?)
  (define-condition-type &violation &serious
    make-violation violation?)
  (define-condition-type &assertion &violation
    make-assertion-violation assertion-violation?)

  (define-condition-type &message &condition 
    make-message-condition message-condition? 
    (message condition-message))

  (define-condition-type &warning &condition
    make-warning warning?)

  (define-condition-type &error &serious
    make-error error?)

  (define-condition-type &irritants &condition 
    make-irritants-condition irritants-condition?
    (irritants condition-irritants))

  (define-condition-type &who &condition
    make-who-condition who-condition?
    (who condition-who))

  (define-condition-type &non-continuable &violation
    make-non-continuable-violation
    non-continuable-violation?)

  (define-condition-type &implementation-restriction &violation
    make-implementation-restriction-violation
    implementation-restriction-violation?)

  (define-condition-type &lexical &violation
    make-lexical-violation lexical-violation?)

  (define-condition-type &syntax &violation
    make-syntax-violation syntax-violation?
    (form syntax-violation-form)
    (subform syntax-violation-subform))

  (define-condition-type &undefined &violation
    make-undefined-violation undefined-violation?))
