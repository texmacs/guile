;;; srfi-35.scm --- Conditions                 -*- coding: utf-8 -*-

;; Copyright (C) 2007-2011, 2017 Free Software Foundation, Inc.
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

;;; Author: Ludovic Courtès <ludo@gnu.org>

;;; Commentary:

;; This is an implementation of SRFI-35, "Conditions".  Conditions are a
;; means to convey information about exceptional conditions between parts of
;; a program.

;;; Code:

(define-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (make-condition-type condition-type?
            make-condition condition? condition-has-type? condition-ref
            make-compound-condition extract-condition
            define-condition-type condition
            &condition
            &message message-condition? condition-message
            &serious serious-condition?
            &error error?))

(cond-expand-provide (current-module) '(srfi-35))


;;;
;;; Condition types.
;;;

;; Like default-record-printer, but prefixed with "condition ":
;; #<condition TYPE FIELD: VALUE ...>.
(define (print-condition c p)
  (display "#<condition " p)
  (display (record-type-name (record-type-descriptor c)) p)
  (let loop ((fields (record-type-fields (record-type-descriptor c)))
             (off 0))
    (match fields
      (() (display ">" p))
      ((field . fields)
       (display " " p)
       (display field p)
       (display ": " p)
       (display (struct-ref c off) p)
       (loop fields (+ 1 off))))))

;; FIXME: Perhaps use a `define-record-type' which allows for parent types.
(define &condition
  (make-record-type '&condition '() print-condition #:extensible? #t))

(define (make-condition-type id parent field-names)
  "Return a new condition type named @var{id}, inheriting from
@var{parent}, and with the fields whose names are listed in
@var{field-names}.  @var{field-names} must be a list of symbols and must
not contain names already used by @var{parent} or one of its
supertypes."
  (unless (condition-type? parent)
    (error "parent is not a condition type" parent))
  (make-record-type id field-names print-condition #:parent parent
                    #:extensible? #t))

(define (condition-type? obj)
  "Return true if OBJ is a condition type."
  (and (record-type? obj)
       (record-type-has-parent? obj &condition)))

(define simple-condition?
  (record-predicate &condition))

;; Compound conditions are represented as a disjoint type, as users
;; never have access to compound condition types.
(define &compound-condition
  (make-record-type 'compound-condition '(conditions)))
(define compound-condition?
  (record-predicate &compound-condition))
(define %make-compound-condition
  (record-constructor &compound-condition))
(define compound-condition-conditions
  (record-accessor &compound-condition 'conditions))


;;;
;;; Conditions.
;;;

(define (condition? obj)
  "Return true if @var{obj} is a condition."
  (or (simple-condition? obj)
      (compound-condition? obj)))

(define (condition-has-type? c type)
  "Return true if condition C has type TYPE."
  (unless (condition-type? type)
    (scm-error 'wrong-type-arg "condition-has-type?" "Not a condition type: ~S"
               (list type) #f))
  (match c
    (($ &compound-condition conditions)
     (or-map (lambda (c) (condition-has-type? c type)) conditions))
    ((? simple-condition?)
     ((record-predicate type) c))
    (_
     (scm-error 'wrong-type-arg "condition-has-type?" "Not a condition: ~S"
                (list c) #f))))

;; Precondition: C is a simple condition.
(define (simple-condition-ref c field-name not-found)
  (match (list-index (record-type-fields (struct-vtable c)) field-name)
    (#f (not-found))
    (pos (struct-ref c pos))))

(define (condition-ref c field-name)
  "Return the value of the field named FIELD-NAME from condition C."
  (match c
    (($ &compound-condition conditions)
     (let lp ((conditions conditions))
       (match conditions
         (() (error "invalid field name" field-name))
         ((c . conditions)
          (simple-condition-ref c field-name (lambda () (lp conditions)))))))
    ((? simple-condition?)
     (simple-condition-ref c field-name
                           (lambda ()
                             (error "invalid field name" field-name))))
    (_
     (scm-error 'wrong-type-arg "condition-ref" "Not a condition: ~S"
                (list c) #f))))

(define (make-condition-from-values type values)
  (apply make-struct/simple type values))

(define (make-condition type . field+value)
  "Return a new condition of type TYPE with fields initialized as specified
by FIELD+VALUE, a sequence of field names (symbols) and values."
  (unless (condition-type? type)
    (scm-error 'wrong-type-arg "make-condition" "Not a condition type: ~S"
               (list type) #f))
  (let ((c (make-struct/no-tail type)))
    (let lp ((inits field+value) (fields (record-type-fields type)))
      (match inits
        (()
         (match fields
           (() c)
           ((field . fields)
            (error "field not specified" field))))
        (((and (? symbol?) field) value . inits)
         (unless (memq field fields)
           (error "unknown field, or duplicate initializer" field))
         ((record-modifier type field) c value)
         (lp inits (delq field fields)))
        (inits
         (scm-error 'wrong-type-arg "make-condition"
                    "Bad initializer list tail: ~S"
                    (list inits) #f))))))

(define (make-compound-condition . conditions)
  "Return a new compound condition composed of CONDITIONS."
  (%make-compound-condition
   (let lp ((conditions conditions))
     (if (null? conditions)
         '()
         (let ((c (car conditions))
               (conditions (cdr conditions)))
           (cond
            ((compound-condition? c)
             (append (compound-condition-conditions c) (lp conditions)))
            (else
             (unless (condition? c)
               (throw 'wrong-type-arg "make-compound-condition"
                      "Not a condition: ~S" c))
             (cons c (lp conditions)))))))))

(define (extract-condition c type)
  "Return a condition of condition type TYPE with the field values specified
by C."
  (unless (condition-type? type)
    (scm-error 'wrong-type-arg "extract-condition" "Not a condition type: ~S"
               (list type) #f))
  (match c
    (($ &compound-condition conditions)
     (or-map (lambda (c) (extract-condition c type))
             conditions))
    ((? simple-condition?)
     (and ((record-predicate type) c)
          c))
    (_
     (scm-error 'wrong-type-arg "extract-condition" "Not a condition: ~S"
                (list c) #f))))


;;;
;;; Syntax.
;;;

(define-syntax-rule (define-condition-type name parent pred (field-name field-accessor) ...)
  (begin
    (define name
      (make-condition-type 'name parent '(field-name ...)))
    (define (pred c)
      (condition-has-type? c name))
    (define (field-accessor c)
      (condition-ref c 'field-name))
    ...))

(define-syntax condition-instantiation
  ;; Build the `(make-condition type ...)' call.
  (syntax-rules ()
    ((_ type (out ...))
     (make-condition type out ...))
    ((_ type (out ...) (field-name field-value) rest ...)
     (condition-instantiation type (out ... 'field-name field-value) rest ...))))

(define-syntax condition
  (syntax-rules ()
    ((_ (type field ...))
     (condition-instantiation type () field ...))
    ((_ (type field ...) ...)
     (make-compound-condition (condition-instantiation type () field ...)
                              ...))))


;;;
;;; Standard condition types.
;;;

(define-condition-type &message &condition
  message-condition?
  (message condition-message))

(define-condition-type &serious &condition
  serious-condition?)

(define-condition-type &error &serious
  error?)

;;; srfi-35.scm ends here
