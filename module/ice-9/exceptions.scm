;;; Exceptions
;;; Copyright (C) 2019 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Definition of the standard exception types.
;;;
;;; Code:


(define-module (ice-9 exceptions)
  #:export (&exception
            make-exception
            make-exception-type
            simple-exceptions
            exception?
            exception-type?
            exception-predicate
            exception-accessor
            define-exception-type

            &message
            make-exception-with-message
            exception-with-message?
            exception-message

            &warning
            make-warning
            warning?

            &error
            make-error
            error?

            &external-error
	    make-external-error
	    external-error?
	
            &programming-error
	    make-programming-error
	    programming-error?

	    &assertion-failure
	    make-assertion-failure
	    assertion-failure?

	    &irritants
	    make-exception-with-irritants
            exception-with-irritants?
	    exception-irritants

	    &origin
	    make-exception-with-origin
            exception-with-origin?
	    exception-origin

            &non-continuable
            make-non-continuable-error
            non-continuable-error?

            &implementation-restriction
            make-implementation-restriction-error
            implementation-restriction-error?

            &lexical
            make-lexical-error
            lexical-error?

            &syntax
            make-syntax-error
            syntax-error?
            syntax-error-form
            syntax-error-subform

            &undefined-variable
            make-undefined-variable-error
            undefined-variable-error?))

(define &exception (make-record-type '&exception '() #:extensible? #t))
(define simple-exception? (record-predicate &exception))

(define &compound-exception (make-record-type '&compound-exception
                                              '((immutable components))))
(define compound-exception? (record-predicate &compound-exception))
(define make-compound-exception (record-constructor &compound-exception))

(define simple-exceptions
  (let ((compound-ref (record-accessor &compound-exception 'components)))
    (lambda (exception)
      "Return a list of the simple exceptions that compose the exception
object @var{exception}."
      (cond ((compound-exception? exception)
             (compound-ref exception))
            ((simple-exception? exception)
             (list exception))
            (else
             (error "not a exception" exception))))))

(define make-exception
  (lambda exceptions
    (let ((simple
           (let flatten ((exceptions exceptions))
             (if (null? exceptions)
                 '()
                 (append (simple-exceptions (car exceptions))
                         (flatten (cdr exceptions)))))))
      (if (and (pair? simple) (null? (cdr simple)))
          (car simple)
          (make-compound-exception simple)))))

(define (exception? obj) 
  "Return true if @var{obj} is an exception."
  (or (compound-exception? obj) (simple-exception? obj)))

(define (exception-type? obj)
  "Return true if OBJ is an exception type."
  (and (record-type? obj)
       (record-type-has-parent? obj &exception)))

(define (make-exception-type id parent field-names)
  "Return a new exception type named @var{id}, inheriting from
@var{parent}, and with the fields whose names are listed in
@var{field-names}.  @var{field-names} must be a list of symbols and must
not contain names already used by @var{parent} or one of its
supertypes."
  (unless (exception-type? parent)
    (error "parent is not a exception type" parent))
  (unless (and-map symbol? field-names)
    (error "field names should be a list of symbols" field-names))
  (make-record-type id field-names #:parent parent #:extensible? #t))

(define (exception-predicate rtd)
  "Return a procedure that will return true if its argument is a simple
exception that is an instance of @var{rtd}, or a compound exception
composed of such an instance."
  (let ((rtd-predicate (record-predicate rtd)))
    (lambda (obj)
      (cond ((compound-exception? obj) 
	     (or-map rtd-predicate (simple-exceptions obj)))
	    (else (rtd-predicate obj))))))

(define (exception-accessor rtd proc)
  (let ((rtd-predicate (record-predicate rtd)))
    (lambda (obj)
      (if (rtd-predicate obj)
          (proc obj)
          (let lp ((exceptions (if (compound-exception? obj) 
                                   (simple-exceptions obj)
                                   '())))
            (when (null? exceptions)
              (error "object is not an exception of the right type" obj rtd))
            (if (rtd-predicate (car exceptions))
                (proc (car exceptions))
                (lp (cdr exceptions))))))))

(define-syntax define-exception-type
  (syntax-rules ()
    ((_ exception-type supertype constructor predicate
	(field accessor) ...)
     (begin
       (define exception-type
         (make-record-type 'exception-type '((immutable field) ...)
                           #:parent supertype #:extensible? #t))
       (define constructor (record-constructor exception-type))
       (define predicate (exception-predicate exception-type))
       (define accessor
         (exception-accessor exception-type
                             (record-accessor exception-type 'field)))
       ...))))

(define-exception-type &error &exception
  make-error error?)
(define-exception-type &programming-error &error
  make-programming-error programming-error?)
(define-exception-type &assertion-failure &programming-error
  make-assertion-failure assertion-failure?)

(define-exception-type &message &exception 
  make-exception-with-message exception-with-message? 
  (message exception-message))

(define-exception-type &warning &exception
  make-warning warning?)

(define-exception-type &external-error &error
  make-external-error external-error?)

(define-exception-type &irritants &exception
  make-exception-with-irritants exception-with-irritants?
  (irritants exception-irritants))

(define-exception-type &origin &exception
  make-exception-with-origin exception-with-origin?
  (origin exception-origin))

(define-exception-type &non-continuable &programming-error
  make-non-continuable-error
  non-continuable-error?)

(define-exception-type &implementation-restriction &programming-error
  make-implementation-restriction-error
  implementation-restriction-error?)

(define-exception-type &lexical &programming-error
  make-lexical-error lexical-error?)

(define-exception-type &syntax &programming-error
  make-syntax-error syntax-error?
  (form syntax-error-form)
  (subform syntax-error-subform))

(define-exception-type &undefined-variable &programming-error
  make-undefined-variable-error undefined-variable-error?)
