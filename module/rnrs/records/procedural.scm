;;; procedural.scm --- Procedural interface to R6RS records

;;      Copyright (C) 2010, 2017 Free Software Foundation, Inc.
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


(library (rnrs records procedural (6))
  (export make-record-type-descriptor 
          (rename (record-type? record-type-descriptor?))
	  make-record-constructor-descriptor
	  
	  record-constructor
	  record-predicate
	  record-accessor	  
	  record-mutator)
	  
  (import (rnrs base (6))
    (only (rename (guile)
                  (record-accessor guile:record-accessor))
          cons*
          logbit?

          when unless

          throw

	  struct-ref
	  struct-set!

          make-record-type
          record-type?
          record-type-name
          record-type-fields
          record-type-constructor
          record-type-mutable-fields
          record-type-parent
          record-type-opaque?
          record-predicate
          guile:record-accessor
          record-modifier

          vector->list))

  (define (make-record-type-descriptor name parent uid sealed? opaque? fields)
    (make-record-type name (vector->list fields) #:parent parent #:uid uid
                      #:extensible? (not sealed?)
                      #:opaque? (or opaque?
                                    (and parent (record-type-opaque? parent)))))

  (define record-constructor-descriptor
    (make-record-type 'record-constructor-descriptor
                      '((immutable rtd)
                        (immutable parent)
                        (immutable protocol))))
  (define rcd-rtd
    (guile:record-accessor record-constructor-descriptor 'rtd))
  (define rcd-parent
    (guile:record-accessor record-constructor-descriptor 'parent))
  (define rcd-protocol
    (guile:record-accessor record-constructor-descriptor 'protocol))

  (define (make-record-constructor-descriptor rtd parent-rcd protocol)
    (unless (record-type? rtd)
      (r6rs-raise (make-assertion-violation)))
    (when protocol
      (unless (procedure? protocol)
        (r6rs-raise (make-assertion-violation))))
    (when parent-rcd
      (unless (eq? (rcd-rtd parent-rcd)
                   (record-type-parent rtd))
        (when protocol
          (r6rs-raise (make-assertion-violation)))))
    ((record-type-constructor record-constructor-descriptor)
     rtd parent-rcd protocol))

  (define (record-constructor rcd)
    ;; The protocol facility allows users to define constructors whose
    ;; arguments don't directly correspond to the fields of the record
    ;; type; instead, the protocol managed a mapping from "args" to
    ;; "inits", where args are constructor args, and inits are the
    ;; resulting set of initial field values.
    (define-syntax if*
      (syntax-rules (=>)
        ((if* (exp => id) consequent alternate)
         (cond (exp => (lambda (id) consequent)) (else alternate)))))
    (define raw-constructor
      (record-type-constructor (rcd-rtd rcd)))
    (if* ((rcd-protocol rcd) => protocol)
         (protocol
          (if* ((rcd-parent rcd) => parent)
               (lambda parent-args
                 (lambda inits
                   (let collect-inits ((parent parent)
                                       (parent-args parent-args)
                                       (inits inits))
                     (apply
                      (if* ((and parent (rcd-protocol parent)) => protocol)
                           (protocol
                            (if* ((rcd-parent parent) => parent)
                                 ;; Parent has a protocol too; collect
                                 ;; inits from parent.
                                 (lambda parent-args
                                   (lambda parent-inits
                                     (collect-inits parent parent-args
                                                    (append parent-inits
                                                            inits))))
                                 ;; Default case: parent args correspond
                                 ;; to inits.
                                 (lambda parent-args
                                   (apply raw-constructor
                                          (append parent-args inits)))))
                           ;; Default case: parent args correspond to inits.
                           (lambda parent-args
                             (apply raw-constructor
                                    (append parent-args inits))))
                      parent-args))))
               raw-constructor))
         raw-constructor))
		    
  (define (record-accessor rtd k)
    (define pred (record-predicate rtd))
    
    (let* ((parent (record-type-parent rtd))
           (parent-nfields (if parent
                               (length (record-type-fields parent))
                               0))
           (k (+ k parent-nfields)))
      (unless (and (<= parent-nfields k)
                   (< k (length (record-type-fields rtd))))
        (r6rs-raise (make-assertion-violation)))
      (lambda (obj)
        (unless (pred obj)
          (r6rs-raise (make-assertion-violation)))
        (struct-ref obj k))))

  (define (record-mutator rtd k)
    (define pred (record-predicate rtd))
    (let* ((parent (record-type-parent rtd))
           (parent-nfields (if parent
                               (length (record-type-fields parent))
                               0))
           (k (+ k parent-nfields)))
      (unless (and (<= parent-nfields k)
                   (< k (length (record-type-fields rtd))))
        (r6rs-raise (make-assertion-violation)))
      (unless (logbit? k (record-type-mutable-fields rtd))
        (r6rs-raise (make-assertion-violation)))
      (lambda (obj val)
        (unless (pred obj)
          (r6rs-raise (make-assertion-violation)))
        (struct-set! obj k val))))

  ;; Condition types that are used in the current library.  These are defined
  ;; here and not in (rnrs conditions) to avoid a circular dependency.

  (define &condition (make-record-type-descriptor '&condition #f #f #f #f '#()))
  (define &condition-constructor-descriptor 
    (make-record-constructor-descriptor &condition #f #f))

  (define &serious (make-record-type-descriptor 
		    '&serious &condition #f #f #f '#()))
  (define &serious-constructor-descriptor
    (make-record-constructor-descriptor 
     &serious &condition-constructor-descriptor #f))

  (define make-serious-condition 
    (record-constructor &serious-constructor-descriptor))

  (define &violation (make-record-type-descriptor
		      '&violation &serious #f #f #f '#()))
  (define &violation-constructor-descriptor
    (make-record-constructor-descriptor 
     &violation &serious-constructor-descriptor #f))
  (define make-violation (record-constructor &violation-constructor-descriptor))

  (define &assertion (make-record-type-descriptor
		      '&assertion &violation #f #f #f '#()))
  (define make-assertion-violation 
    (record-constructor 
     (make-record-constructor-descriptor
      &assertion &violation-constructor-descriptor #f)))

  ;; Exception wrapper type, along with a wrapping `throw' implementation.
  ;; These are used in the current library, and so they are defined here and not
  ;; in (rnrs exceptions) to avoid a circular dependency.

  (define &raise-object-wrapper
    (make-record-type-descriptor '&raise-object-wrapper #f #f #f #f
				 '#((immutable obj) (immutable continuation))))
  (define make-raise-object-wrapper 
    (record-constructor (make-record-constructor-descriptor 
			 &raise-object-wrapper #f #f)))
  (define raise-object-wrapper? (record-predicate &raise-object-wrapper))
  (define raise-object-wrapper-obj (record-accessor &raise-object-wrapper 0))
  (define raise-object-wrapper-continuation 
    (record-accessor &raise-object-wrapper 1))

  (define (r6rs-raise obj) 
    (throw 'r6rs:exception (make-raise-object-wrapper obj #f)))
  (define (r6rs-raise-continuable obj)
    (define (r6rs-raise-continuable-internal continuation)
      (throw 'r6rs:exception (make-raise-object-wrapper obj continuation)))
    (call/cc r6rs-raise-continuable-internal))
  )
