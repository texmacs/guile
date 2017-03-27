;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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

(use-modules (language tree-il)
             (language tree-il primitives)
             (language tree-il canonicalize)
             (srfi srfi-1)
             (ice-9 control)
             (ice-9 pretty-print)
             (system syntax internal))

;; Minimize a syntax-object such that it can no longer be used as the
;; first argument to 'datum->syntax', but is otherwise equivalent.
(define (squeeze-syntax-object syn)
  (define (ensure-list x) (if (vector? x) (vector->list x) x))
  (let ((x    (syntax-expression syn))
        (wrap (syntax-wrap syn))
        (mod  (syntax-module syn)))
    (let ((marks (car wrap))
          (subst (cdr wrap)))
      (define (squeeze-wrap marks subst)
        (make-syntax x (cons marks subst) mod))
      (cond
       ((symbol? x)
        (let loop ((marks marks) (subst subst))
          (cond
           ((null? subst) (squeeze-wrap marks subst))
           ((eq? 'shift (car subst)) (loop (cdr marks) (cdr subst)))
           ((find (lambda (entry) (and (eq? x (car entry))
                                       (equal? marks (cadr entry))))
                  (apply map list (map ensure-list
                                       (cdr (vector->list (car subst))))))
            => (lambda (entry)
                 (squeeze-wrap marks
                               (list (list->vector
                                      (cons 'ribcage
                                            (map vector entry)))))))
           (else (loop marks (cdr subst))))))
       ((or (pair? x) (vector? x)) syn)
       (else x)))))

(define (squeeze-constant x)
  (cond ((syntax? x) (squeeze-syntax-object x))
        ((pair? x)
         (cons (squeeze-constant (car x))
               (squeeze-constant (cdr x))))
        ((vector? x)
         (list->vector (squeeze-constant (vector->list x))))
        (else x)))

(define (squeeze-tree-il x)
  (post-order (lambda (x)
                (if (const? x)
                    (make-const (const-src x)
                                (squeeze-constant (const-exp x)))
                    x))
              x))

(define (translate-literal-syntax-objects x)
  (define (find-make-syntax-lexical-binding x)
    (let/ec return
      (pre-order (lambda (x)
                   (when (let? x)
                     (for-each (lambda (name sym)
                                 (when (eq? name 'make-syntax)
                                   (return sym)))
                               (let-names x) (let-gensyms x)))
                   x)
                 x)
      #f))
  (let ((make-syntax-gensym (find-make-syntax-lexical-binding x))
        (retry-tag (make-prompt-tag)))
    (define (translate-constant x)
      (let ((src (const-src x))
            (exp (const-exp x)))
        (cond
         ((list? exp)
          (let ((exp (map (lambda (x)
                            (translate-constant (make-const src x)))
                          exp)))
            (if (and-map const? exp)
                x
                (make-primcall src 'list exp))))
         ((pair? exp)
          (let ((car (translate-constant (make-const src (car exp))))
                (cdr (translate-constant (make-const src (cdr exp)))))
            (if (and (const? car) (const? cdr))
                x
                (make-primcall src 'cons (list car cdr)))))
         ((vector? exp)
          (let ((exp (map (lambda (x)
                            (translate-constant (make-const src x)))
                          (vector->list exp))))
            (if (and-map const? exp)
                x
                (make-primcall src 'vector exp))))
         ((syntax? exp)
          (make-call src
                     (if make-syntax-gensym
                         (make-lexical-ref src 'make-syntax
                                           make-syntax-gensym)
                         (abort-to-prompt retry-tag))
                     (list
                      (translate-constant
                       (make-const src (syntax-expression exp)))
                      (translate-constant
                       (make-const src (syntax-wrap exp)))
                      (translate-constant
                       (make-const src (syntax-module exp))))))
         (else x))))
    (call-with-prompt retry-tag
      (lambda ()
        (post-order (lambda (x)
                      (if (const? x)
                          (translate-constant x)
                          x))
                    x))
      (lambda (k)
        ;; OK, we have a syntax object embedded in this code, but
        ;; make-syntax isn't lexically bound.  This is the case for the
        ;; top-level macro definitions in psyntax that follow the main
        ;; let blob.  Attach a lexical binding and retry.
        (unless (toplevel-define? x) (error "unexpected"))
        (translate-literal-syntax-objects
         (make-toplevel-define
          (toplevel-define-src x)
          (toplevel-define-name x)
          (make-let (toplevel-define-src x)
                    (list 'make-syntax)
                    (list (module-gensym))
                    (list (make-toplevel-ref #f 'make-syntax))
                    (toplevel-define-exp x))))))))

;; Avoid gratuitous churn in psyntax-pp.scm due to the marks and labels
;; changing session identifiers.
(set! syntax-session-id (lambda () "*"))

(let ((source (list-ref (command-line) 1))
      (target (list-ref (command-line) 2)))
  (let ((in (open-input-file source))
        (out (open-output-file (string-append target ".tmp"))))
    (write '(eval-when (compile) (set-current-module (resolve-module '(guile))))
           out)
    (newline out)
    (let loop ((x (read in)))
      (if (eof-object? x)
          (begin
            (close-port out)
            (close-port in))
          (begin
            (pretty-print (tree-il->scheme
                           (translate-literal-syntax-objects
                            (squeeze-tree-il
                             (canonicalize
                              (resolve-primitives
                               (macroexpand x 'c '(compile load eval))
                               (current-module)))))
                           (current-module)
                           (list #:avoid-lambda? #f
                                 #:use-case? #f
                                 #:strip-numeric-suffixes? #t
                                 #:use-derived-syntax?
                                 (and (pair? x)
                                      (eq? 'let (car x)))))
                          out #:width 120 #:max-expr-width 70)
            (newline out)
            (loop (read in))))))
  (system (format #f "mv -f ~s.tmp ~s" target target)))
