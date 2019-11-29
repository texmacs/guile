;;; srfi-34.scm --- Exception handling for programs

;; Copyright (C) 2003, 2006, 2008, 2010, 2019 Free Software Foundation, Inc.
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

;;; Author: Neil Jerram <neil@ossau.uklinux.net>

;;; Commentary:

;; This is an implementation of SRFI-34: Exception Handling for
;; Programs.  For documentation please see the SRFI-34 document; this
;; module is not yet documented at all in the Guile manual.

;;; Code:

(define-module (srfi srfi-34)
  #:re-export (with-exception-handler
               (raise-exception . raise))
  #:re-export-and-replace ((raise-exception . raise))
  #:export-syntax (guard))

(cond-expand-provide (current-module) '(srfi-34))

(define-syntax guard
  (syntax-rules (else)
    "Syntax: (guard (<var> <clause1> <clause2> ...) <body>)
Each <clause> should have the same form as a `cond' clause.

Semantics: Evaluating a guard form evaluates <body> with an exception
handler that binds the raised object to <var> and within the scope of
that binding evaluates the clauses as if they were the clauses of a
cond expression.  That implicit cond expression is evaluated with the
continuation and dynamic environment of the guard expression.  If
every <clause>'s <test> evaluates to false and there is no else
clause, then raise is re-invoked on the raised object within the
dynamic environment of the original call to raise except that the
current exception handler is that of the guard expression."
    ((guard (var clause ... (else e e* ...)) body body* ...)
     (with-exception-handler
      (lambda (var)
        (cond clause ...
              (else e e* ...)))
      (lambda () body body* ...)
      #:unwind? #t))
    ((guard (var clause clause* ...) body body* ...)
     (let ((tag (make-prompt-tag)))
       (call-with-prompt
        tag
        (lambda ()
          (with-exception-handler
           (lambda (exn)
             (abort-to-prompt tag exn)
             (raise-exception exn))
           (lambda () body body* ...)))
        (lambda (rewind var)
          (cond clause clause* ...
                (else (rewind)))))))))


;;; (srfi srfi-34) ends here.
