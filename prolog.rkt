;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prolog1.lisp: First version of the prolog interpreter (11.2).

#lang racket

(require "unify.rkt"
         "auxfns.rkt")

(module+ test
  (require rackunit))

;; Clauses are represented as (head . body) cons cells
(define (clause-head clause)
  (car clause))
(define (clause-body clause)
  (cdr clause))

;; clauses are stored on the predicate's plist
(define (get-clauses pred)
  (hash-ref db-predicates pred))
(define (predicate relation)
  (car relation))

;; A list of all predicates stored in the database
(define db-predicates (make-hash))
(hash-set! db-predicates 'show-prolog-vars null)

;; Add a clause to the data base
(define-syntax-rule (<- fact1 ...)
  (add-clause (replace-?-vars '(fact1 ...))))

;; Add a clause to the data base, indexed by head's predicate
(define (add-clause clause)
  (let ((pred (predicate (clause-head clause))))
    ;; The predicate must be a non-variable symbol
    (when (and (symbol? pred) (not (variable? pred)))
      (if (hash-has-key? db-predicates pred)
          (hash-update! db-predicates pred
                        (lambda (value)
                          (append value (list clause))))
          (hash-set! db-predicates pred (list clause))))
    pred))

;; Remove all clauses (for all predicates) from the data base
(define (clear-db)
  (hash-clear! db-predicates))

;; Remove the clauses for a single predicate
(define (clear-predicate predicate)
  (hash-remove! db-predicates predicate))

;; Replace all variables in x with new ones
(define (rename-variables x)
  (sublis (map (lambda (var) (cons var (gensym (symbol->string var))))
               (variables-in x))
          x))

;; Return a list of leaves of tree satisfying predicate, with duplicates removed
(define (unique-find-anywhere-if predicate tree (found-so-far '()))
  (if (cons? tree)
      (unique-find-anywhere-if
       predicate
       (car tree)
       (unique-find-anywhere-if predicate (cdr tree) found-so-far))
      (if (apply predicate (list tree))
          (adjoin tree found-so-far)
          found-so-far)))

(module+ test
  (check-equal? '(?who) (unique-find-anywhere-if variable? '((likes Sandy ?who))))
  (check-equal? null (unique-find-anywhere-if variable? '((likes Kim Robin))))
  (check-equal? '(?x) (unique-find-anywhere-if variable? '((likes Kim ?x) (likes ?x Lee) (likes ?x Kim))))
  (check-equal? '(?x) (unique-find-anywhere-if variable? '((likes ?x ?x))))
  (check-equal? '(?x) (unique-find-anywhere-if variable? '((likes Sandy ?x) (likes ?x cats)))))

(define-syntax-rule (?- goal1 ...)
  (top-level-prove (replace-?-vars '(goal1 ...))))

;; Return a list of solutions to the conjunction of goals
(define (prove-all goals bindings)
  (cond ((equal? bindings fail) fail)
        ((null? goals) (list bindings))
        (else (prove (car goals) bindings (cdr goals)))))

;; Return a 1ist of possible solutions to goal
(define (prove goal bindings other-goals)
  (let* ((pred (predicate goal))
         (clauses (get-clauses pred)))
    (if (null? clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (eval `(,pred ',(rest goal) ',bindings ',other-goals))
        (for/or ((clause clauses))
          (let ((new-clause (rename-variables clause)))
            (prove-all
             (append (clause-body new-clause) other-goals)
             (unify goal (clause-head new-clause) bindings)))))))

;; Prove the goals, and print variables readably
(define (top-level-prove goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
             no-bindings)
  (printf "No.")
  (values))

;; Print each variable with its binding
(define (show-prolog-vars vars bindings other-goals)
  (if (null? vars)
      (printf "Yes~%")
      (for ((var vars))
        (printf "~a = ~a~%"
                var
                (subst-bindings bindings var))))
  (if (continue?)
      fail
      (prove-all other-goals bindings)))

;; Ask user if we should continue looking for solutions
(define (continue?)
  (case (read-char)
    ((#\;) #t)
    ((#\.) #f)
    ((#\newline) (continue?))
    (else
     (printf "Type ; to see more or . to stop")
     (continue?))))

;; Return a list of all the variables in EXP
(define (variables-in exp)
  (unique-find-anywhere-if non-anon-variable? exp))

(define (non-anon-variable? x)
  (and (variable? x) (not (eq? x '?))))

;; Replace any ? within exp with a var of the form ?123
(define (replace-?-vars exp)
  (cond ((equal? exp '?) (gensym "?"))
        ((not (cons? exp)) exp)
        (else (reuse-cons (replace-?-vars (car exp))
                          (replace-?-vars (cdr exp))
                          exp))))
