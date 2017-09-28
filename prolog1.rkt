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
  (first clause))
(define (clause-body clause)
  (rest clause))

;; clauses are stored on the predicate's plist
(define (get-clauses pred)
  (hash-ref db-predicates pred))
(define (predicate relation)
  (first relation))

;; A list of all predicates stored in the database
(define db-predicates (make-hash))

;; Add a clause to the data base
(define-syntax-rule (<- fact1 ...)
  (add-clause (list 'fact1 ...)))

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

;; Return a 1ist of possible solutions to goal
(define (prove goal bindings)
  (my-flatten (filter (lambda (elem)
                        (not (null? elem)))
                      (map (lambda (clause)
                             (let ((new-clause (rename-variables clause)))
                               (prove-all (clause-body new-clause)
                                          (unify goal (clause-head new-clause) bindings))))
                           (get-clauses (predicate goal))))))

;; Return a list of solutions to the conjunction of goals
(define (prove-all goals bindings)
  (cond ((equal? bindings fail) fail)
        ((null? goals) bindings)
        (else (map (lambda (goal1-solution)
                     (prove-all (rest goals) goal1-solution))
                   (prove (first goals) bindings)))))

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
       (first tree)
       (unique-find-anywhere-if predicate (rest tree) found-so-far))
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
  (top-level-prove (list 'goal1 ...)))

;; Prove the goals, and print variables readably
(define (top-level-prove goals)
  (show-prolog-solutions
   (variables-in goals)
   (prove-all goals no-bindings)))

;; Print the variables in each of the solutions
(define (show-prolog-solutions vars solutions)
  (if (null? solutions)
      (format "~%No.")
      (map (lambda (solution) (show-prolog-vars vars solution))
           solutions))
  (values))

;; Print each variable with its binding
(define (show-prolog-vars vars bindings)
  (if (null? vars)
      (displayln "Yes")
      (for ((var vars))
                (displayln
                 (string-append
                  (symbol->string var) " = "
                  (symbol->string (subst-bindings bindings var)) ";")))))

(module+ test
  (check-equal? 'Lee (subst-bindings '((?who . Lee)) '?who))
  (check-equal? 'Kim (subst-bindings '((?who . Kim)) '?who))
  (check-equal? 'Robin (subst-bindings '((?x71156 . Robin) (?who . ?x71156)) '?who))
  (check-equal? 'Sandy (subst-bindings '((?x71160 . cats) (?x71157 . cats) (?x71156 . Sandy) (?who . ?x71156)) '?who))
  (check-equal? 'cats (subst-bindings '((?x71169 . cats) (?x71156 . ?x71169) (?who . ?x71156)) '?who))
  (check-equal? 'Sandy (subst-bindings '((?who . Sandy) (?x71171 . Sandy)) '?who)))

;; Return a list of all the variables in EXP
(define (variables-in exp)
  (unique-find-anywhere-if variable? exp))

