;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File unify.lisp: Unification functions

#lang racket

(require "auxfns.rkt")

(provide unify
         subst-bindings)

(module+ test
  (require rackunit))

(define occurs-check? #t)

;; Auxiliar function for unit test
(define (set-occurs-check value)
  (set! occurs-check? value))

;; See if x and y match with given bindings
(define (unify x y [bindings no-bindings])
  (cond ((eq? bindings fail) fail)
        ((equal? x y) bindings)
        ((variable? x) (unify-variable x y bindings))
        ((variable? y) (unify-variable y x bindings))
        ((and (cons? x) (cons? y))
         (unify (cdr x) (cdr y)
                (unify (car x) (car y) bindings)))
        (else fail)))

(module+ test
  (check-equal? '((?x . ?y)) (unify '(f ?x) '(f ?y)))
  (check-equal? '((?y . 0) (?x . ?y) (?a . ?x)) (unify '(?a + ?a = 0) '(?x + ?y = ?y)))
  (check-equal? '((?y . 1) (?x . 2)) (unify '(?x + 1) '(2 + ?y)))
  (check-equal? '((?x . ?y)) (unify '?x '?y))
  (check-equal? '((?x . ?y)) (unify '(?x ?x) '(?y ?y)))
  (check-equal? '((?x . ?y)) (unify '(?x ?y) '(?y ?x)))
  (check-equal? '((?y . a) (?x . ?y)) (unify '(?x ?y a) '(?y ?x ?x)))
  (check-equal? '((?y . a) (?x . ?y)) (unify '(?x ?y a) '(?y ?x ?x)))
  (check-equal? '((?y . a) (?x . ?y)) (unify '(?x ?y a) '(?y ?x ?x)))
  (check-equal? #f (unify '?x '(f ?x)))
  (check-equal? #f (unify '(?x ?y) '((f ?y) (f ?x))))
  (check-equal? #f (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))))
  (check-equal? '((#t . #t)) (unify 'a 'a)))

(module+ test
  (test-begin
   (set-occurs-check #f)
   (check-equal? '((?x f ?x)) (unify '?x '(f ?x)))
   (check-equal? '((?y f ?x) (?x f ?y)) (unify '(?x ?y) '((f ?y) (f ?x))))
   (check-equal? '((?z ?x ?y) (?y ?x ?z) (?x ?y ?z)) (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y))))
   (set-occurs-check #t)))

;; Unify var with x, using (and maybe extending) bindings
(define (unify-variable var x bindings)
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable? x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((and occurs-check? (occurs-check var x bindings))
         fail)
        (else (extend-bindings var x bindings))))

;; Does var occur anywhere 'inside x?
(define (occurs-check var x bindings)
  (cond ((equal? var x) #t)
        ((and (variable? x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((cons? x) (or (occurs-check var (car x) bindings)
                       (occurs-check var (cdr x) bindings)))
        (else #f)))

;;; ==============================

;; Substitute the value of variables in bindings into x, taking recursively bound variables into account
(define (subst-bindings bindings x)
  (cond ((equal? bindings fail) fail)
        ((equal? bindings no-bindings) x)
        ((and (variable? x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((not (cons? x)) x)
        (else (reuse-cons (subst-bindings bindings (car x))
                          (subst-bindings bindings (cdr x))
                          x))))

(module+ test
  (check-equal? 'Lee (subst-bindings '((?who . Lee)) '?who))
  (check-equal? 'Kim (subst-bindings '((?who . Kim)) '?who))
  (check-equal? 'Robin (subst-bindings '((?x71156 . Robin) (?who . ?x71156)) '?who))
  (check-equal? 'Sandy (subst-bindings '((?x71160 . cats) (?x71157 . cats) (?x71156 . Sandy) (?who . ?x71156)) '?who))
  (check-equal? 'cats (subst-bindings '((?x71169 . cats) (?x71156 . ?x71169) (?who . ?x71156)) '?who))
  (check-equal? 'Sandy (subst-bindings '((?who . Sandy) (?x71171 . Sandy)) '?who)))

;;; ==============================

;; Return something that unifies with both x and y (or fail)
(define (unifier x y)
  (subst-bindings (unify x y) x))

(module+ test
  (check-equal? '(0 + 0 = 0) (unifier '(?a + ?a = 0) '(?x + ?y = ?y)))
  (check-equal? '(2 + 2 = 2) (unifier '(?a + ?a = 2) '(?x + ?y = ?y)))
  (check-equal? '(a a a) (unifier '(?x ?y a) '(?y ?x ?x)))
  (check-equal? '((?a * 5 ^ 2) + (4 * 5) + 3) (unifier '((?a * ?x ^ 2) + (?b * ?x) + ?c)
                                                       '(?z + (4 * 5) + 3)))
  (check-equal? '(a a a) (unifier '(?x ?y a) '(?y ?x ?x)))
  (check-equal? '(a a a) (unifier '(?x ?y a) '(?y ?x ?x)))
  (check-equal? '(a a a) (unifier '(?x ?y a) '(?y ?x ?x))))
