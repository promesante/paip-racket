;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prolog1.lisp: First version of the prolog interpreter (11.2).

#lang racket

(module+ test
  (require rackunit))

(provide adjoin
         binding-val
         extend-bindings
         fail
         get-binding
         lookup
         no-bindings
         reuse-cons
         sublis
         variable?)

;; Indicates pat-match failure
(define fail #f)

;; Indicates pat-match success, with no variables
(define no-bindings '((#t . #t)))

;; Get the value part of a single binding
(define (binding-val binding)
  (cdr binding))

;; Find a (variable . value) pair in a binding list
(define (get-binding var bindings)
  (assoc var bindings))

;; Get the value part (for var) from a binding list
(define (lookup var bindings)
  (binding-val (get-binding var bindings)))

;; Add a (var .value) pair to a binding list
(define (extend-bindings var val bindings)
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq? bindings no-bindings)
            null
            bindings)))

;; Is x a variable (a symbol beginning with '?')?
(define (variable? x)
  (and (symbol? x) (char=? (string-ref (symbol->string x) 0) #\?)))

;; Return (cons x y), or just x-y if it is equal to (cons x y)."
(define (reuse-cons x y x-y)
  (if (and (equal? x (car x-y)) (equal? y (cdr x-y)))
      x-y
      (cons x y)))

;; Racket implementation's specific functions

(define (sublis pairs lst)
  (map (lambda (elem)
         (process pairs elem))
       lst))

(module+ test
  (check-equal? '((likes ?x113409 ?x113409)) (sublis '((?x . ?x113409)) '((likes ?x ?x))))
  (check-equal? '((likes Sandy ?x113399) (likes ?x113399 cats)) (sublis '((?x . ?x113399)) '((likes Sandy ?x) (likes ?x cats))))
  (check-equal? '((likes Kim ?x113405) (likes ?x113405 Lee) (likes ?x113405 Kim)) (sublis '((?x . ?x113405)) '((likes Kim ?x) (likes ?x Lee) (likes ?x Kim)))))

(define (process pairs elem)
  (cond ((list? elem) (sublis pairs elem))
        ((pair? elem) (subpair pairs elem))
        (else (subelem pairs elem))))

(define (subpair pairs pair)
  (cons (process pairs (car pair)) (process pairs (cdr pair))))

(define (subelem pairs elem)
  (let ((mem-pairs (memf (lambda (pair)
                           (equal? elem (car pair)))
                         pairs)))
    (if (not mem-pairs)
        elem
        (cdr (car mem-pairs)))))

(define (adjoin item lst)
  (if (member item lst)
      lst
      (append lst (list item))))

(module+ test
  (check-equal? '(1 2 3) (adjoin 1 '(1 2 3)))
  (check-equal? '(1 2 3 4) (adjoin 4 '(1 2 3))))

