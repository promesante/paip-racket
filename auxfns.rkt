;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxfns.lisp: Auxiliary functions used by all other programs
;;; Load this file before running any other programs.

#lang racket

(module+ test
  (require rackunit))

(provide adjoin
         binding-val
         extend-bindings
         fail
         get-binding
         lookup
         my-flatten
         no-bindings
         reuse-cons
         sublis
         variable?)

;; Indicates pat-match failure
(define fail null)

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
        (if (equal? bindings no-bindings)
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

; Hack in order to flat as a list the result I get as a tree
(define (my-flatten lst)
  (if (null? lst)
      null
      (filter (lambda (elem) (not (null? elem)))
              (append
               (append-map my-flatten (filter list? lst))
               (list (filter (lambda (elem)
                               (and (not (list? elem)) (cons? elem)))
                             lst))))))

(module+ test
  (check-equal?
   '(((?x20483 . Robin) (?who . ?x20483))
     ((?x20493 . cats) (?x20487 . cats) (?x20483 . Sandy) (?who . ?x20483))
     ((?x20501 . cats) (?x20483 . ?x20501) (?who . ?x20483)))
   (my-flatten '(((?x20483 . Robin) (?who . ?x20483))
                 (((?x20493 . cats) (?x20487 . cats) (?x20483 . Sandy) (?who . ?x20483)))
                 ((?x20501 . cats) (?x20483 . ?x20501) (?who . ?x20483)))))
  (check-equal?
   '(((?who . Lee))
     ((?who . Kim))
     ((?x20483 . Robin) (?who . ?x20483))
     ((?x20493 . cats) (?x20487 . cats) (?x20483 . Sandy) (?who . ?x20483))
     ((?x20501 . cats) (?x20483 . ?x20501) (?who . ?x20483))
     ((?who . Sandy) (?x20503 . Sandy)))
   (my-flatten '(((?who . Lee))
                 ((?who . Kim))
                 (((?x20483 . Robin) (?who . ?x20483))
                  (((?x20493 . cats) (?x20487 . cats) (?x20483 . Sandy) (?who . ?x20483)))
                  ((?x20501 . cats) (?x20483 . ?x20501) (?who . ?x20483)))
                 ((?who . Sandy) (?x20503 . Sandy))))))

(define (sublis pairs lst)
  (map (lambda (elem)
         (if (list? elem)
             (sublis pairs elem)
             (let ((mem-pairs (memf (lambda (pair)
                                      (equal? elem (car pair)))
                                    pairs)))
               (if (not mem-pairs)
                   elem
                   (cdr (car mem-pairs))))))
         lst))

(module+ test
  (check-equal? '((likes ?x113409 ?x113409)) (sublis '((?x . ?x113409)) '((likes ?x ?x))))
  (check-equal? '((likes Sandy ?x113399) (likes ?x113399 cats)) (sublis '((?x . ?x113399)) '((likes Sandy ?x) (likes ?x cats))))
  (check-equal? '((likes Kim ?x113405) (likes ?x113405 Lee) (likes ?x113405 Kim)) (sublis '((?x . ?x113405)) '((likes Kim ?x) (likes ?x Lee) (likes ?x Kim)))))

(define (adjoin item lst)
  (if (member item lst)
      lst
      (append lst (list item))))

(module+ test
  (check-equal? '(1 2 3) (adjoin 1 '(1 2 3)))
  (check-equal? '(1 2 3 4) (adjoin 4 '(1 2 3))))

