;;Name: Sophie Pallanck
;;Date 11/24/20
;;Lab 7
;;Class: CSCI 301
#lang racket

;;This function takes two bags as parameters
;;and returns a bag containing the elements
;;of the first bag minus the elements in the
;second bag 
(define (bag-difference bag1 bag2)
  (cond ((empty? bag2) bag1)
        ((empty? bag1) bag1)
        ((not (member? (car bag2) bag1))
         (bag-difference bag1 (cdr bag2)))
        (else (if (member? (car bag1) bag2) (bag-difference (remove (car bag1) bag1) (remove (car bag1) bag2))
                  (bag-difference (append (cdr bag1)(list (car bag1))) bag2)))))

;;This function takes parameters
;; x and L and checks if element x
;;is a member of list L, returning
;;true if x is a part of list L,
;;and false otherwise
(define (member? x L)
  (cond ((null? x) #f)
        ((null? L) #f)
        ((equal? x (car L)) #t)
        (else (member? x (cdr L)))))

;;This function takes two bags as parameters and
;;returns a bag containing the minimumm number of
;;elements contained in the two bags
(define (bag-union bag1 bag2)
  (append (bag-difference bag1 bag2) bag2))

;;This function takes two bags as parameters and
;;returns a bag containing the maximum number of
;;elements contained in the two bags
(define (bag-intersection bag1 bag2)
  (bag-difference bag1 (bag-difference bag1 bag2)))

(bag-difference '(a a b a) '(b a a)) 
(bag-difference '(a b a a) '(a b c)) 
(bag-difference '(a b c) '(a b a a)) 
(bag-difference '(a b c) '(a b c)) 
(bag-difference '() '(a b a a)) 
(bag-difference '(a b a a) '())
