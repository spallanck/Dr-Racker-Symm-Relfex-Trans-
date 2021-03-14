;;Name: Sophie Pallanck
;;Date 11/7/20
;;Lab 5
;;Class: CSCI 301
#lang racket

;;This function takes a list
;;of pairs L and a list S and
;;interprets L as the relation
;;over S, returning true if
;;the relation is reflexive,
;;i.e. if for all elements of
;;S there is a pair (x, x) in L
;;and false otherwise
(define (Reflexive? L S)
  (reflexive  L L S S 1))

;;This function receives the list SL
;;that will be shortened as the function
;;runs, the same list L that will be kept
;;intact, the list S that will be shortened
;;as the function runs and the list SS
;;that will be kept intact, as well as a
;;value that will indicate if this is the first
;;running of the function. This function does most of the
;;work of determining if the relation L is reflexive
(define (reflexive SL L S SS first)
  (cond ((and (empty? SL) (empty? S))
         #t)
        ((and (empty? SL) (not (empty? S))) #f)       
        ((and (not (empty? SL)) (empty? S) (equal? 1 first)) #f)
        ((empty? S) #t)
        ((not (areMembers L SS)) #f)
        (else (if (Rhelper L (car S) SS) (reflexive (cdr SL) L (cdr S) SS 0)
                  #f))))

;;This function takes the full list of pairs
;;and the original list SS as parameters and makes
;;sure all elements in L are members of the list SS,
;;returning false if an element is found in L
;;that is not a part of SS, and true otherwise
(define (areMembers L SS)
  (cond ((empty? L) #t)
        ((not (member? (car (car L)) SS)) #f)
        ((not (member? (car (cdr (car L))) SS)) #f)
        (else (areMembers (cdr L) SS))))

;;This function takes the list of pairs, L
;;and the list SS as well as an element from S as parameters
;;and checks to see if this element x has a pair of
;;the form (x x) in L, returning true if a pair is found,
;;and false otherwise
(define (Rhelper L x SS)
  (cond ((empty? L) #f)
        ((and (equal? x (car (car L)))
              (equal? x (car (cdr (car L)))) #t))
        (else (Rhelper (cdr L) x SS))))

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

;;This function takes a list of
;;pairs, L, which is interpreted as
;;a binary relation,and returns true if the
;;relation is reflexive, i.e. for all element x, y
;; in L of the form (x, y) there is also a pair
;;of the form (y, x), and false otherwise
(define (Symmetric? L)
  (symmetric L L))

;;This function take the list of pairs
;;SL that will be shorted as this function
;;runs, and the list of pairs L that will be
;;kept intact as the function runs as parameters. This function
;;does most of the work for checking if the relation
;;over L is symmetric as described above
(define (symmetric SL L)
   (cond ((empty? SL) #t)
        (else (if (equal? (car (car SL)) (car (cdr (car SL)))) (symmetric (cdr SL) L) 
             (if (Shelper (car (car SL)) (car (cdr (car SL))) L)
                 (symmetric (cdr (cdr SL)) L) #f)))))

;;This function takes elements of a pair x and
;;y and the list of pairs L as paremeters and returns
;;true if another pair in the list is of the format
;;(y, x) and false otherwise
(define (Shelper x y L)
  (cond ((empty? L) #f)
        ((and (equal? x (car (cdr (car L))))
             (equal? y (car (car L)))
         #t))
        (else (Shelper x y (cdr L)))))

;;This function takes a list of
;;pairs L as a parameter, which is interpreted as
;;a binary relation, and returns true
;;if the list is transitive and false
;;otherwise. A list is transitive if
;;whenever there is a pair of the form
;;(x y) and (y z) then there is also a
;;pair of the form (x z)
(define (Transitive? L)
  (transitive L L))

;;This function takes a list of pairs SL
;;that will be shorted as the function runs,
;;and the list of pairs L that will remain intact,
;;as parameters.  This function
;;does most of the work for checking if the relation
;;over L is transitive as described above
(define (transitive SL L)
  (cond ((empty? SL) #t)
        (else (if (equal? (car (car SL)) (car (cdr (car SL))))
                  (transitive (cdr SL) L)
                  (if (Thelper1 (car (car SL)) (car (cdr (car SL))) L)
                      (if (Thelper2 (car (car SL)) (car (cdr (car SL))) L L)
                           (transitive (cdr SL) L)
                           #f)
                           (transitive (cdr SL) L))))))

;;This function takes elements
;;x and y from a pair in L, as well
;;as the full list of pairs L as
;;parameters and returns true if
;;element y is also the first element
;;of another pair in the L, and false
;;otherwise
(define (Thelper1 x y L)
  (cond ((empty? L) #f)
        ((equal? y (car (car L))))
        (else (Thelper1 x y (cdr L)))))

;;This function takes elements j and
;;k from a pair in L, as well as the
;;the list of pairs KL that will remain
;;intact while this function runs, and the
;;list of pairs L, as parameters and returns
;;true if there is a transitive pair found
;;and false otherwise
(define (Thelper2 j k KL L)
  (cond ((empty? L) #f)
        (else (if (equal? k (car (car L)))
            (Thelper3 j (car (cdr (car L))) KL)
            (Thelper2 j k KL (cdr L))
            ))))

;;This function takes elements x and z
;;and the list of pairs L as parameters
;;and returns true if a pair of the form
;;(x z) is found and false otherwise
(define (Thelper3 x z L)
  (cond ((empty? L) #f)
        ((and (equal? x (car (car L)))
              (equal? z (car (cdr (car L))))))
        (else (Thelper3 x z (cdr L)))))