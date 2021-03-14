#lang racket

(define (bag-difference bag1 bag2)
  (bulk bag1 bag2 bag1 bag2 bag1))

(define (bulk bag1 bag2 c1 c2 R)
  (cond ((empty? bag2) bag1)
        ((empty? bag1) bag1)
        ((not (member? (car bag2) bag1))
         (bulk bag1 (cdr bag2) c1 c2 R))
        ( else (if (member? (car bag1) bag2) (bulk (remove (car bag1) bag1) (remove (car bag1) bag2) c1 c2 R) (bulk (append (cdr bag1)(list (car bag1))) bag2 c1 c2 R )))))
        


(define (member? x L)
  (cond ((null? x) #f)
        ((null? L) #f)
        ((equal? x (car L)) #t)
        (else (member? x (cdr L)))))

(define (bag-union bag1 bag2)
  (append (bag-difference bag1 bag2) bag2))

(define (bag-intersection bag1 bag2)
 ;; (bag1 - (bag-difference bag1 bag2)))
  (bag-difference bag1 (bag-difference bag1 bag2)))

(bag-intersection '(a b a a) '(a b c))

#|(define (bag-union bag1 bag2)
  (cond ((and (empty? bag1) (empty? bag2)) '())
        ((empty? bag1) bag2)
        ((empty? bag2) bag1)
        (else (helper bag1 bag2 '()))))



(define (helper bag1 bag2 R)
  (cond ((and (empty? bag1) (empty? bag2)) R)
        ((empty? bag1) (append bag2 R))
        ((empty? bag2) (append bag1 R))
        ((> (counter (car bag1) bag1 0) (counter (car  bag1) bag2 0))
         (helper (remove-occurrences (car bag1) bag1 (length bag1))
                 (remove-occurrences (car bag1) bag2 (length bag2)) (addTo (car bag1) (counter (car bag1) bag1 0) R)))
        ((> (counter (car bag1) bag2 0) (counter (car bag1) bag1 0))
         (helper (remove-occurrences (car bag1) bag1 (length bag1)) (remove-occurrences (car bag1) bag2 (length bag2))
                 (addTo (car bag1) (counter (car bag1) bag2 0) R)))
        ((> (counter (car bag2) bag1 0) (counter (car  bag2) bag2 0))
         (helper (remove-occurrences (car bag2) bag1 (length bag1))
                 (remove-occurrences (car bag2) bag2 (length bag2)) (addTo (car bag2) (counter (car bag2) bag1 0) R)))
        (else (helper (remove-occurrences (car bag2) bag1 (length bag1)) (remove-occurrences (car bag2) bag2 (length bag2))
                 (addTo (car bag2) (counter (car bag2) bag2 0) R)))))

;;(> (counter (car bag2) bag2 0) (counter (car bag2) bag1 0))

(define (addTo x num bag)
  (cond ((equal? num 0) bag)
        (else (addTo x (- num 1) (cons x bag)))))

(define (counter x bag total)
  (cond ((empty? bag) total)
        ((equal? x (car bag))
         (counter x (cdr bag) (+ 1 total)))
        (else (counter x (cdr bag) total))))

(define (remove-occurrences x bag count)
  (cond ((equal? count 0) bag)
        ((equal? x (car bag))
         (remove-occurrences x (cdr bag) (- count 1)))
        (else (cons (car bag) (remove-occurrences x (cdr bag) (- count 1))))))

(define (bag-intersection bag1 bag2)
         (cond ((and (empty? bag1) (empty? bag2)) '())
        ((empty? bag1) '())
        ((empty? bag2) '())
        (else (helper2 bag1 bag2 '()))))

(define (helper2  bag1 bag2 R)
  (cond ((and (empty? bag1) (empty? bag2)) R)
        ((empty? bag1) R)
        ((empty? bag2) R)
        ((< (counter (car bag1) bag1 0) (counter (car  bag1) bag2 0))
         (helper2 (remove-occurrences (car bag1) bag1 (length bag1))
                 (remove-occurrences (car bag1) bag2 (length bag2)) (addTo (car bag1) (counter (car bag1) bag1 0) R)))
        ((< (counter (car bag1) bag2 0) (counter (car bag1) bag1 0))
         (helper2 (remove-occurrences (car bag1) bag1 (length bag1)) (remove-occurrences (car bag1) bag2 (length bag2))
                 (addTo (car bag1) (counter (car bag1) bag2 0) R)))
        ((< (counter (car bag2) bag1 0) (counter (car  bag2) bag2 0))
         (helper2 (remove-occurrences (car bag2) bag1 (length bag1))
                 (remove-occurrences (car bag2) bag2 (length bag2)) (addTo (car bag2) (counter (car bag2) bag1 0) R)))
        (else (helper2 (remove-occurrences (car bag2) bag1 (length bag1)) (remove-occurrences (car bag2) bag2 (length bag2))
                 (addTo (car bag2) (counter (car bag2) bag2 0) R)))))

(bag-union '(a a b a) '(b a a)) |#
