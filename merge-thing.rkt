#lang racket

;; My accidental bubble sort
(define (s-t a [b #f])
  (if (false? b)
      (s-t (first a) (second a))
      (if (< a b)
          (list a b)
          (list b a))))
(define (f-t x)
  (define ft (s-t (first x) (second x)))
  (if (= (length x) 2)
      ft
      (list
       (first ft)
       (f-t (flatten (list
                      (second ft)
                      (drop x 2)))))))
(define (b-s x [c 0])
  ;(display (format "~a. ~s~n" c x))
  (if (= (length x) c)
      (flatten x)
      (b-s (flatten (f-t x)) (+ c 1))))

;; Not used here, but I learned about append from it.
(define (by-x x [c 2])
  (if (= c 1)
      (map (lambda (y) (list y)) x)
      (if (<= (length x) c)
          (list x)
          (append
           (list (take x c))
           (by-x (drop x c))))))

;; My merge sort, courtesy of the Wikipedia page.
(define (m-s x)
  (define middle (ceiling (/ (length x) 2)))
  (if (<= (length x) 1)
      x
      (merge
       (m-s (take x middle))
       (m-s (drop x middle)))))
(define (merge l r [a '()])
  (cond
    [(and (> (length l) 0) (> (length r) 0))
     (if (< (first l) (first r))
         (merge (rest l) r (append a (list (first l))))
         (merge l (rest r) (append a (list (first r)))))]
    [(> (length l) 0) (merge (rest l) r (append a (list (first l))))]
    [(> (length r) 0) (merge l (rest r) (append a (list (first r))))]
    [else a]))
(define (sort-input)
  (display (format "Enter a list of numbers separated by spaces:~n"))
  (define x (read-line))
  (m-s (map (lambda (x) (string->number x)) (regexp-split #rx" +" x))))
(sort-input)