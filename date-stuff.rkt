#lang racket
(require racket/date)
(require racket/block)
(provide (all-defined-out))

(define (is-leap? y)
  (cond
    [(= (modulo y 400) 0) #t]
    [(= (modulo y 100) 0) #f]
    [(= (modulo y 4) 0) #t]
    [else #f]))

(define months (list
                 (list 1 "January" 31)
                 (list 2 "February" (if (is-leap? (date-year (current-date))) 29 28))
                 (list 3 "March" 31)
                 (list 4 "April" 30)
                 (list 5 "May" 31)
                 (list 6 "June" 30)
                 (list 7 "July" 31)
                 (list 8 "August" 31)
                 (list 9 "September" 30)
                 (list 10 "October" 31)
                 (list 11 "November" 30)
                 (list 12 "December" 31)))

(define (show-days [x months])(map (lambda (m) (format "~a has ~a days this year." (list-ref m 1) (list-ref m 2))) x))

(define (get-month [mon (date-month (current-date))] [monthlist months]) (if (= mon (list-ref (first monthlist) 0))
                                                                             (first monthlist)
                                                                             (get-month mon (cdr monthlist))))

(define (what-month [mon (date-month (current-date))]) (list-ref (get-month mon months) 1))

(define (add-days [mon months]) (cond
                                  [(zero? (length mon)) 0]
                                  [(= (length mon) 1) (list-ref (first mon) 2)]
                                  [(> (length mon) 1) (+ (list-ref (first mon) 2) (add-days (cdr mon)))]))

(define (months-in-range [start 1] [stop 12]) (filter (lambda (mon) (if (and (<= start (list-ref mon 0)) (>= stop (list-ref mon 0))) #t #f)) months))

(define (days-in-range [start 1] [stop 12]) (add-days (months-in-range start stop)))

(define (days-left [mon (date-month (current-date))] [day 0]) ((lambda (now)
                                                                 (cond
                                                                   [(negative? (- mon (date-month now)))
                                                                    (format "That month is gone.")]
                                                                   [(zero? (- mon (date-month now)))
                                                                    (format "There are ~a days left this month." (- (list-ref (get-month mon) 2) (date-day now)))]
                                                                   [(positive? (- mon (date-month now)))
                                                                    (format "There are ~a days until ~a ~a."
                                                                            (+
                                                                             (- (list-ref (get-month (date-month now)) 2) (date-day now))
                                                                             (days-in-range (+ (date-month now) 1) (- mon 1))
                                                                             (- day 1))
                                                                            (list-ref (get-month mon) 1)
                                                                            (if (= day 0) "1" day))]))
                                                               (current-date)))

(define (show-days-this-month [mon (date-month (current-date))])
  (block
   (define one-month (car (show-days (months-in-range mon mon))))
   (display one-month)))
(define sdtm show-days-this-month)

(define (days-in-year [m months])
  (define (count-days l [d 0])
    (if (empty? l)
        d
        (count-days (cdr l) (+ (last (first l)) d))))
  (count-days m))

(define (days-so-far [m months])
  (define (count-days l [d 0])
    (if (= (first (first l)) (date-month (current-date)))
        (+ d (date-day (current-date)))
        (count-days (cdr l) (+ d (last (first l))))))
  (count-days m))