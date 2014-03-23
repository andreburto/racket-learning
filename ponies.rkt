#lang slideshow

(define (pony-slide-show)
  (slide
   #:title "Canary Belle"
   #:timeout 2
   (bitmap "canary_belle.png")
   (item "Based on: Lady Lawful"))
  (slide
   #:title "Spare Parts"
   #:timeout 2
   (bitmap "spare_parts.png")
   (item "Based on: Doctor Developr")))

(define (loop-slide-show t [c 0])
  (cond
    [(equal? t c) #f]
    [(pony-slide-show)
     (loop-slide-show t (+ c 1))]))

(loop-slide-show 2)