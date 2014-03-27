#lang racket
(require racket/include racket/gui/base racket/date)
(require "date-stuff.rkt")

(define frame
  (new frame%
       [label "Date GUI 2"]
       [width 500]
       [height 200]))

(define vpanel
  (new vertical-panel%
       [parent frame]
       [alignment '(center top)]))

(define top-canvas
  (new canvas%
       [parent vpanel]
       [min-width 500]
       [min-height 50]
       [paint-callback
        (lambda (canvas dc)
                (send dc set-scale 2 2)
                (send dc set-text-foreground "red")
                (send dc draw-text (days-left) 1 5))]))

(define hpanel1
  (new horizontal-panel%
       [parent frame]
       [alignment '(center bottom)]
       [min-height 50]
       [stretchable-height 50]))

(define bottom-left-canvas
  (new canvas%
       [parent hpanel1]
       [min-width 250]
       [min-height 50]
       [paint-callback
        (lambda (canvas dc)
          (send dc set-scale 2 2)
          (send dc set-text-foreground "blue")
          (send dc draw-text (format "Day ~a of ~a" (days-so-far) (days-in-year)) 1 5))]))

(define bottom-right-canvas
  (new canvas%
       [parent hpanel1]
       [min-width 250]
       [min-height 50]
       [paint-callback
        (lambda (canvas dc)
          (define is-now (current-date))
          (send dc set-scale 2 2)
          (send dc set-text-foreground "lime")
          (send dc draw-text
                (format "~a/~a/~a"
                        (date-month is-now)
                        (date-day is-now)
                        (date-year is-now))
                1
                5))]))


(send frame show #t)