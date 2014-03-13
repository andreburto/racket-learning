#lang racket
(require racket/include)
(require racket/gui/base)
(require "date-stuff.rkt")

(define frame (new frame%
                   [label "Date GUI"]
                   [width 500]
                   [height 100]))

(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 2 2)
                (send dc set-text-foreground "red")
                (send dc draw-text (days-left) 1 5))])

(send frame show #t)