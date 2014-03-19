#lang racket
(require racket/draw racket/file racket/math racket/class racket/gui/base)
(struct htg (home-dir parity))
(define setup (htg (current-directory) "parity")) ;; YOU NEED A DIRECTORY CALLED parity WITH IMAGES.
(define (list-files path) (filter (lambda (f) (file-exists? f) f) (directory-list (build-path (htg-home-dir setup) path))))
(define (list-parity) (map (lambda (f) (build-path (htg-home-dir setup) (htg-parity setup) f)) (list-files (htg-parity setup))))
(define pics-parity (map (lambda (p) (read-bitmap p)) (list-parity)))
(define (one-pic) (set! pics-parity (append (cdr pics-parity) (list (car pics-parity)))) (last pics-parity))

;; GUI
(define my-f% (class frame% (super-new) (define/augment (on-close) (kill-thread pt))))
(define f (new my-f% [label "Parity"] [alignment '(left top)]))
(define pv (new vertical-panel% [parent f] [alignment '(left top)]))
(define pic1 (new canvas% [parent pv] [min-height 500] [min-width 400] [paint-callback (lambda (canvas dc)
                                                                                         (send dc set-background "black")
                                                                                         (send dc clear)
                                                                                         (send dc draw-bitmap (one-pic) 0 0))]))
(send f show #t)

;; UPDATER
(define (update ch) (sleep 10) (send ch on-paint) (update ch))
(define pt (thread (lambda () (update pic1))))
