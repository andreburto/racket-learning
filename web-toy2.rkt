#lang racket
(require racket/block)
(require "date-stuff.rkt")

(define (handle in out)
  ; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>" out)
  (display (format "<h2 style=\"text-align:center;\">~a</h2>" (days-left)) out)
  (display "</body></html>" out))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (block
  (define t (thread
             (lambda () 
               (handle in out)
               (close-input-port in)
               (close-output-port out))))
  (thread (lambda () (sleep 10) (kill-thread t)))))

(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
    (block (thread loop)))

;;(define stop (serve 4545))
(serve 4545)