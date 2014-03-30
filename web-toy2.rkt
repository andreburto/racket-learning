;; Working from: http://docs.racket-lang.org/more/index.html
#lang racket
(require xml net/url)
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
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda () 
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  (thread (lambda ()
              (sleep 10)
              (custodian-shutdown-all cust))))

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define stop (serve 4545))
;;(serve 4545)