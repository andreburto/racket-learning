;; Working from: http://docs.racket-lang.org/more/index.html
#lang racket
(require racket/date)

(define whenisit (lambda () (define now (current-date)) (format "~s/~s/~s" (date-day now) (date-month now) (date-year now))))

(define (handle in out)
  (define counter 0)
  (define (loop say count)
    (display (format "~a " count) say)
    (if (= count 1000)
        (display "</body></html>" say)
        (loop say (+ 1 count))))
  ; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>" out)
  (loop out counter))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (define t (thread
             (lambda () 
               (handle in out)
               (close-input-port in)
               (close-output-port out))))
  (thread (lambda () (sleep 10) (kill-thread t))))

(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)
    (write "Gone")))

(define stop (serve 4545))

(define cnt 0)
(define (count-10 c)
  (display (format "~s~n" c))
  (if (= c 10)
      (display "")
      (count-10 (+ c 1))))
;;(count-10 cnt)