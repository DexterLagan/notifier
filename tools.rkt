#lang racket
(require "config.rkt")
(require net/sendmail)

(provide specs->product-str
         product-vec->product-str
         send-email)

; unit test
(define sample-query-results
  '(#("refcom1" "designation1" 100 1)
    #("refcom2" "designation2" 200 2)
    #("refcom3" "designation3" 300 3)))

;; build a product line from the given data
(define (specs->product-str refcom design price qty)
  (string-append (number->string qty) " x " design " (" refcom ") - " (number->string price) " Euros HT"))
; unit test
(module+ test
  (displayln
   (string-append
    "\nProduct Designation Generated:\n"
    (specs->product-str "RPLNMS-12920" "Lenovo Thinkpad T420s" 399 3)
    "\n")))

;; send a pre-formatted email using defaults
(define (send-email to from subject body)
  (define dest       (list to))
  (send-mail-message from subject dest null null body))

;; helper function returns a product string from product vector line from query
(define (product-vec->product-str v)
  ;(specs->product-str refcom qty design price)
  (specs->product-str (vector-ref v 0)
                      (vector-ref v 1)
                      (vector-ref v 2)
                      (vector-ref v 3)))
; unit test
(module+ test
  (for-each displayln (map product-vec->product-str sample-query-results)))

