#lang racket
(require db)
(module+ test (require rackunit))
(provide connect-to-db           ; (connect-to-db hostname database username password)
         keywords->sql-like-lst) ; (keywords->sql-like-lst str))

;;; defs
         
;; connect to mysql database, returns a database object or #f if unsuccessful
(define (connect-to-db hostname database username password)
  (mysql-connect #:user username
                 #:database database
                 #:server hostname
                 #:password password))

;; returns a list of SQL-formatted LIKE keywords from the given keyword string list
;; i.e. "a,a b, c" -> '("*a*" "*a*b*" "*c*")
(define (keywords->sql-like-lst str)
  (let* ((lst (string-split str ","))
         (trimmed (map string-trim lst))
         (spaced (map (λ (s) (string-replace s " " "*"))
                      trimmed)))
    (map (λ (s) (string-append "*" s "*")) spaced)))
; unit text
(module+ test
  (check-equal? (keywords->sql-like-lst "a,a b, c")
                '("*a*" "*a*b*" "*c*")))

; EOF
