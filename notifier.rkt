#lang racket
(require db/base)
(require db/mysql)
(require "db.rkt")
(require "tools.rkt")
(require "consts.rkt")
(require "common.rkt")
(require "config.rkt")
(module+ test (require rackunit))

;;; Version history

; v1.0 - initial release.

;;; purpose

; to automatically send notification emails when products matching keywords appear
; in an SQL feed.

; 1) run a query that lists items, for example:
;    fetch product, designation, quantity, price
; 2) read the email addresses from a config file, along with keyword list string
; 3) for each email address in the mailing list,
;      fetch keyword list for the email address
;      generate a list of matching products from the stock list and the keywords
;      send an email to the address with the matching product lines

;;; future plans

; add support for concurrency
; production-grade

;;; defs

;; process a config section
(define (process-subscriber config dispo-products-str-lst)
  (define to        (get-config-value config *email-prefix*))
  (define keyw-str  (get-config-value config *keywords-prefix*))
  (define keywords  (string-split keyw-str ","))
  (define name      (get-config-value config *name-prefix*))
  (define greeting  (get-config-value config *greeting-prefix*))
  (define signature (get-config-value config *footer-prefix*))

  (define header    (string-split (string-replace greeting "[name]" name) *linefeed-delim*))
  (define footer    (string-split signature *linefeed-delim*))
  (define filtered-products      (filter* dispo-products-str-lst keywords))
  (define body      (list header filtered-products footer))

  (unless (null? filtered-products)
    (displayln (string-append "Sending email to " to "..."))
    (send-email to *default-email-from* *default-email-subject* (flatten body))))

;; return a list of matches given a list and a list of keywords
(define (filter* l keywords)
  (filter (λ (p) (string-contains?* p keywords)) l))

;; returns a list of product lines (strings) given a product query
;; query must return a list of #("refcom" "qty" "designation" "price")
(define (get-dispo-products query)
  (define dispo-products (query-rows db query))
  (map product-vec->product-str dispo-products))

;;; main

; read program and database configuration;
(define config-lst (load-config *config-filename* *config-section-length*))
(unless config-lst (die *program-config-error*))
(define db-config (first config-lst))
(define-values (database hostname username password)
  (get-db-settings db-config
                   *default-database* *default-hostname* *default-username* *default-password*))
; For future use: use a query from the config file
(define dispo-products-query (get-config-item-or-die db-config "query="))

(displayln "Connecting to database...")
(define db (connect-to-db hostname database username password))
(unless db (die "Unable to connect to database."))

(displayln "Querying database...")
(define dispo-products (get-dispo-products dispo-products-query))

(displayln "Processing configuration...")
(process-config-items process-subscriber
                      (rest config-lst)
                      dispo-products)


(displayln "Disconnecting from database...")
(disconnect db)

; EOF
