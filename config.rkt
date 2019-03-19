#lang racket
(require "common.rkt")

;;; version history

; v1.0 - initial release - support for processing config items

;;; purpose

; to provide an easy-to-use module for loading items from a standard configuration file
; sample main:

; read program and database configuration
;(define config-lst
;  (load-config *config-filename*
;               *config-section-length*))
;(unless config-lst
;  (die *program-config-error*))
;
;(define db-config
;  (first config-lst))
;
;(define-values (database hostname username password)
;  (get-db-settings db-config
;                   *default-database*
;                   *default-hostname*
;                   *default-username*
;                   *default-password*))
;
;(define product-search-query
;  (get-config-item-or-die db-config "query="))
;
;; unit test
;(module+ test
;  (displayln (string-append "Database Query:\n"
;                            product-search-query)))
;
;; for each config item, run the query and send an email if results found.
;(process-config-items process-subscriber
;                      (rest config-lst)
;                      product-search-query)

(provide load-config                         ; (load-config filename section-length)
         check-config                        ; (check-config section-count config-lines section-length)
         enumerate-config-sections           ; (enumerate-config-sections config-lines)
         get-config-list                     ; (get-config-list config-filename section-line-count config-error)
         get-config-value                    ; (get-config-value config prefix)
         get-config-item-or-die              ; (get-config-item-or-die config prefix)
         strip-config                        ; (strip-config str)
         get-program-config-value-or-default ; (get-launchpad-config-value-or-default config prefix default config-error)
         get-db-settings                     ; (get-db-settings program-config default-database default-hostname default-username default-password)
         process-config-items                ; (process-config-items proc config-lst optional-param-str)
         )

;;; constants

;; regex matches a [section in brackets]
(define *config-section-regex* #rx"^[]|[]")

; config file database line prefixes and config defaults
(define *hostname-prefix* "hostname=")
(define *database-prefix* "database=")
(define *username-prefix* "username=")
(define *password-prefix* "password=")

;;; defs

;; return a launchpad config valyue or use provided default if no value set
(define (get-program-config-value-or-default config prefix default)
  (let ((value (get-config-value config prefix)))
    (if (non-empty-string? value) value default)))

;; strip a string from [brackets] and double-quotes ""'s
(define (strip-config str)
  ((composex
    (string-replace x *config-section-regex* "") ; remove []'s
    (string-replace x "\"" ""))                ; remove double quotes
   str))

;; returns a list of config [section]'s from the given config-file
(define (enumerate-config-sections config-lines)
  (map strip-config (grep config-lines *config-section-regex*)))

;; generic config value extractor
;; config is a list of .conf file lines
;; config-error specifies the error message to display if valut not found
(define (get-config-value config prefix)
  (let ((value (get-unique-prefix-line config prefix)))
    (if value value #f)))

;; check config lines for the proper number of items
(define (check-config section-count config-lines section-length)
  (if (= (length config-lines) (* section-count section-length))
      #t
      #f)) ; force config file format to be divisible by the number of paths

;; returns true if string isn't empty or a comment (starts with '//')
(define (not-empty-or-comment? s)
  (and (non-empty-string? s)
       (not (string-prefix? s "//"))
       (not (string-prefix? s "#"))
       (not (string-prefix? s ";"))))

;; load config file and return a list of config sections in the following format:
; [section]
; path to binary folder
; path to licence.dat file
(define (get-config-list config-filename config-section-line-count)
  (let* ((config-lines       (file->lines config-filename))
         (trimmed-lines      (map string-trim config-lines))
         (clean-config-lines (filter not-empty-or-comment? trimmed-lines))
         (sections           (map strip-config (grep clean-config-lines *config-section-regex*)))
         (section-count      (length sections)))
    (if (= (length clean-config-lines) (* section-count config-section-line-count))
        (chop clean-config-lines config-section-line-count)
        #f)))

;; check config file exists and read, clean and check config file
(define (load-config filename section-length)
  (if (file-exists? filename)
      (get-config-list filename section-length)
      #f))

;; grab a database configuration from a config file
(define (get-db-settings program-config default-database
                         default-hostname
                         default-username
                         default-password)
  (let ((database
         (get-program-config-value-or-default program-config
                                              *database-prefix*
                                              default-database))
        (hostname
         (get-program-config-value-or-default program-config
                                              *hostname-prefix*
                                              default-hostname))
        (username
         (get-program-config-value-or-default program-config
                                              *username-prefix*
                                              default-username))
        (password
         (get-program-config-value-or-default program-config
                                              *password-prefix*
                                              default-password)))
    (values database hostname username password)))

;; process configs from the config file
(define (process-config-items proc config-lst [optional-param-str ""])
  (let ((results (andmap (λ (c) (proc c optional-param-str)) config-lst)))
    (if results (displayln "Configuration executed successfully.")
        (die (string-append "Error executing configuration: " (~a results))))))

;; returns a config item from a single config and a prefix
(define (get-config-item-or-die config prefix)
  (let ((value (get-config-value config prefix)))
    (if value value
        (die (string-append "Missing config item: " prefix "...")))))

; EOF