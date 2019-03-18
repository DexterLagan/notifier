#lang racket/base

(provide *config-filename*
         *config-section-length*
         *default-email-from*
         *default-email-subject*
         *linefeed-delim*
         *program-config-error*
         *item-config-error*
         *default-hostname*
         *default-database*
         *default-username*
         *default-password*
         *name-prefix*     
         *email-prefix*    
         *keywords-prefix*
         *greeting-prefix*
         *footer-prefix*)

;;; constants

(define *config-filename* "notifier.conf")
(define *config-section-length* 6)

(define *default-email-from* "notifier@dexterphoto.com")
(define *default-email-subject* "Product Update")

(define *linefeed-delim* ";")

(define *program-config-error*
  (string-append "Invalid database configuration. Sample syntax:   \n"
                 "[Notifier]\n"
                 "hostname=\n"
                 "database=\n"
                 "username=\n"
                 "password=\n"
                 "query=SELECT produit_descript_designation FROM produit_descript "
                 "WHERE produit_descript_num = 1\n"))

(define *item-config-error*
  (string-append "Invalid item configuration. Sample item syntax:   \n"
                 "[Dexter]\n"
                 "name=Dexter"
                 "email=dexterlagan@gmail.com\n"
                 "keywords=Lenovo T420s, Lenovo X1 i7\n"
                 "greeting=Hi [name],\n\nthe following products have been found:"
                 "footer=Regards,\n\nNodixia Notifier\n"))

(define *default-hostname* "my-hostname.mydomain.com")
(define *default-database* "my-database")
(define *default-username* "username")
(define *default-password* "password")

(define *name-prefix*     "name=")
(define *email-prefix*    "email=")
(define *keywords-prefix* "keywords=")
(define *greeting-prefix* "greeting=")
(define *footer-prefix*   "footer=")


