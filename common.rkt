#lang racket
(module+ test (require rackunit))

(provide chop                        ; (chop l n)
         composex                    ; (composex stx) [MACRO]
         die                         ; (die msg)
         ;show-error-message         ; (show-error-message msg)
         execute-async               ; (execute-async startup-path program-binary-path parameters)
         get-unique-prefix-line      ; (get-unique-prefix-line lst prefix)
         grep                        ; (grep lines regex-pattern)
         grepl                       ; (grepl lines prefix)
         label->filename             ; (label->filename label ext)
         maybe-copy-file             ; (maybe-copy-file source destination error-message exists-ok?)
         define-command-line-params  ; (define-command-line-params program-name debug-switch)
         string-contains?*)          ; (string-contains?* str keywords)
         
;;; purpose

; to provide common functions and macros for general purpose

;;; defs

(define version                    "1.1")
(define program-name               "Notifier")
(define appname (string-append     program-name " v" version))

;; returns true if string contains at least one keyword from the list
(define (string-contains?* str keywords)
  (ormap (λ (k) (andmap (λ (sk) (string-contains? str sk)) (string-split k))) keywords))
; unit test
(module+ test
  (check-equal? (string-contains?* "... Lenovo Thinkpad T420 ..." '("Lenovo T420")) #t))

; Macro that defines whichever parameters are fed to it and fills them in from command line
(define-syntax define-command-line-params
  (syntax-rules ()
    ((define-command-line-params appname param1 ...)
     (define-values (param1 ...)
       (command-line #:program appname
                     #:args (param1 ...)
                     (values param1 ...))))))

;; macro that lets one compose functions with any number of parameters
;; each composed expression is essencially curried to accept one parameter 'x' through lambdas
;; the result of each composed expression (evaluated right-to-left) is passed on to the next with 'x'
(define-syntax (composex stx)
  (syntax-case stx ()
    ((_ f1 ...)
     (with-syntax ([x-var (datum->syntax stx 'x)]) ; create a local variable so Racket is happy
       #'(compose1 (λ (x-var) f1) ...)))))         ; move each composed expression inside a lambda
 
; unit test
(module+ test
  (require rackunit)
  (check-equal? ((composex (string-replace x " " "-")
                           (string-downcase x)
                           (string-trim x)) " Naice Day ")
                "naice-day"))

;; returns a chopped list of n lists
(define (chop l n)
  (if (null? l) null
      (cons (take l n) (chop (drop l n) n))))

;; displays an error
;(define (show-error-message msg)
;  (void (message-box appname (string-append msg "   ") #f (list 'ok 'caution))))

;; displays an error and quits
(define (die msg)
  (displayln msg)
  (exit 1))

;; grep using a regex
(define (grep lines regex-pattern)
  (filter (λ (line) (regexp-match? regex-pattern line)) lines))

;; grep using a prefix only
(define (grepl lines prefix)
  (filter (λ (line) (string-prefix? line prefix)) lines))

;; generate a filename from a title
;; i.e. The Lion Guard --> the-lion-guard
(define (label->filename label ext)
  (string-append
   (string-downcase
    (string-replace label " " "-"))
   ext))

;; returns the matching unique line starting with prefix
;; ensures a single element line exists in the list
;; ensures the prefix exists
;; returns false otherwise
(define (get-unique-prefix-line lst prefix)
  (if (not (list? lst)) #f
      (let ((bin-line (grepl lst prefix)))
        (if (and (list? bin-line)
                 (= (length bin-line) 1))
            (string-replace (first bin-line) prefix "")
            #f))))

;; attempts to cleanly copy a file with exception handling
;; displays an error if copy fails
(define (maybe-copy-file source destination error-message exists-ok?)
  (when (and (non-empty-string? source) (file-exists? source))
    (with-handlers ([exn:fail:filesystem? (λ (e) (displayln error-message))])
      (when (and exists-ok? (file-exists? destination)) (delete-file destination)) ; Racket bugfix
      (copy-file source destination exists-ok?))))

;; launches a program in a cross-platform way
(define (execute-async startup-path program-binary-path command-line-parameters)
  (if (and (non-empty-string? startup-path)
           (non-empty-string? program-binary-path)
           (file-exists? program-binary-path))
      (if (equal? (system-type 'os) 'windows)
          (shell-execute #f
                         program-binary-path
                         command-line-parameters
                         startup-path
                         'sw_shownormal) ; possible values: 'sw_shownormal 'sw_hide 'sw_minimize
          (process program-binary-path))
      (displayln "This version of Harmony is not installed.")))

; EOF