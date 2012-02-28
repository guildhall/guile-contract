(define-module (ice-9 contract src blame)
  #:use-module (compat racket misc)
  #:use-module (compat racket struct)
  #:use-module (compat racket struct-def)
  #:use-module (ice-9 pretty-print)

  #:export (blame?
            make-blame
            blame-source
            blame-positive
            blame-negative
            blame-contract
            blame-value
            blame-original?
            blame-swapped?
            blame-swap
            blame-replace-negative ;; used for indy blame
            
            raise-blame-error
            current-blame-format))
#|
#lang racket/base

(require unstable/srcloc racket/pretty)

(provide blame?
         make-blame
         blame-source
         blame-positive
         blame-negative
         blame-contract
         blame-value
         blame-original?
         blame-swapped?
         blame-swap
         blame-replace-negative ;; used for indy blame

         raise-blame-error
         current-blame-format
         (struct-out exn:fail:contract:blame))
|#

(define (blame=? a b equal?/recur)
  (and (equal?/recur (blame-source a)    (blame-source b))
       (equal?/recur (blame-value a)     (blame-value b))
       (equal?/recur (blame-contract a)  (blame-contract b))
       (equal?/recur (blame-positive a)  (blame-positive b))
       (equal?/recur (blame-negative a)  (blame-negative b))
       (equal?/recur (blame-original? a) (blame-original? b))))

(define (blame-hash b hash/recur)
  (logxor (hash/recur (blame-source b))
          (hash/recur (blame-value b))
          (hash/recur (blame-contract b))
          (hash/recur (blame-positive b))
          (hash/recur (blame-negative b))
          (hash/recur (blame-original? b))))

;; prop:equal+hash is not supported by guile e.g. customize
;; equal and hash functions
(define-rstruct blame
  [source value contract positive negative user original?])
  ;; #:property prop:equal+hash
  ;; (list blame=? blame-hash blame-hash))



(define (blame-swap b)
  (make-blame
   (blame-source b)
   (blame-value b)
   (blame-contract b)
   (blame-negative b)
   (blame-positive b)
   (blame-user b)
   (not (blame-original? b))))

(define (blame-replace-negative b new-neg)
  (make-blame
   (blame-source b)
   (blame-value b)
   (blame-contract b)
   (blame-positive b)
   new-neg
   (blame-user b)
   (blame-original? b)))


(define (blame-swapped? b)
  (not (blame-original? b)))


(define-rstruct exn:fail:contract (a b))
(define-rstruct (exn:fail:contract:blame exn:fail:contract) [object])
(struct-out exn:fail:contract:blame)

(define (raise-blame-error b x fmt . args)
  (error ((current-blame-format) b x (apply format fmt args)))
  #;(error (apply format #f fmt args)))

(define (default-blame-format b x custom-message)
  (let* ([source-message 
          (regexp-replace 
           ": *$" (aif (it) (blame-source b) it "") "")]

         [positive-message (show/display (blame-positive b))]
         
         [contract-message 
          (format #f "  contract: ~a" (show/write 
                                       (blame-contract 
                                        b)   
                                        "            "))]

         [contract-message+at 
          (if (regexp-match "\n$" contract-message)
              (string-append contract-message
                             (if (string=? source-message "")
                                 ""
                                 (format #f "  at: ~a" source-message)))
              (string-append contract-message
                             "\n"
                             (if (string=? source-message "")
                                 ""
                                 (format 
                                  #f "        at: ~a" source-message))))]
                                                 
         [value-message (if (blame-value b)
                            (format #f " on ~a" (show/display (blame-value b)))
                            "")])
    ;; use (regexp-match #rx"\n" ...) to find out if show/display decided that 
    ;; this is a multiple-line message and adjust surrounding formatting 
    ;; accordingly

    (cond
      [(blame-original? b)
       (string-append
        (format #f "self-contract violation: ~a\n" custom-message)
        (format #f "  contract~a  from ~a~a blaming ~a~a" 
                value-message 
                positive-message
                (if (regexp-match "\n" positive-message)
                    " "
                    ",")
                positive-message
                (if (regexp-match "\n" positive-message)
                    ""
                    "\n"))
        contract-message+at)]

      [else
       (let ()
         (define negative-message (show/display (blame-negative b)))
         (define user-message
           (if (equal? (blame-positive b) (blame-user b))
               ""
               (format #f "  via ~a" (show/display (blame-user b)))))
         (string-append
          (format #f "contract violation: ~a\n" custom-message)
          (format #f "  contract~a  from ~a~a~a blaming ~a~a" 
                  value-message
                  negative-message 
                  user-message
                  (if (regexp-match "\n" negative-message)
                      " "
                      ",")
                  positive-message
                  (if (regexp-match "\n" positive-message)
                      ""
                      "\n"))
          contract-message+at))])))

(define (add-newline str)
  (if (regexp-match "\n$" str)
      str
      (string-append str "\n")))


(define (pretty-format/display v . l)
  (let ([port (open-output-string)])
    (pretty-print v port #:display? #t)
    (get-output-string port)))

(define pretty-format/write 
  (case-lambda 
    ((v)
     (let ([port (open-output-string)])
       (pretty-print v port)
       (get-output-string port)))

    ((v s)
     (let ([port (open-output-string)])
       (pretty-print v port #:per-line-prefix s)
       (skip s (get-output-string port))))))

(define show/display pretty-format/display)
(define show/write   pretty-format/write)

(define (show-line-break line port len cols)
  (newline port)
  (if line
    (begin (display "    " port) 4)
    0))

(define current-blame-format
  (make-parameter default-blame-format))

(define (skip s l)
  (let loop ((s (string->list s)) (l (string->list l)))
    (if (pair? s)
        (loop (cdr s) (cdr l))
        (list->string l))))
