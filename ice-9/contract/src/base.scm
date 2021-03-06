(define-module (ice-9 contract src base)
  #:use-module (ice-9 contract src guts)
  #:use-module (ice-9 contract src blame)
  #:use-module (ice-9 contract src arrow)

  #:use-module (compat racket misc)
  #:use-module (compat racket procedures)
  #:use-module (compat racket struct)

  #:export (contract
            recursive-contract
            current-contract-region))
#|
#lang racket/base

#|

improve method arity mismatch contract violation error messages?
  (abstract out -> and friends even more?)

|#



(provide contract
         recursive-contract
         current-contract-region)

(require (for-syntax racket/base syntax/name)
         racket/stxparam
         unstable/srcloc
         unstable/location
         "guts.rkt"
         "blame.rkt"
         "arrow.rkt")

|#

(define-syntax current-contract-region
  (λ (stx) #'(quote-module-path)))

(define-syntax contract 
  (lambda (stx)
  (syntax-case stx ()
    [(_ c v pos neg name loc)
     (syntax
       (apply-contract c v pos neg name loc (current-contract-region)))]
    [(_ c v pos neg)
     (with-syntax ([name (syntax-local-infer-name stx)])
       (syntax
        (apply-contract c v pos neg 'name
                        (build-source-location #f)
                        (current-contract-region))))]
    [(_ c v pos neg src)
     (raise-syntax-error 
      'contract
      (string-append
       "please update contract application to new protocol "
       "(either 4 or 6 arguments)"))])))

(define (apply-contract c v pos neg name loc usr)
  (let ([c (coerce-contract 'contract c)])
    #;(check-source-location! 'contract loc)
    (let ([new-val
           (((contract-projection c)
             (make-blame loc name (contract-name c) pos neg usr #t))
            v)])
      (if (and (not (parameter? new-val))  ;; when PR 11221 is fixed, 
                                           ;; remove this line
               (procedure? new-val)
               (object-name v)
               (not (eq? (object-name v) (object-name new-val))))
          (let ([vs-name (object-name v)])
            (cond
              [(contracted-function? new-val)
               ;; when PR11222 is fixed, change these things:
               ;;   - eliminate this cond case
               ;;   - remove the require of arrow.rkt above
               ;;   - change (struct-out contracted-function) 
               ;;     in arrow.rkt to make-contracted-function
               (make-contracted-function 
                (procedure-rename (contracted-function-proc new-val) vs-name)
                (contracted-function-ctc new-val))]
              [else
               (procedure-rename new-val vs-name)]))
          new-val))))

(define-syntax recursive-contract 
  (lambda (stx)
  (syntax-case stx ()
    [(_ arg type)
     (keyword? (syntax-e #'type))
     (with-syntax 
         ([maker
           (case (syntax-e #'type)
             [(#:impersonator) #'make-contract]
             [(#:chaperone)    #'make-chaperone-contract]
             [(#:flat)         #'make-flat-contract]
             [else 
              (raise-syntax-error 
               'recursive-contract
               "type must be one of #:impersonator, #:chaperone, or #:flat"
               #'type)])]

          [coerce
           (case (syntax-e #'type)
             [(#:impersonator) #'coerce-contract]
             [(#:chaperone)    #'coerce-chaperone-contract]
             [(#:flat)         #'coerce-flat-contract]
             [else 
              (raise-syntax-error 
               'recursive-contract
               "type must be one of #:impersonator, #:chaperone, or #:flat"
               #'type)])]

          [(type ...)
           (if (eq? (syntax-e #'type) '#:impersonator)
               null
               (list #'type))])

       (syntax
        (maker
         #:name '(recursive-contract arg type ...)
         #:first-order
         (λ (val)
            (let ([ctc (coerce 'recursive-contract arg)])
              (contract-first-order-passes? ctc val)))
         #:projection
         (λ (blame)
            (let ([ctc (coerce 'recursive-contract arg)])
              (let ([f (contract-projection ctc)])
                (λ (val)
                   ((f blame) val))))))))]
    [(_ arg)
     (syntax
      (recursive-contract arg #:impersonator))])))
