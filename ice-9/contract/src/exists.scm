(define-module (ice-9 contract src exists)
  #:use-module (ice-9 contract src guts)

  #:use-module (compat racket misc)
  #:use-module (compat racket struct)
  
  #:export (new-∃/c
                 new-∀/c
                 ∀∃?))
#|
#lang racket/base

(require "private/guts.rkt")

(provide new-∃/c
         new-∀/c
         ∀∃?)
|#

(define (∀∃-proj ctc)
  (let ([in (∀∃/c-in ctc)]
        [out (∀∃/c-out ctc)]
        [pred? (∀∃/c-pred? ctc)]
        [neg? (∀∃/c-neg? ctc)])
  (λ (blame)
    (if (eq? neg? (blame-swapped? blame))
        (λ (val)
          (if (pred? val)
              (out val)
              (raise-blame-error blame
                                 val 
                                 "non-polymorphic value: ~a"
                                 val)))
	in))))

(define-rstruct ∀∃/c (in out pred? name neg?)
  #:property prop:contract
  (build-contract-property
   #:name (λ (ctc) (∀∃/c-name ctc))
   #:first-order (λ (ctc) (λ (x) #t)) ;; ???
   #:projection ∀∃-proj))

(define-rstruct ∀∃ ())

(define (new-∃/c raw-name) (mk raw-name #t))
(define (new-∀/c raw-name) (mk raw-name #f))

(define (mk raw-name neg?)
  (define name (string->symbol (format #f "~a/~a" raw-name (if neg? "∃" "∀"))))
  (define-values (struct-type constructor predicate accessor mutator)
    (make-struct-type name ∀∃ 1 0))
  (make-∀∃/c constructor (λ (x) (accessor x 0)) predicate raw-name neg?))

