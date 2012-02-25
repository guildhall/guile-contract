(define-module (ice-9 contract src legacy)
  #:use-module (ice-9 contract src guts)
  #:use-module (ice-9 contract src blame)

  #:use-module (compat racket misc)
  #:use-module (compat racket struct)

  #:export (make-proj-contract
            raise-contract-error
            contract-proc

            proj-prop proj-get proj-pred?
            name-prop name-get name-pred?
            stronger-prop stronger-get stronger-pred?
            first-order-prop first-order-get first-order-pred?
            flat-prop flat-get flat-pred?))

#|
#lang racket/base

(require "guts.rkt" "blame.rkt" unstable/srcloc)

(provide make-proj-contract
         raise-contract-error
         contract-proc

         proj-prop proj-get proj-pred?
         name-prop name-get name-pred?
         stronger-prop stronger-get stronger-pred?
         first-order-prop first-order-get first-order-pred?
         flat-prop flat-get flat-pred?

         )
|#

(define (raise-contract-error x src pos name fmt . args)
  (apply raise-blame-error
         (make-blame (unpack-source src)
                     (unpack-name src)
                     name
                     (unpack-blame pos)
                     "<<unknown party>>"
                     #t
                     "<<unknown party>>")
         x
         fmt
         args))

(define (make-proj-contract name proj test)
  (make-contract
   #:name name
   #:first-order test
   #:projection
   (cond
    [(procedure-arity-includes? proj 5)
     (lambda (blame)
       (proj (blame-positive blame)
             (blame-negative blame)
             (list (blame-source blame) (blame-value blame))
             (blame-contract blame)
             (not (blame-swapped? blame))))]
    [(procedure-arity-includes? proj 4)
     (lambda (blame)
       (proj (blame-positive blame)
             (blame-negative blame)
             (list (blame-source blame) (blame-value blame))
             (blame-contract blame)))]
    [else
     (rerror 'make-proj-contract
            "expected a projection that accepts 4 or 5 arguments; got: ~a"
            proj)])))

(define (contract-proc c)
  (let* ([proj (contract-projection c)])
    (lambda* (pos neg src name #:optional [original? #t])
      (proj (make-blame (unpack-source src)
                        (unpack-name src)
                        name
                        (unpack-blame (if original? pos neg))
                        (unpack-blame (if original? neg pos))
                        original? 
                        (unpack-blame (if original? neg pos)))))))

(define (legacy-property name)
  (define-values [ prop pred get ]
    (make-struct-type-property
     name
     (lambda (impl info)
       (error
        name
        (string-append
         "this property is a legacy implementation; "
         "use prop:contract or prop:flat-contract instead.")))))
  prop)

(define proj-prop (legacy-property 'proj-prop))
(define name-prop (legacy-property 'name-prop))
(define stronger-prop (legacy-property 'stronger-prop))
(define first-order-prop (legacy-property 'first-order-prop))
(define flat-prop (legacy-property 'flat-prop))

(define proj-pred? contract-struct?)
(define name-pred? contract-struct?)
(define stronger-pred? contract-struct?)
(define first-order-pred? contract-struct?)
(define flat-pred? contract-struct?)

(define (proj-get c) contract-proc)
(define (name-get c) contract-name)
(define (stronger-get c) contract-stronger?)
(define (first-order-get c) contract-first-order)
(define (flat-get c) flat-contract-predicate)

;; unpack-blame : any/c -> any/c
;; Constructs an S-expression for use in the blame error messages.
;; A variable reference represents a module or top-level context.
;; Other representations of blame are returned as-is.
(define (variable-reference? x) #f)
(define (variable-reference->module-source blame) #f)

(define (unpack-blame blame)
  (if (variable-reference? blame)
      (let ([resolved (variable-reference->module-source blame)])
        (cond
         [(not resolved) 
          'top-level]
         [else
          (cond
           [(symbol? resolved) `(quote ,resolved)]
           [else `(file ,resolved)])]))
      blame))

(define (unpack-source info)
  info
 #;(cond
   [(syntax? info) (build-source-location info)]
   [(list? info)
    (let ([loc (list-ref info 0)])
      (if (syntax? (srcloc-source loc))
        (struct-copy
         srcloc loc
         [source
          (resolved-module-path-name
           (module-path-index-resolve
            (syntax-source-module
             (srcloc-source loc))))])
        loc))]
   [else
    (rerror 'contract
           "expected a syntax object or list of two elements, got: ~a"
           info)]))

(define (unpack-name info)
  (cond
   [(syntax? info) (and (identifier? info) (syntax-e info))]
   [(list? info) (list-ref info 1)]
   [else
    (rerror 'contract
           "expected a syntax object or list of two elements, got: ~a"
           info)]))
