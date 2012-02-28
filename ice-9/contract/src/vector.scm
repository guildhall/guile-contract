(define-module (ice-9 contract src vector)
  #:use-module (ice-9 contract src guts)

  #:use-module (compat racket misc)
  #:use-module (compat racket struct)
  #:use-module (compat racket for)
  #:use-module (compat racket procedures)

  #:use-module (srfi srfi-11)

  #:export (vectorof vector/c
                     vector-immutable/c vector-immutableof))

#|
#lang racket/base

(require (for-syntax racket/base)
         "guts.ss")

(provide (rename-out [wrap-vectorof vectorof]
                     [wrap-vector/c vector/c])
         vector-immutable/c vector-immutableof)
|#

(define-rstruct base-vectorof (elem immutable))

(define (convert-args args this-one)
  (let loop ([args args]
             [new-args null])
    (cond
      [(null? args) (reverse new-args)]
      [(keyword? (syntax-e (car args)))
       (if (null? (cdr args))
           (reverse (cons (car args) new-args))
           (loop (cddr args)
                 (list* (cadr args) (car args) new-args)))]
      [else
       (loop (cdr args)
             (cons (syntax-property
                    (car args)
                    'racket/contract:positive-position
                    this-one)
                   new-args))])))

(define (vectorof-name c)
  (let ([immutable (base-vectorof-immutable c)])
    (apply build-compound-type-name 'vectorof 
           (contract-name (base-vectorof-elem c))
           (append
            (if (and (flat-vectorof? c)
                     (not (eq? immutable #t)))
                (list '#:flat? #t)
                null)
            (if (not (eq? immutable 'dont-care))
                (list '#:immutable immutable)
                null)))))

(define (check-vectorof c) 
  (let ([elem-ctc (base-vectorof-elem c)]
        [immutable (base-vectorof-immutable c)]
        [flat? (flat-vectorof? c)])
    (lambda* (val fail #:optional [first-order? #f])
      (unless (vector? val)
        (rerror 'xxx "expected a vector, got ~a" val))
      (cond
        [(eq? immutable #t)
         (unless (immutable? val)
           (rerror 'xxx "expected an immutable vector, got ~a" val))]
        [(eq? immutable #f)
         (when (immutable? val)
           (rerror 'xxx "expected an mutable vector, got ~a" val))]
        [else (void)])
      (when first-order?
        (for ([e (in-vector val)]
              [n (in-naturals)])
          (unless (contract-first-order-passes? elem-ctc e)
            (rerror 'xxx "expected <~s> for element ~v, got ~v" (contract-name elem-ctc) n e))))
      #t)))

(define (vectorof-first-order ctc)
  (let ([check (check-vectorof ctc)])
    (λ (val)
      (let/ec return
        (check val (λ _ (return #f)) #t)))))

(define-rstruct (flat-vectorof base-vectorof) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection 
   (λ (ctc) 
     (λ (blame) 
       (λ (val)
         ((check-vectorof ctc) val 
          (λ args (apply raise-blame-error blame val args)))
         (let* ([elem-ctc (base-vectorof-elem ctc)]
                [p ((contract-projection elem-ctc) blame)])
           (for ([e (in-vector val)])
             (p e)))
         val)))))

(define (vectorof-ho-projection vector-wrapper)
  (λ (ctc)
     (let ([elem-ctc (base-vectorof-elem ctc)]
           [immutable (base-vectorof-immutable ctc)])
       (λ (blame)
         (let ([elem-pos-proj ((contract-projection elem-ctc) blame)]
               [elem-neg-proj ((contract-projection elem-ctc) 
                               (blame-swap blame))])
           (λ (val)
             ((check-vectorof ctc) val (λ args (apply raise-blame-error 
                                                      blame val args)))
             (if (immutable? val)
                 (apply vector-immutable
                        (for/list ([e (in-list (vector->list val))])
                          (elem-pos-proj e)))
                 (vector-wrapper
                  val
                  (λ (vec i val)
                    (elem-pos-proj val))
                  (λ (vec i val)
                    (elem-neg-proj val))
                  impersonator-prop:contracted ctc))))))))

(define-rstruct (chaperone-vectorof base-vectorof) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection (vectorof-ho-projection chaperone-vector)))

(define-rstruct (impersonator-vectorof base-vectorof) ()
  #:property prop:contract
  (build-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection (vectorof-ho-projection impersonate-vector)))

(define-syntax wrap-vectorof 
  (lambda (stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax vectorof)
      'racket/contract:contract
      (vector (gensym 'ctc) (list #'x) null))]
    [(vecof arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'vectorof-ctc)])
       (with-syntax ([(new-arg ...) (convert-args args this-one)])
         (syntax-property
          (syntax
            (vectorof new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'vecof) null))))])))

(define* (vectorof c #:key [immutable 'dont-care]  [flat? #f])
  (let ([ctc (if flat?
                 (coerce-flat-contract 'vectorof c)
                 (coerce-contract 'vectorof c))])
    (cond
      [(or flat?
           (and (eq? immutable #t)
                (flat-contract? ctc)))
       (make-flat-vectorof ctc immutable)]
      [(chaperone-contract? ctc)
       (make-chaperone-vectorof ctc immutable)]
      [else
       (make-impersonator-vectorof ctc immutable)])))

(define/subexpression-pos-prop (vector-immutableof c)
  (vectorof c #:immutable #t))

(define-rstruct base-vector/c (elems immutable))

(define (vector/c-name c)
  (let ([immutable (base-vector/c-immutable c)])
    (apply build-compound-type-name 'vector/c
           (append
            (map contract-name (base-vector/c-elems c))
            (if (and (flat-vector/c? c)
                     (not (eq? immutable #t)))
                (list '#:flat? #t)
                null)
            (if (not (eq? immutable 'dont-care))
                (list '#:immutable immutable)
                null)))))

(define (check-vector/c c) 
  (let ([elem-ctcs (base-vector/c-elems c)]
        [immutable (base-vector/c-immutable c)]
        [flat? (flat-vector/c? c)])
    (lambda* (val fail #:optional [first-order? #f])
      (unless (vector? val)
        (rerror 'xxx "expected a vector, got ~a" val))
      (cond
        [(eq? immutable #t)
         (unless (immutable? val)
           (rerror 'xxx "expected an immutable vector, got ~a" val))]
        [(eq? immutable #f)
         (when (immutable? val)
           (rerror 'xxx "expected an mutable vector, got ~a" val))]
        [else (void)])
      (let ([elem-count (length elem-ctcs)])
          (unless (= (vector-length val) elem-count)
            (rerror 'xxx "expected a vector of ~a element~a, got ~a"
                  elem-count (if (= elem-count 1) "" "s") val)))
      (when first-order?
        (for ([e (in-vector val)]
              [n (in-naturals)]
              [c (in-list elem-ctcs)])
          (unless (contract-first-order-passes? c e)
            (rerror 'xxx "expected <~s> for element ~v, got ~v" 
                    (contract-name c) n val))))
      #t)))

(define (vector/c-first-order ctc)
  (let ([check (check-vector/c ctc)])
    (λ (val)
      (let/ec return
        (check val (λ _ (return #f)) #t)))))

(define-rstruct (flat-vector/c base-vector/c) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:projection 
   (λ (ctc) 
     (λ (blame) 
       (λ (val)
         ((check-vector/c ctc) val 
          (λ args (apply raise-blame-error blame val args)))
         (for ([e (in-vector val)]
               [c (in-list (base-vector/c-elems ctc))])
           (((contract-projection c) blame) e))
         val)))))

(define (vector/c-ho-projection vector-wrapper)
  (λ (ctc)
     (let ([elem-ctcs (base-vector/c-elems ctc)]
           [immutable (base-vector/c-immutable ctc)])
       (λ (blame)
         (let ([elem-pos-projs (apply vector-immutable
                                      (map (λ (c) ((contract-projection c) 
                                                   blame)) elem-ctcs))]
               [elem-neg-projs (apply vector-immutable
                                      (map (λ (c) ((contract-projection c) 
                                                   (blame-swap blame))) 
                                           elem-ctcs))])
           (λ (val)
             ((check-vector/c ctc) val (λ args (apply raise-blame-error blame 
                                                      val args)))
             (if (immutable? val)
                 (apply vector-immutable
                        (for/list ([e (in-vector val)]
                                   [i (in-naturals)])
                          ((vector-ref elem-pos-projs i) e)))
                 (vector-wrapper
                  val
                  (λ (vec i val)
                    ((vector-ref elem-pos-projs i) val))
                  (λ (vec i val)
                    ((vector-ref elem-neg-projs i) val))
                  impersonator-prop:contracted ctc))))))))

(define-rstruct (chaperone-vector/c base-vector/c) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:projection (vector/c-ho-projection chaperone-vector)))

(define-rstruct (impersonator-vector/c base-vector/c) ()
  #:property prop:contract
  (build-contract-property
   #:name vector/c-name
   #:first-order vector/c-first-order
   #:projection (vector/c-ho-projection impersonate-vector)))

(define-syntax wrap-vector/c 
  (lambda (stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax vector/c)
      'racket/contract:contract
      (vector (gensym 'ctc) (list #'x) null))]
    [(vec/c arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'vector/c-ctc)])
       (with-syntax ([(new-arg ...) (convert-args args this-one)])
         (syntax-property
          (syntax
            (vector/c new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'vec/c) null))))])))

(define* (vector/c #:key [immutable 'dont-care] [flat? #f] . cs)
  (let ([ctcs (if flat?
                  (map (λ (c) (coerce-flat-contract 'vector/c c)) cs)
                  (map (λ (c) (coerce-contract 'vector/c c))      cs))])
    (cond
      [(or flat?
           (and (eq? immutable #t)
                (andmap flat-contract? ctcs)))
       (make-flat-vector/c ctcs immutable)]
      [(andmap chaperone-contract? ctcs)
       (make-chaperone-vector/c ctcs immutable)]
      [else
       (make-impersonator-vector/c ctcs immutable)])))

(define/subexpression-pos-prop (vector-immutable/c . args)
  (apply vector/c args #:immutable #t))
