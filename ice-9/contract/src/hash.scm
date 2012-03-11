(define-module (ice-9 contract src hash)
  #:use-module (ice-9 contract src guts)
  
  #:use-module (compat racket misc)
  #:use-module (compat racket for)
  #:use-module (compat racket struct)
  #:use-module (compat racket procedures)

  #:export (hash/c))
#|
#lang racket/base

(require (for-syntax racket/base)
         "guts.ss")

(provide (rename-out [wrap-hash/c hash/c]))
|#

(define-syntax wrap-hash/c 
  (lambda (stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax hash/c)
      'racket/contract:contract
      (vector (gensym "ctc") (list stx) null))]
    [(h/c arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym "ctc")])
       (define (convert-args args)
         (let loop ([args args]
                    [new-args null]
                    [neg-ctc? #t])
           (cond
             [(null? args) (reverse new-args)]
             [(keyword? (syntax-e (car args)))
              (if (null? (cdr args))
                  (reverse (cons (car args) new-args))
                  (loop (cddr args)
                        (list* (cadr args) (car args) new-args)
                        neg-ctc?))]
             [neg-ctc?
              (loop (cdr args)
                    (cons (syntax-property 
                           (car args)
                           'racket/contract:negative-position
                           this-one)
                          new-args)
                    #f)]
             [else
              (append (reverse new-args)
                      (cons (syntax-property
                             (car args)
                             'racket/contract:positive-position
                             this-one)
                            (cdr args)))])))
       (with-syntax ([(new-arg ...) (convert-args args)]
                     [app (datum->syntax stx '%app)])
         (syntax-property
          (syntax
            (app hash/c new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'h/c) null))))])))

(define* (hash/c dom rng #:key [immutable 'dont-care] [flat? #f])
  (unless (memq immutable '(#t #f dont-care))
    (rerror 'hash/c 
            (string-append "expected #:immutable argument to be either #t, "
                           "#f, or 'dont-care, got ~s") immutable))
  (let ([dom-ctc (if flat?
                     (coerce-flat-contract 'hash/c dom)
                     (coerce-contract 'hash/c dom))]
        [rng-ctc (if flat?
                     (coerce-flat-contract 'hash/c rng)
                     (coerce-contract 'hash/c rng))])
    (unless (chaperone-contract? dom-ctc)
      (rerror 'hash/c 
              (string-append "expected either a flat or chaperone contract for "
                             "the domain, got ~s") (contract-name dom-ctc)))
    (cond
     [(or flat?
          (and (eq? immutable #t)
               (flat-contract? dom-ctc)
               (flat-contract? rng-ctc)))
      (make-flat-hash/c dom-ctc rng-ctc immutable)]
     [(chaperone-contract? rng-ctc)
      (make-chaperone-hash/c dom-ctc rng-ctc immutable)]
     [else
      (make-impersonator-hash/c dom-ctc rng-ctc immutable)])))

(define (check-hash/c ctc) 
  (let ([dom-ctc   (base-hash/c-dom ctc)]
        [rng-ctc   (base-hash/c-rng ctc)]
        [immutable (base-hash/c-immutable ctc)]
        [flat?     (flat-hash/c? ctc)])
    (lambda* (val fail #:optional [first-order? #f])
      (unless (hash-table? val)
        (fail "expected a hash, got ~a" val))
      (case immutable
        [(#t) 
         (unless (immutable? val) 
           (rerror 'check-hash/c "expected an immutable hash, got ~a" val))]
        [(#f)
         (when (immutable? val)
           (rerror 'check-hash/c "expected an mutable hash, got ~a" val))]
        [(dont-care) (void)])
      (when first-order?
        (for ([k (in-list (hash-map->list (lambda (k v) k) val))]
              [v (in-list (hash-map->list (lambda (k v) v) val))])
          (unless (contract-first-order-passes? dom-ctc k)
            (rerror 'check-hash/c "expected <~s> for key, got ~v" 
                    (contract-name dom-ctc) k))
          (unless (contract-first-order-passes? rng-ctc v)
            (rerror 'check-hash/c "expected <~s> for value, got ~v" 
                    (contract-name rng-ctc) v))))
      #t)))

(define (hash/c-first-order ctc)
  (let ([check (check-hash/c ctc)])
    (λ (val)
      (let/ec return
        (check val (λ _ (return #f)) #t)))))

(define (hash/c-name ctc)
  (apply 
   build-compound-type-name
   'hash/c (base-hash/c-dom ctc) (base-hash/c-rng ctc)
   (append
    (if (and (flat-hash/c? ctc)
             (not (eq? (base-hash/c-immutable ctc) #t)))
        (list '#:flat? #t)
        null)
    (case (base-hash/c-immutable ctc)
      [(dont-care) null]
      [(#t)
       (list '#:immutable #t)]
      [(#f)
       (list '#:immutable #f)]))))

(define-rstruct base-hash/c (dom rng immutable))

(define-rstruct (flat-hash/c base-hash/c) ()
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name hash/c-name
   #:first-order hash/c-first-order
   #:projection
   (λ (ctc)
     (λ (blame)
       (λ (val)
         ((check-hash/c ctc) val (λ args (apply raise-blame-error blame val 
                                                args)))
         (let ([dom-proj ((contract-projection (base-hash/c-dom ctc)) blame)]
               [rng-proj ((contract-projection (base-hash/c-rng ctc)) blame)])
           (for ([k (in-list (hash-map->list (lambda (k v) k) val))]
                 [v (in-list (hash-map->list (lambda (k v) v) val))])
             (dom-proj k)
             (rng-proj v)))
         val)))))

(define (ho-projection hash-wrapper)
  (λ (ctc)
    (let ([dom-proc (contract-projection (base-hash/c-dom ctc))]
          [rng-proc (contract-projection (base-hash/c-rng ctc))]
          [immutable (base-hash/c-immutable ctc)])
      (λ (blame)
        (let ([pos-dom-proj (dom-proc blame)]
              [neg-dom-proj (dom-proc (blame-swap blame))]
              [pos-rng-proj (rng-proc blame)]
              [neg-rng-proj (rng-proc (blame-swap blame))])
          (λ (val)
            ((check-hash/c ctc) val (λ args (apply raise-blame-error blame val 
                                                   args)))
            
            (if (immutable? val)
                (let ([hash-maker (make-hash-table)])
                  (hash-maker
                   (for/list ([k (hash-map->list (lambda (k v) k) val)]
                              [v (hash-map->list (lambda (k v) v) val)])
                       (cons (pos-dom-proj k)
                             (pos-rng-proj v)))))
                (hash-wrapper
                 val
                 (λ (h k)
                   (values (neg-dom-proj k)
                           (λ (h k v)
                             (pos-rng-proj v))))
                 (λ (h k v)
                   (values (neg-dom-proj k)
                           (neg-rng-proj v)))
                 (λ (h k)
                   (neg-dom-proj k))
                 (λ (h k)
                   (pos-dom-proj k))
                 impersonator-prop:contracted ctc))))))))

(define-rstruct (chaperone-hash/c base-hash/c) ()
  #:omit-define-syntaxes
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name hash/c-name
   #:first-order hash/c-first-order
   #:projection (ho-projection chaperone-hash)))

(define-rstruct (impersonator-hash/c base-hash/c) ()
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:name hash/c-name
   #:first-order hash/c-first-order
   #:projection (ho-projection impersonate-hash)))

(define-syntax hash/c 
  (syntax-rules () ((_ . l) (wrap-hash/c . l))))
