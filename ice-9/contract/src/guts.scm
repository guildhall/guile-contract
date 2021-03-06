(define-module (ice-9 contract src guts)
  #:use-module (ice-9 contract src helpers)
  #:use-module (ice-9 contract src blame)
  #:use-module (ice-9 contract src prop)
  
  #:use-module (compat racket misc)
  #:use-module (compat racket struct)
  #:use-module (compat racket for)
  #:use-module (compat racket struct-def)

  #:use-module (srfi srfi-11)

  #:export (coerce-contract
            coerce-contracts
            coerce-flat-contract
            coerce-flat-contracts
            coerce-chaperone-contract
            coerce-chaperone-contracts
            coerce-contract/f
         
            chaperone-contract?
         
            flat-contract?
            flat-contract
            flat-contract-predicate
            flat-named-contract
            build-flat-contract
         
            build-compound-type-name
         
            and/c
            any/c
            none/c
            make-none/c 
         
            contract?
            contract-name
            contract-projection
         
            contract-stronger?

            contract-first-order
            contract-first-order-passes?
         
            prop:contracted
            impersonator-prop:contracted
            has-contract?
            value-contract
         
            ;; for opters
            check-flat-contract
            check-flat-named-contract
            any
         
            ;; helpers for adding properties that check syntax uses
            define/final-prop
            define/subexpression-pos-prop))

(re-export-all (ice-9 contract src prop))
(re-export-all (ice-9 contract src blame) #:except make-blame)
                  
(define (has-contract? v)
  (or (has-prop:contracted? v)
      (has-impersonator-prop:contracted? v)))

(define (value-contract v)
  (cond
   [(has-prop:contracted? v)
    (get-prop:contracted v)]
   [(has-impersonator-prop:contracted? v)
    (get-impersonator-prop:contracted v)]
   [else #f]))

(define-values (prop:contracted has-prop:contracted? get-prop:contracted)
  (let-values ([(prop pred get)
                (make-struct-type-property
                 'prop:contracted
                 (lambda (v si)
                   (if (number? v)
                       (let ([ref (cadddr si)])
                         (lambda (s) (ref s v)))
                       (lambda (s) v))))])
    (values prop pred (λ (v) ((get v) v)))))

(define-values (impersonator-prop:contracted 
                has-impersonator-prop:contracted? 
                get-impersonator-prop:contracted)
  (make-impersonator-property 'impersonator-prop:contracted))

(define-syntax any 
  (lambda (stx)
    (raise-syntax-error 
     'any "use of 'any' outside the range of an arrow contract" stx)))

(define (contract-first-order c)
  (contract-struct-first-order
   (coerce-contract 'contract-first-order c)))

(define (contract-first-order-passes? c v)
  ((contract-struct-first-order
    (coerce-contract 'contract-first-order-passes? c))
   v))

;; contract-stronger? : contract contract -> boolean
;; indicates if one contract is stronger (ie, likes fewer values) than another
;; this is not a total order.
(define (contract-stronger? a b)
  (contract-struct-stronger? (coerce-contract 'contract-stronger? a)
                             (coerce-contract 'contract-stronger? b)))

;; coerce-flat-contract : symbol any/c -> contract
(define (coerce-flat-contract name x)
  (let ([ctc (coerce-contract/f x)])
    (unless (flat-contract-struct? ctc)
      (rerror 
       name 
       (string-append "expected a flat contract or a value that can be "
                      "coerced into one, got ~a")
       x))
    ctc))

;; coerce-flat-contacts : symbol (listof any/c) -> (listof flat-contract)
;; like coerce-contracts, but insists on flat-contracts
(define (coerce-flat-contracts name xs) 
  (for/list ([x (in-list xs)]
             [i (in-naturals)])
    (let ([ctc (coerce-contract/f x)])
      (unless (flat-contract-struct? ctc)
        (rerror 
         name
         (string-append "expected all of the arguments to be flat contracts, "
                        "but argument ~a was not, got ~a")
         i
         x))
      ctc)))

;; coerce-chaperone-contract : symbol any/c -> contract
(define (coerce-chaperone-contract name x)
  (let ([ctc (coerce-contract/f x)])
    (unless (chaperone-contract-struct? ctc)
      (rerror 
       name
       (string-append "expected a chaperone contract or a value that can be "
                      "coerced into one, got ~a")
       x))
    ctc))

;; coerce-chaperone-contacts : symbol (listof any/c) -> (listof flat-contract)
;; like coerce-contracts, but insists on chaperone-contracts
(define (coerce-chaperone-contracts name xs)
  (for/list ([x (in-list xs)]
             [i (in-naturals)])
    (let ([ctc (coerce-contract/f x)])
      (unless (chaperone-contract-struct? ctc)
        (rerror 
         name
         (string-append "expected all of the arguments to be chaperone "
                        "contracts, but argument ~a was not, got ~a")
         i
         x))
      ctc)))

;; coerce-contract : symbol any/c -> contract
(define (coerce-contract name x)
  (or (coerce-contract/f x)
      (rerror
       name 
       "expected contract or a value that can be coerced into one, got ~a"
       x)))

;; coerce-contracts : symbols (listof any) -> (listof contract)
;; turns all of the arguments in 'xs' into contracts
;; the error messages assume that the function named by 'name'
;; got 'xs' as it argument directly
(define (coerce-contracts name xs)
  (for/list ([x (in-list xs)]
             [i (in-naturals)])
    (let ([ctc (coerce-contract/f x)])
      (unless ctc
        (rerror 
         name
         (string-append "expected all of the arguments to be contracts, but "
                        "argument ~a was not, got ~a")
         i
         x))
      ctc)))

;; coerce-contract/f : any -> (or/c #f contract?)
;; returns #f if the argument could not be coerced to a contract
(define (coerce-contract/f x)
  (cond
    [(contract-struct? x) x]
    [(and (procedure? x) (procedure-arity-includes? x 1)) 
     (make-predicate-contract (or (procedure-name x) '???) x)]
    [(or (symbol? x) (boolean? x) (char? x) (null? x)) (make-eq-contract x)]
    [(or (string? x)) (make-equal-contract x)]
    [(number? x) (make-=-contract x)]
    [(or (regexp? x)) (make-regexp/c x)]
    [else #f]))

(define-syntax define/final-prop 
  (lambda (stx)
    (syntax-case stx ()
      [(_ header bodies ...)
       (with-syntax ([ctc (if (identifier? #'header)
                              #'header
                              (stx-car #'header))])

         (with-syntax ([ctc/proc 
                        (datum->syntax 
                         #'ctc 
                         (string->symbol                          
                          (format #f "~a/proc" (syntax-e #'ctc))))])
           #'(begin
               (define ctc/proc
                 (let ()
                   (define header bodies ...)
                   ctc))
               (define-syntax ctc 
               (lambda (stx)
               (syntax-case stx ()
                 [x
                  (identifier? #'x)
                  (syntax-property 
                   #'ctc/proc
                   'racket/contract:contract 
                   (vector (gensym (symbol->string 'ctc)) 
                           (list stx)
                           '()))]
                 [(_ margs (... ...))
                  (with-syntax ([app #'%%app])
                    (syntax-property 
                     #'(app ctc/proc margs (... ...))
                     'racket/contract:contract 
                     (vector (gensym (symbol->string 'ctc))
                             (list (car (syntax-e stx)))
                             '())))]))))))])))

(define-syntax define/subexpression-pos-prop 
  (lambda (stx)
  (syntax-case stx ()
    [(_ header bodies ...)
     (with-syntax ([ctc (if (identifier? #'header)
                            #'header
                            (stx-car #'header))])

       (with-syntax ([ctc/proc 
                      (datum->syntax
                       #'ctc
                       (string->symbol 
                        (format #f "~a/proc" (syntax-e #'ctc))))])
         #'(begin
             (define ctc/proc
               (let ()
                 (define header bodies ...)
                 ctc))
             (define-syntax ctc 
               (lambda (stx)
               (syntax-case stx ()
                 [x
                  (identifier? #'x)
                  (syntax-property 
                   #'ctc/proc
                   'racket/contract:contract 
                   (vector (gensym (symbol->string 'ctc)) 
                           (list stx)
                           '()))]
                 [(_ margs (... ...))
                  (let ([this-one (gensym (symbol->string 'ctc))])
                    (with-syntax 
                        ([(margs (... ...)) 
                          (map (λ (x) (syntax-property
                                       x 
                                       'racket/contract:positive-position 
                                       this-one))
                               (syntax->list #'(margs (... ...))))]
                         [app #'%%app])
                      (syntax-property 
                       #'(app ctc/proc margs (... ...))
                       'racket/contract:contract 
                       (vector this-one 
                               (list (car (syntax-e stx)))
                               '()))))]))))))])))

;                                                      
;                                                      
;                                                      
;                                                      
;                                                      
;                          ;                       ;   
;    ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;  ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;  ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;  ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;    ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;                                                      
;                                                      
;                                                      

(define (flat-contract-predicate x)
  (contract-struct-first-order
   (coerce-flat-contract 'flat-contract-predicate x)))

(define (flat-contract? x) 
  (let ([c (coerce-contract/f x)])
    (and c
         (flat-contract-struct? c))))

(define (chaperone-contract? x)
  (let ([c (coerce-contract/f x)])
    (and c
         (chaperone-contract-struct? c))))

(define (contract-name ctc)
  (contract-struct-name
   (coerce-contract 'contract-name ctc)))

(define (contract? x) (and (coerce-contract/f x) #t))
(define (contract-projection ctc)
  (contract-struct-projection
   (coerce-contract 'contract-projection ctc)))

(define (check-flat-contract predicate) 
  (coerce-flat-contract 'flat-contract predicate))

(define (flat-contract predicate) 
  (coerce-flat-contract 'flat-contract predicate))

(define (check-flat-named-contract predicate) 
  (coerce-flat-contract 'flat-named-contract predicate))

(define (flat-named-contract name predicate)
  (cond
    [(and (procedure? predicate)
          (procedure-arity-includes? predicate 1))
     (make-predicate-contract name predicate)]
    [(flat-contract? predicate)
     (make-predicate-contract name (flat-contract-predicate predicate))]
    [else
     (rerror 
      'flat-named-contract 
      (string-append "expected a flat contract or procedure of arity 1 as "
                     "second argument, got ~a")
      predicate)]))

;; build-compound-type-name : (union contract symbol) ... -> (-> sexp)
(define (build-compound-type-name . fs)
  (for/list ([sub (in-list fs)])
     (if (contract-struct? sub) (contract-name sub) sub)))

(define (and-name ctc)
  (apply build-compound-type-name 'and/c (base-and/c-ctcs ctc)))

(define (and-first-order ctc)
  (let ([tests (map contract-first-order (base-and/c-ctcs ctc))])
    (λ (x) (andmap (lambda (test) (test x)) tests))))

(define (and-proj ctc)
  (let ([mk-pos-projs (map contract-projection (base-and/c-ctcs ctc))])
    (lambda (blame)
      (let ([projs (map (λ (c) (c blame)) mk-pos-projs)])
        (for/fold ([proj (car projs)])
          ([p (in-list (cdr projs))])
          (λ (v) (p (proj v))))))))

(define (and-stronger? this that)
  (and (base-and/c? that)
       (let ([this-ctcs (base-and/c-ctcs this)]
             [that-ctcs (base-and/c-ctcs that)])
         (and (= (length this-ctcs) (length that-ctcs))
              (andmap contract-stronger?
                      this-ctcs
                      that-ctcs)))))

(define-rstruct base-and/c (ctcs))

(define-rstruct (chaperone-and/c base-and/c) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?))

(define-rstruct (impersonator-and/c base-and/c) ()
  #:property prop:contract
  (build-contract-property
   #:projection and-proj
   #:name and-name
   #:first-order and-first-order
   #:stronger and-stronger?))

(define/subexpression-pos-prop (and/c . raw-fs)
  (let ([contracts (coerce-contracts 'and/c raw-fs)])
    (cond
      [(null? contracts) any/c]
      [(andmap flat-contract? contracts)
       (let ([preds (map flat-contract-predicate contracts)])
         (flat-named-contract
          (apply build-compound-type-name 'and/c contracts)
          (λ (x) (andmap (lambda (pred) (pred x)) preds))))]
      [(andmap chaperone-contract? contracts)
       (make-chaperone-and/c contracts)]
      [else (make-impersonator-and/c contracts)])))

(define (get-any-projection c) any-projection)
(define (any-projection b) any-function)
(define (any-function x) x)

(define (get-any? c) any?)
(define (any? x) #t)

(define-rstruct any/c ()
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:projection get-any-projection
   #:stronger (λ (this that) (any/c? that))
   #:name (λ (ctc) 'any/c)
   #:first-order get-any?))

(define/final-prop any/c (make-any/c))

(define (none-curried-proj ctc)
  (λ (blame)
    (λ (val) 
      (raise-blame-error
       blame
       val
       "~s accepts no values, given: ~a"
       (none/c-name ctc)
       val))))

(define-rstruct none/c (name)
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:projection none-curried-proj
   #:stronger (λ (this that) #t)
   #:name (λ (ctc) (none/c-name ctc))
   #:first-order (λ (ctc) (λ (val) #f))))

(define/final-prop none/c (make-none/c 'none/c))




;           ATOMIC CONTRACT                                                     

(define-rstruct eq-contract (val)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order (λ (ctc) (λ (x) (eq? (eq-contract-val ctc) x)))
   #:name
   (λ (ctc) 
      (if (symbol? (eq-contract-val ctc))
        `',(eq-contract-val ctc)
        (eq-contract-val ctc)))
   #:stronger
   (λ (this that)
      (and (eq-contract? that)
           (eq? (eq-contract-val this) (eq-contract-val that))))))

(define-rstruct equal-contract (val)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order (λ (ctc) (λ (x) (equal? (equal-contract-val ctc) x)))
   #:name (λ (ctc) (equal-contract-val ctc))
   #:stronger
   (λ (this that)
      (and (equal-contract? that)
           (equal? (equal-contract-val this) (equal-contract-val that))))))

(define-rstruct =-contract (val)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order (λ (ctc) (λ (x) (and (number? x) (= (=-contract-val ctc) x))))
   #:name (λ (ctc) (=-contract-val ctc))
   #:stronger
   (λ (this that)
      (and (=-contract? that)
           (= (=-contract-val this) (=-contract-val that))))))

(define-rstruct regexp/c (reg)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order
   (λ (ctc)
      (λ (x)
         (and (or (string? x))
              (regexp-match (regexp/c-reg ctc) x)
              #t)))
   #:name (λ (ctc) (regexp/c-reg ctc))
   #:stronger
   (λ (this that)
      (and (regexp/c? that) (eq? (regexp/c-reg this) (regexp/c-reg that))))))


(define-rstruct predicate-contract (name pred)
  #:property prop:flat-contract
  (build-flat-contract-property
   #:stronger
   (λ (this that) 
      (and (predicate-contract? that)
           (procedure-closure-contents-eq? (predicate-contract-pred this)
                                           (predicate-contract-pred that))))
   #:name (λ (ctc) (predicate-contract-name ctc))
   #:first-order (λ (ctc) (predicate-contract-pred ctc))))

(define (build-flat-contract name pred) (make-predicate-contract name pred))

