(define-module (compat racket boundmap)
  #:use-module (compat racket misc))

(define-syntax make-mapping-code
  (lambda (stx)
    (syntax-case stx ()
    [(_ identifier->symbol
        make-identifier-mapping
        identifier-mapping-ht
        identifier-mapping?
        identifier-mapping-get
        identifier-mapping-put!
        identifier-mapping-for-each
        identifier-mapping-map
        identifier=?)
     (and (identifier? (syntax identifier-mapping))
          (identifier? (syntax identifier-mapping-get))
          (identifier? (syntax identifier-mapping-put!))
          (identifier? (syntax identifier-mapping-for-each))
          (identifier? (syntax identifier-mapping-map)))
     (syntax
      (begin          
        (define mk-identifier-mapping
          (let ([make-identifier-mapping
                 (lambda ()
                   (make-identifier-mapping
                    (make-hash-table)))])
            make-identifier-mapping))
          
        (define identifier-mapping-get
          (lambda* (bi id #:optional 
                       [fail (lambda () 
                               (error 'identifier-mapping-get
                                      "no mapping for ~e"
                                      id))])
                   (let ([i (ormap (lambda (i)
                                     (and (identifier=? (car i) id)
                                          i))
                                   (hashq-ref (identifier-mapping-ht bi)
                                              (identifier->symbol id) 
                                              null))])
                     (if i
                         (cdr i)
                         (fail)))))
          
        (define identifier-mapping-put!
          (lambda (bi id v)
            (let ([l (hashq-ref
                      (identifier-mapping-ht bi)
                      (identifier->symbol id) 
                      null)])
              (hashq-set!
               (identifier-mapping-ht bi)
               (identifier->symbol id) 
               (let loop ([l l])
                 (cond
                  [(null? l) (list (cons id v))]
                  [(identifier=? (caar l) id)
                   (cons (cons id v) (cdr l))]
                  [else (cons (car l) (loop (cdr l)))]))))))
          
        (define identifier-mapping-for-each
          (lambda (bi f)
            (hash-for-each 
             (lambda (k v)
               (for-each (lambda (i)
                           (f (car i) (cdr i)))
                         v))
             (identifier-mapping-ht bi))))
          
        (define identifier-mapping-map
          (lambda (bi f)
            (let* ([r null])
              (identifier-mapping-for-each
               bi
               (lambda (k v)
                 (set! r (cons (f k v) r))))
              (reverse r))))
        (define make-identifier-mapping mk-identifier-mapping)
        (export
         make-identifier-mapping
         identifier-mapping?
         identifier-mapping-get
         identifier-mapping-put!
         identifier-mapping-for-each
         identifier-mapping-map)))])))
  
;; ht : hash-table[symbol(key) -> (listof (cons syntax[identifier] any))]
;; the entries in the hash-table narrow the mapping to 
;; the identifiers that match that key.
(define-struct-struct bound-identifier-mapping (ht))

(define (bound-identifier->symbol id) (syntax-e id))
  
(make-mapping-code
 bound-identifier->symbol
 make-bound-identifier-mapping
 bound-identifier-mapping-ht
 bound-identifier-mapping?
 bound-identifier-mapping-get
 bound-identifier-mapping-put!
 bound-identifier-mapping-for-each
 bound-identifier-mapping-map
 bound-identifier=?)

;; ht : hash-table[symbol(key) -> (listof (cons syntax[identifier] any))]
;; the entries in the hash-table narrow the mapping to 
;; the identifiers that match that key.
(define-struct-struct free-identifier-mapping (ht))

(define (module-identifier->symbol id) 
  (syntax-e id)
  #;(let ([binding (identifier-binding id)])
    (if (pair? binding)
        (cadr binding)
          (syntax-e id))))
  
(make-mapping-code
 module-identifier->symbol
 make-free-identifier-mapping
 free-identifier-mapping-ht
 free-identifier-mapping?
 free-identifier-mapping-get
 free-identifier-mapping-put!
 free-identifier-mapping-for-each
 free-identifier-mapping-map
 free-identifier=?)
