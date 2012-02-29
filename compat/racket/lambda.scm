(define-module (compat racket lambda)
  #:use-module (compat racket misc)
  #:use-module (syntax parse)
  #:export (rlambda))


(define (keyw->sym-stx k)
  (datum->syntax 
   k (keyword->symbol
      (syntax->daym k))))

(define-splicing-syntax-class kwt
  (pattern (~seq key:keyword (i:id kv))
           #:with key-name (keyw->sym-stx #'key))
  (pattern (~seq key:keyword  i:id)
           #:with key-name (keyw->sym-stx #'key)                            
           #:with kv #f))

(define-syntax rlambda
  (lambda (stx)
    (syntax-parse stx
      ((_ ((~or x:id k:kwt (xo:id vo)) ... . rest)
          code ...)
       (cond 
        ((and (stx-pair? #'(k ...)) (stx-pair? #'(xo ...)))
         #'(lambda* (x ... 
                       #:optional (xo vo)           ... 
                       #:key      (k.key-name k.kv) ... . rest)
              (let ((k.i k.key-name) ...)
                code ...)))

        ((and (stx-pair? #'(k ...)) (stx-null? #'(xo ...)))
         #'(lambda* (x ... #:key (k.key-name k.kv) ... . rest)
              (let ((k.i k.key-name) ...)
                code ...)))

        ((and (stx-null? #'(k ...)) (stx-pair? #'(xo ...)))
         #'(lambda* (x ... #:optional (xo vo) ... . rest)
                    code ...))

        (else
         #'(lambda (x ... . rest) code ...)))))))
