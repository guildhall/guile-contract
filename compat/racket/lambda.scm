(define-module (compat racket lambda)
  #:use-module (compat racket misc)
  #:use-module (syntax parse)
  #:export (rlambda))

(define-syntax rlambda
  (lambda (stx)
    (syntax-parse stx
      ((_ ((~or x:id (~seq k:keyword (xk:id vk)) (xo:id vo)) ... . rest)
          code ...)
       (cond 
        ((and (stx-pair? #'(k ...)) (stx-pair? #'(xo ...)))
         #'(lambda* (x ... #:optional (xo vo) ... #:key (xk vk) ... . rest)
                    code ...))

        ((and (stx-pair? #'(k ...)) (stx-null? #'(xo ...)))
         #'(lambda* (x ... #:key (xk vk) ... . rest)
                    code ...))

        ((and (stx-null? #'(k ...)) (stx-pair? #'(xo ...)))
         #'(lambda* (x ... #:optional (xo vo) ... . rest)
                    code ...))

        (else
         #'(lambda (x ... . rest) code ...)))))))
