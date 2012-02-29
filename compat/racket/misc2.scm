(define-module (compat racket misc2)
  #:use-module (compat racket misc)
  #:use-module (srfi srfi-11)
  #:export (define-syntaxes))


(define-syntax define-syntaxes
  (lambda (x)
    (syntax-case x ()
      ((_ (s ...) code ...)
       (with-syntax (((ss  ...) (generate-temporaries #'(s ...)))
                     ((sss ...) (generate-temporaries #'(s ...)))
                     (a         (datum->syntax (stx-car #'(s ...))
                                               (gensym "a"))))
         
         #'(begin
             (eval-when (compile load eval)
               (define ss #f))
             ...
             (define a
               (let-values (((sss ...) (begin code ...)))                 
                 (set! ss sss)
                 ...
                 #f))             
             (define-syntax s ss)
             ...))))))
           
