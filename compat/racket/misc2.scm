(define-module (compat racket misc2)
  #:use-module (system vm program)
  #:use-module (compat racket misc)
  #:use-module (srfi srfi-11)
  #:export (define-syntaxes 
             procedure-argdata-list
             rprocedure-minimum-arity))

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
           
(define (get-program-arguments f)
  (aif (it) (procedure-property f 'arglists)
       it
       (map (lambda (args)
              (program-arguments-alist f (+ (car args) 1)))
            (program-arities f))))

(define (procedure-argdata-list f)
  (or (procedure-property f 'arglists)
      (and (program? f) 
           (get-program-arguments f))
      (list (procedure-arguments f))))


(define (rprocedure-minimum-arity f)
  (map (lambda (x)
         (let ((req (length (cdr (assoc 'required x))))
               (opt (length (cdr (assoc 'optional x))))
               (rst (if (cdr (assoc 'rest     x))
                        #t #f)))
           (list req opt rst)))
       (get-program-arguments f)))

 
