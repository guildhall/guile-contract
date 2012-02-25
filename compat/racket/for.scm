(define-module (compat racket for)
  #:use-module (syntax parse)
  #:use-module (srfi srfi-11)
  #:export (for/fold for/list for in-vector))

(define (in-vector x) (vector->list x))

(define-syntax true     (lambda (x) #'#t))
(define-syntax idd      (syntax-rules () ((_ x) x)))
(define-syntax add-one  (syntax-rules () ((_ x) (+ x 1))))

(define-syntax-class gen
  (pattern ((~datum in-list) li:expr)
           #:with init  #'li
           #:with pair? #'pair?
           #:with car   #'car
           #:with cdr   #'cdr)

  (pattern ((~datum in-naturals))
           #:with init  #'0
           #:with pair? #'true
           #:with car   #'idd
           #:with cdr   #'add-one)

  (pattern li:expr
           #:with init  #'li
           #:with pair? #'pair?
           #:with car   #'car
           #:with cdr   #'cdr))

(define-syntax for/fold
  (lambda (stx)
    (syntax-parse stx
      ((_ ((foldv:id foldi:expr) ...) ((recv:id reci:gen) ...) code ...)
       (with-syntax (((recvs ...) (generate-temporaries #'(recv ...))))
         #'(let loop ((foldv foldi) ... (recvs reci.init) ...)
             (if (and (reci.pair? recvs) ...)
                 (let ((recv (reci.car recvs)) ...)
                   (let-values (((foldv ...) (begin code ...)))
                     (loop foldv ... (reci.cdr recvs) ...)))
                 (values foldv ...))))))))

(define-syntax for
  (lambda (stx)
    (syntax-parse stx
      ((_ ((recv:id reci:gen) ...) code ...)
       #'(let loop ((recv reci.init) ...)
           (if (and (reci.pair? recv) ...)
               (let ((recv (reci.car recv)) ...)
                 code ... 
                 (loop (reci.cdr recv) ...))))))))

(define-syntax for/list
  (lambda (stx)
    (syntax-parse stx
      ((_ ((recv:id reci:gen) ...) code ...)
       #'(let loop ((recv reci.init) ...)
           (if (and (reci.pair? recv) ...)
               (cons
                (let ((recv (reci.car recv)) ...)
                  code ...)
                (loop (reci.cdr recv) ...))
               '()))))))
