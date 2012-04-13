(define-module (compat racket for)
  #:use-module (syntax parse)
  #:use-module (srfi srfi-11)
  #:use-module (compat racket hash)
  #:export (for/fold for/list for/and for/or for in-vector))

(define (in-vector x) (vector->list x))

(define-syntax true     (lambda (x) #'#t))
(define-syntax idd      (syntax-rules () ((_ x) x)))
(define-syntax add-one  (syntax-rules () ((_ x) (+ x 1))))

(define-syntax-class rng
  (pattern ((~datum in-range) end)
	   #:with a #'0
	   #:with b #'end
	   #:with c #'1)
  (pattern ((~datum in-range) start end)
	   #:with a #'start
	   #:with b #'end
	   #:with c #'1)
  (pattern ((~datum in-range) start step end)
	   #:with a #'start
	   #:with b #'end
	   #:with c #'step))

(define-syntax-class gen
  (pattern ((k:id v:id) ((~datum in-hash) ha:expr))
	   #:with (letv ...)  #'()
	   #:with (lets ...)  #`()
	   #:with (helps ...) #'()
	   #:with (helpv ...) #'()
	   #:with (init ...)  #'((hash->list ha))
	   #:with (li   ...)  (generate-temporaries #'(k))
	   #:with (pair? ...) #'((pair? li) ...)
	   #:with (car   ...) #'((k (caar li ...)) (v (cdar li ...)))
	   #:with (cdr   ...) #'((cdr li) ...))

  (pattern (i:id range:rng)
	   #:with (letv  ...) #'()
	   #:with (lets  ...) #`()
	   #:with (helps ...) #'()
	   #:with (helpv ...) #'()
	   #:with (init  ...) #'(range.a)
	   #:with (li    ...) #'(i)
	   #:with (pair? ...) #'((<= i range.b))
	   #:with (car   ...) #'()
	   #:with (cdr   ...) #'((+ i range.c)))

  (pattern (i:id ((~datum in-vector) vec:expr))
	   #:with (letv ...)  #'(vec)	   
	   #:with (lets ...)  #`(#,(datum->syntax #'vec (gensym "vec")))
	   #:with (helps ...) #`(#,(datum->syntax #'vec (gensym "n")))
	   #:with (helpv ...) #((vector-length lets ...))
	   #:with (init ...)  #'(0)
	   #:with (li   ...)  #'(i)
	   #:with (pair? ...) #'((< li helps) ...)
	   #:with (car   ...) #'((vector-ref lets li) ...)
	   #:with (cdr   ...) #'((+ li 1) ...))

  (pattern ( x:id ((~datum in-list) l:expr))
	   #:with (lets ...)  #'()
	   #:with (letv ...)  #'()
	   #:with (helps ...) #'()
	   #:with (helpv ...) #'()
           #:with (init ...)  #'(l)
	   #:with (li   ...)  (generate-temporaries #'(x))
           #:with (pair? ...) #'((pair? li) ...)
           #:with (car   ...) #'((x (car li)) ...)
           #:with (cdr   ...) #'((cdr li) ...))

  (pattern (i:id ((~datum in-naturals)))
	   #:with (lets ...)   #'()
	   #:with (letv ...)   #'()
	   #:with (helps ...)  #'()
	   #:with (helpv ...)  #'()
           #:with (init ...)   #'(0)
	   #:with (li    ...)  #'(i)
           #:with (pair? ...)  #'(#t)
           #:with (car   ...)  #'()
           #:with (cdr   ...)  #'((+ li ... 1)))

  (pattern (x:id  l:expr)
	   #:with (lets ...)  #'()
	   #:with (letv ...)  #'()
	   #:with (helps ...) #'()
	   #:with (helpv ...) #'()
           #:with (init ...)  #'(l)
	   #:with (li  ...)   (generate-temporaries #'(x))
           #:with (pair? ...) #'((pair? li) ...)
           #:with (car ...)   #'((x (car li)) ...)
           #:with (cdr ...)   #'((cdr li)   ...)))

(define-syntax for/fold
  (lambda (stx)
    (syntax-parse stx
      ((_ ((foldv:id foldi:expr) ...) (rec:gen ...) code ...)
       #'(let* ((rec.lets  rec.letv)  ... ...
		(rec.helps rec.helpv) ... ...)
	   (let loop ((foldv foldi) ... (rec.li rec.init) ... ...)
	     (if (and rec.pair? ... ...)
                 (let (rec.car ... ...)
                   (let-values (((foldv ...) (begin code ...)))
                     (loop foldv ... rec.cdr ... ...)))
                 (values foldv ...))))))))


(define-syntax for
  (lambda (stx)
    (syntax-parse stx
      ((_ (rec:gen ...) code ...)
       #'(let* ((rec.lets rec.letv) ... ...
		(rec.helps rec.helpv) ... ...)
	   (let loop ((rec.li rec.init) ... ...)
	     (if (and rec.pair? ... ...)
		 (let (rec.car  ... ...)
		   code ... 
		   (loop rec.cdr ... ...)))))))))


(define-syntax for/list
  (lambda (stx)
    (syntax-parse stx
      ((_ (rec:gen ...) code ...)
       #'(let* ((rec.lets rec.letv) ... ...
		(rec.helps rec.helpv) ... ...)
	   (let loop ((rec.li rec.init) ... ...)
             (if (and rec.pair? ... ...)
                 (cons
                  (let (rec.car ... ...)
                    code ...)
                  (loop rec.cdr ... ...))
                 '())))))))

(define-syntax for/and
  (lambda (stx)
    (syntax-parse stx
      ((_ (rec:gen ...) code ...)
       #'(let* ((rec.lets rec.letv) ... ...
		(rec.helps rec.helpv) ... ...)
	   (let loop ((out #f)(rec.li rec.init) ... ...)
	     (if (and rec.pair? ... ...)
		 (let (rec.car ... ...)
		   (loop (begin code ...) rec.cdr ... ...))
		 out)))))))

(define-syntax for/or
  (lambda (stx)
    (syntax-parse stx
      ((_ (rec:gen ...) code ...)
       #'(let* ((rec.lets rec.letv) ... ...
		(rec.helps rec.helpv) ... ...)
	   (let loop ((rec.li rec.init) ... ...)
	     (if (and rec.pair? ... ...)
		 (or (let (rec.car ... ...)
		       code ...) 
		     (loop rec.cdr ... ...))
		 #f)))))))