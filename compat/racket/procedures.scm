(define-module (compat racket procedures)
  #:use-module (system base compile)
  #:use-module (system vm program)
  #:use-module (ice-9 match)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-11)
  #:use-module (compat racket struct)
  #:use-module (compat racket struct-def)
  #:use-module (compat racket misc)
  #:use-module (compat racket misc2)
  #:use-module ((compat racket vector) #:select (vector->rvector))
  #:export (impersonate-procedure
            chaperone-procedure
            impersonate-struct
            chaperone-struct
            impersonate-hash
            chaperone-hash
            impersonate-box
            chaperone-box
            impersonate-vector
            chaperone-vector
            
            make-derived-parameter
            procedure-rename
            
            chaperone-of?))



(define old-assoc assoc)
(define (assoc k x)
  (if x
      (old-assoc k x)
      #f))

(define impersonate-struct
  (lambda x
    (error "impersonator-struct is not implemented")))
(define chaperone-struct
  (lambda x
    (error "chaperone-struct is not implemented")))

(define impersonate-box
  (lambda x
    (error "impersonator-box is not implemented")))
(define chaperone-box
  (lambda x
    (error "chaperone-box is not implemented")))

(define impersonate-hash
  (lambda x
    (error "impersonator-hash is not implemented")))
(define chaperone-hash
  (lambda x
    (error "chaperone-hash is not implemented")))

(define env (current-module))

(define novalue (gensym "not-defined"))


(define (impersonate-vector vec ref-proc set-proc . props)
  (let* ((f-ref (lambda (vec i)
                  (ref-proc vec i (vector-ref vec i))))
         (f-set (lambda (vec i v)
                  (vector-set! vec i (set-proc vec i v))))
         (vec   (vector->rvector f-ref f-set vec)))

    (let loop ((props props))
      (match props
        ((prop val . l)
         ((struct-type-property-attach prop) vec val '())
         (loop l))
        (() 'ok)))

    vec))


(define chaperone-vector impersonate-vector)    


(define (put-proc-first i ii)
  (let loop ((i i))
    (if (pair? i)
        (if (member (car i) ii)
            (cons i (loop (cdr i)))
            (error "wraper has not a key of the item"))
        (let loop ((ii ii))
          (if (pair? ii)
              (if (member (car ii) i)
                  (loop (cdr ii))
                  (cons (car ii) (loop (cdr ii))))
              '())))))
         
(define (match-wrap-res proc n)
  (lambda (xin)
    (case-lambda
      (() 
       (proc))
    
      ((x . l)
       (if (procedure? x)
           (if (eq? (length l) (length xin))
               (call-with-values
                   (lambda () (apply proc l))
                 x)
               (apply proc x l))
           (apply proc x l))))))



(define (to-keys x vs)
  (let loop ((vs vs) (x x) (r '()))
    (match x
      (((? keyword? k) v . l)
       (loop (cdr vs) l `(,k ,(car vs) ,@r)))
      ((               v . l)
       (loop vs l r))
      (()
       r))))

(define (match-wrap-res-keys proc n)
  (let ((alt (match-wrap-res proc n)))
    (lambda (x)
      (if (ormap keyword? x)
          (case-lambda
            ((ks)
             (apply proc (to-keys x ks)))
            ((f ks . l)
             (if (procedure? f)
                 (let-values (((args keys)
                               (let loop ((x x) (a '()) (ks '()))
                                 (match x
                                   (((keyword? k) v . l)
                                    (loop l a `(,v ,k ,@ks)))
                                   ((x . l)
                                    (loop l (cons x a) ks))
                                   (()
                                    (values (reverse a) (reverse ks)))))))
                   (call-with-values
                       (lambda () 
                         (apply proc (append l (to-keys keys ks))))
                     f))
                 (apply proc ks (append l (to-keys x f))))))
          (alt x)))))


;Todo allow for optional varibales
(define (impersonate-procedure proc wrapper-proc . props)
  (when (and (procedure? proc) (procedure? wrapper-proc))
    (let* ((argdata   (procedure-arguments proc))
           (argdata-w (procedure-arguments wrapper-proc))
           (keys      (let-values (((a b) (procedure-keywords proc)))
                        b))
           (rest      (assoc 'rest argdata))
           (rest-w    (assoc 'rest argdata-w))
           (opt       (assoc 'optional argdata))
           (opt-w     (assoc 'optional argdata-w))
           (v-k-w     (map keyword->symbol keys))
           (v         (cdr (assoc 'required argdata)))
           (v-w       (cdr (assoc 'required argdata-w)))
           (name      (procedure-name proc))

           (match-wrap
            (if (null? keys)
                (match-wrap-res proc (length v))
                (match-wrap-res-keys proc (length v))))

           (arity-error 
            (lambda () 
              (error 
               (string-append "wrong arity in the construction of "
                              "personate/chaperone - procedure")))))

      (let loop ((v (append v opt)) (v-w (append v-w opt)))
        (if (pair? v)
            (if (pair? v-w)
                (loop (cdr v) (cdr v-w))
                (if (not rest-w)
                    (arity-error)))
            (if rest
                (if (not rest-w)
                    (arity-error)))))
                    
                    
      (let ((res
             (if (null? keys)
                 (lambda x
                   (call-with-values
                       (lambda () (apply wrapper-proc x))
                     (match-wrap x)))
                     
                 (lambda x
                   (call-with-values
                       (lambda () (apply wrapper-proc x))
                     (match-wrap x))))))

        (let loop ((props props))
          (match props
            ((k v . l)
             ((struct-type-property-attach k) res v '())
             (loop l))
            (_ #f)))

        (aif (name) (procedure-name proc)
             (procedure-rename res name))

        (let ((arity (procedure-minimum-arity proc)))
          (set-procedure-minimum-arity!
           res (car arity) (cadr arity) (caddr arity)))
        
        (set-procedure-property!
         res 'arglist 
         (aif (it) (procedure-property proc 'arglist)
              it
              (let loop ((as argdata))
                (match as
                  ((('allow-other-keys? . f) . l)
                   (cons f (loop l)))
                  ((('rest . f) . l) 
                   (cons f (loop l)))
                  (() '())
                  (((a . l) . u)
                   (cons l (loop u)))))))
                          

        (set-procedure-property!
         res 'arglists (procedure-argdata-list proc))
        
        res))))
        
            
        
            
         

;;TODO implement this
(define (chaperone-procedure proc . l)
  (let ((res (apply impersonate-procedure proc  l)))
    ((struct-type-property-attach prop:chaperone) res proc '())
    res))

(define (chaperone-of? f1 f2)
  (aif (chap) (prop:chaperone? f2)
       (if (equal? f1 chap)
           #t
           (chaperone-of? f1 chap))
       (equal? f1 f2)))


(define (make-derived-parameter p g w)
  (case-lambda
    (()  (w (p)))
    ((x) (p (g x)))))

(define (procedure-rename f name)
  (aif (it) (assoc 'name (procedure-properties f))
       (set-cdr! it name)
       (set-procedure-properties!
        f (cons (cons 'name name) (aif (it2) it it2 '()))))
  f)
                             
