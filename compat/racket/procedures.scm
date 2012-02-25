(define-module (compat racket procedures)
  #:use-module (system base compile)
  #:use-module (ice-9 match)
  #:use-module (ice-9 session)
  #:use-module (compat racket struct)
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
            
            make-derived-parameter))

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

(define impersonate-vector
  (lambda x
    (error "impersonator-vector is not implemented")))
(define chaperone-vector
  (lambda x
    (error "chaperone-vector is not implemented")))

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
         

;Todo allow for optional varibales
(define (impersonate-procedure proc wrapper-proc . props)
  (when (and (procedure? proc) (procedure? wrapper-proc))
    (let* ((argdata   (procedure-arguments proc))
           (argdata-w (procedure-arguments wrapper-proc))
           (keys      (cdr (assoc 'keyword argdata)))
           (keys-w    (cdr (assoc 'keyword argdata-w)))
           (keys-w    (put-proc-first keys keys-w))
           (v-k-w     (map keyword->symbol keys))
           (v         (cdr (assoc 'required argdata)))
           (v-w       (cdr (assoc 'required argdata-w)))
           (to-keys   (lambda (vs)
                        (map (lambda (k v) (list k v)) keys vs))))
      (let ((res
             ((compile
              `(lambda (proc wrapper-proc to-keys)
                 (lambda* (,@v-w ,@(if (null? keys-w)
                                       '()
                                       `(#:key ,@(map (lambda (k) 
                                                        `(k ,novalue))
                                                      v-k-w))))
                    (call-with-values
                        (lambda () 
                          (wrapper-proc 
                           ,@v-w
                           ,@(let loop ((k keys-w) (v v-k-w) (r '()))
                               (if (pair? k)
                                   (loop (cdr k) (cdr v)
                                         `(,(car k) ,(car v) ,@r))
                                   r))))
                      ,(if (null? keys)
                           `(case-lambda
                              ((,@v) 
                               (proc ,@v))
                              ((f ,@v)
                               (call-with-values
                                   (lambda () (proc ,@v))
                                 f)))
                                 
                           `(case-lambda
                              ((ks ,@v) 
                               (apply proc ,@v (to-keys ks)))
                              ((f ks ,@v)
                               (call-with-values
                                   (lambda () 
                                     (apply proc ,@v (to-keys ks)))
                                 f)))))))
              #:env env)
              proc wrapper-proc to-keys)))
        (let loop ((props props))
          (match props
            ((k v . l)
             ((struct-type-property-attach k) res v '())
             (loop l))
            (_ #f)))
        res))))
            
            
        
            
         

;;TODO implement this
(define chaperone-procedure impersonate-procedure)


(define (make-derived-parameter p g w)
  (case-lambda
    (()  (w (p)))
    ((x) (p (g x)))))
