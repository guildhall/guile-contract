(define-module (compat racket struct)
  #:use-module (syntax parse)
  #:use-module (compat racket misc)
  #:use-module (system base compile)
  #:use-module (srfi srfi-11)

  #:export (define-rstruct 
             make-struct-type
             make-struct-type-property
             struct-type-property-attach
             struct-type-info
             current-inspector
             object-name

             struct-info?
             extract-struct-info))

(define old-assoc assoc)
(define (assoc k x)
  (if x
      (old-assoc k x)
      #f))

(define env (current-module))
(define *rec-props* (make-weak-key-hash-table))
(define *fail* (gensym "fail"))

(define-struct-struct struct-type-property (attach))

(define-struct-struct struct-type (super-type info vtab size init-size 
                                        inspector init))


(define (printer name)
  (lambda (struct port)
    (display "#<"  port)
    (display name  port)
    (display ">"   port)))

(define* (make-struct-type-property key #:optional guard (supers '()))  
  (let* ((guard2
          (if (procedure? guard)
              guard
              (lambda (x y) x)))
         
         ;;this will later be set!:ed to the descriptor
         (a    #f)

         (attach
          (lambda (s v i)
            (let ((l (hashq-ref *rec-props* s '()))
                  (v (guard2 v i)))
              (hashq-set! *rec-props* s (cons (cons key (cons a v)) l))
              (for-each 
               (lambda (x)
                 (let ((mk (struct-type-property-attach (car x))))
                   (mk s ((cdr x) v) i)))
               supers))))

         (desc 
          (let ((d (make-struct-type-property attach)))
            (set! a d)
            d))

         (type?
          (lambda (item)
            (aif (rec) (if (struct-type? item)
                           (struct-type-vtab item)
                           (if (struct? item)
                               (struct-vtable item)
                               item))
                 (aif (it) (hashq-ref *rec-props* rec)
                      (aif (it2) (assoc key it)
                           (and (eq? (cadr it2) desc) (cddr it2))
                           #f)
                      #f)
                 #f)))

         (accessor
          (lambda* (rec #:optional (failres *fail*))
             (let ((fail (cond ((procedure? failres)
                                failres)
                               ((eq? failres *fail*)
                                (lambda ()
                                  (error "struct type prop cannot find value")))
                               (else
                                (lambda () failres)))))

               (let ((rec (if (struct-type? rec)
                              (struct-type-vtab rec)
                              (if (struct? rec)
                                  (struct-vtable rec)
                                  rec))))
                 (aif (it) (hashq-ref *rec-props* rec)
                      (aif (it2) (assoc key it)
                           (if (eq? (cadr it2) desc)
                               (cddr it2)
                               (fail))
                           (fail))
                      (fail)))))))

  (values desc type? accessor)))



(define default-inspector (lambda x (error "this feature is not implemented")))
(define current-inspector (make-fluid default-inspector))

(define make-struct-type* make-struct-type)
(define* (make-struct-type name	 
                           super-type	 
                           init-field-cnt	 
                           auto-field-cnt
                           #:optional
                           (auto-v  #f)	 
                           (props   '())
                           inspector
                           proc-spec
                           (immutables '())
                           guard
                           constructor-name
                           generate	 
                           exercise)
  (if proc-spec
      (error "prop-spec is not implemented"))
  
  (let* ((pdesc super-type)
         (np    (if pdesc
                    (struct-type-size pdesc)
                    0))

         (n     (+ init-field-cnt
                   auto-field-cnt                   
                   np))

         (pinit (if pdesc
                    (struct-type-init-size pdesc)
                    0))

         (ninit (+ init-field-cnt pinit))

         (vtab  (make-vtable
                 (list->string 
                  (let loop ((i 0) (r '()))
                    (if (= n i)
                        (reverse r)
                        (loop (+ i 1) (list* #\w #\p r)))))
                 (printer name)))
         
         (v-parent (let loop ((i 0) (v '()))
                     (if (= i pinit)
                         v
                         (loop (+ i 1) (cons (gensym "v-parent") v)))))

         (v-init   (let loop ((i 0) (v '()))
                     (if (= i init-field-cnt)
                         v
                         (loop (+ i 1) (cons (gensym "v") v)))))

         (n-init   (let loop ((i np) (j 0) (r '()))
                     (if (= j init-field-cnt)
                         (reverse r)
                         (loop (+ i np) (+ j 1) 
                               (cons i r)))))

         (n-default (let loop ((i (+ np init-field-cnt)) (j 0) (r '()))
                     (if (= j auto-field-cnt)
                         (reverse r)
                         (loop (+ i np) (+ j 1) 
                               (cons i r)))))

         (ref      (lambda (s n)
                     (struct-ref s (+ n np))))

         (set      (lambda (s n v)
                     (struct-set! s (+ n np) v)))

         (super    (let loop ((super super-type) (ret #f))
                     (if super
                         (let ((i (struct-type-info super)))
                           (if (eq? (struct-type-inspector super) inspector)
                               (loop (struct-type-super-type super)
                                     super)
                               ret))
                         ret)))
         
         (i     (list name init-field-cnt auto-field-cnt 
                      ref set '() 
                      super
                      (if super
                          (struct-type-super-type super)
                          #f)))
                                                              

         (init-p (if pdesc (struct-type-init pdesc) #f))

         (init   (if pdesc
                     ((compile
                       `(lambda (init-p guard auto-v)
                          (lambda (s ,@v-parent ,@v-init)
                            (let-values (((,@v-parent ,@v-init)
                                          (if guard
                                              (guard ,@v-parent
                                                     ,@v-init
                                                     ',name)
                                              (values ,@v-parent
                                                      ,@v-init))))
                              (init-p s ,@v-parent)
                              ,@(map (lambda (v n) `(struct-set! s ,n ,v))
                                     v-init
                                     n-init)
                              ,@(map (lambda (n)   `(struct-set! s ,n auto-v))
                                     n-default)
                              (if #f #f))))
                       #:env env)
                      init-p guard auto-v)
                     ((compile
                       `(lambda (guard auto-v)
                          (lambda (s ,@v-init)
                            (let-values (((,@v-init)
                                          (if guard
                                              (guard  ,@v-init
                                                      ',name)
                                              (values ,@v-init))))
                              ,@(map (lambda (v n) `(struct-set! s ,n ,v))
                                     v-init
                                     n-init)
                              ,@(map (lambda (n)   `(struct-set! s ,n 
                                                                 auto-v))
                                     n-default)

                              (if #f #f))))
                       #:env env)
                      guard auto-v)))

         (it        (make-struct-type* super-type i vtab n 
                                       ninit inspector init))
         
                     
         (mk        ((compile                  
                      `(lambda (v init)
                         (lambda (,@v-parent ,@v-init)
                           (let ((s (make-struct v 0)))
                             (init s ,@v-parent ,@v-init)
                             s)))
                      #:env env)
                     vtab init))


         (?        (lambda (s) 
                     (if (struct? s)
                         (eq? (struct-vtable s) vtab)
                         #f))))

    
    (for-each 
     (lambda (x)
       (let ((mk (struct-type-property-attach 
                  (car x))))
         (mk vtab (cdr x) i)))
     props)
    
    (values it mk ? ref set)))
                        

(define-syntax-class np
  (pattern name:id
           #:with parent #'#f)
  (pattern (name:id parent:expr)))
           


(define-syntax define-rstruct
  (lambda (x)
    (syntax-parse x
      ((_ n:np (field ...)
          (~seq #:property pkey:expr pval:expr) ...)
       (let ((nm (syntax->datum #'n.name)))
         (with-syntax ((q   (format-id #'n.name "~a-fkns"   nm))
                       (mk  (format-id #'n.name "make-~a"   nm))
                       (ref (format-id #'n.name "~a-ref"    nm))
                       (set (format-id #'n.name "~a-set!"   nm))
                       (?  (format-id #'n.name "~a?" nm))
                       ((fn ...)
                        (map (lambda (x)
                               (format-id #'n.name "~a-~a"
                                          nm (syntax->datum x)))
                             (stx->list #'(field ...))))
                       ((fi ...) 
                        (let loop ((n 0) (ns (stx->list #'(field ...))))
                          (if (pair? ns)
                              (cons (datum->syntax #'n.name n)
                                    (loop (+ n 1) (cdr ns)))
                              '()))))
           #`(begin             
               (define-values 
                 (n.name mk ? ref set)
                 (make-struct-type 'n.name 
                                   n.parent
                                   (length '(field ...))
                                   0
                                   #f
                                   (list (cons pkey pval) ...)))

                 (define (fn s) (ref s fi))
                 ...
                              
                 (define q '(q n.name mk ? fn ...)))))))))

(define (object-name x)
  (if (struct? x)
      (let ((r (struct-vtable-name (struct-vtable x))))
        (if (eq? r 'struct-type)
            (struct-vtable-name (struct-vtable (struct-type-vtab x)))
            r))
      (if (procedure? x)
          (procedure-name x)
          (rerror 'object-name "name not suported for ~a" x))))

(define (struct-info? x)
  (or (and (list? x) (= (length x) 6))
      (struct-type? x)))

(define (extract-struct-info x)
  (cond 
   ((list? x) x)
   ((struct-type? x) (struct-type-info x))))
