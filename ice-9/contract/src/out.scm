(define-module (ice-9 contract src out)
  #:use-module (syntax parse)
  #:use-module (ice-9 match)
  #:use-module (ice-9 contract src base)
  #:export (contract-out contract-out-spec))

(define-syntax-class def
  (pattern (n:id contract:expr)
           #:with (ids ...) #'((list 'n contract))))

(define-syntax-class ren
  (pattern ((~datum rename) m:id n:id contract:expr)
           #:with (ids ...) #'((list 'm 'n contract))))

(define-syntax-class pc
  (pattern (~or x:def x:ren)
           #:with (ids ...) #'(x.ids ...)))

(define-syntax contract-out-spec
  (lambda (stx)     
    (syntax-parse stx
      ((_ n:id arg:pc ...)
       #'(define n
           (list arg.ids ... ...))))))


(define-syntax contract-out 
  (lambda (stx)
   (syntax-case stx ()
     ((_ nm)
      #'(for-each
         (lambda (d)
           (match d
             ((newsym sym cnt)
              (let ((ctc-sym+ (gensym "ctc+"))
                    (ctc-sym- (gensym "ctc-")))
                (module-define! (current-module) ctc-sym+
                                (let ((mod (module-name (current-module))))
                                  (lambda (arg neg)
                                    (contract cnt arg mod neg))))

                (module-define! (current-module) ctc-sym-
                                (let ((mod (module-name (current-module))))
                                  (lambda (arg pos)
                                    (contract cnt arg pos mod))))

                (module-define!
                 (current-module)
                 newsym
                 (make-syntax-transformer
                  newsym
                  'macro
                  (make-variable-transformer
                   (lambda (stx)
                     (with-syntax
                         ((id  (datum->syntax #'nm sym))
                          (ctc+ (datum->syntax #'nm ctc-sym+))
                          (ctc- (datum->syntax #'nm ctc-sym-))
                          (mod (datum->syntax 
                                #'nm
                                (module-name
                                 (current-module)))))
                       (syntax-case stx (set!)
                         [(set! i arg)
                          #'(set! id (ctc- arg 'mod))]
                        
                         [(f arg (... ...))
                          #'((ctc+ id 'mod) arg (... ...))]
                         
                         [ident
                          (identifier? #'ident)
                          #'(ctc+ id 'mod)]))))))
                
                (export (list newsym))))
                   

             ((sym cnt)
              (let ((ctc-sym+ (gensym "ctc+"))
                    (ctc-sym- (gensym "ctc-"))
                    (id-sym   (gensym (symbol->string sym))))
                (module-define! (current-module) ctc-sym+ 
                                (let ((mod (module-name (current-module))))
                                  (lambda (arg neg)
                                    (contract cnt arg mod neg))))

                (module-define! (current-module) ctc-sym-
                                (let ((mod (module-name (current-module))))
                                  (lambda (arg pos)
                                    (contract cnt arg pos mod))))

                (module-add! (current-module) id-sym
                             (module-variable 
                              (current-module) sym))

                (module-add!
                 (current-module)
                 sym
                 (make-variable
                  (make-syntax-transformer
                   (string->symbol
                    (string-append (symbol->string sym) "/contracted"))
                   'macro
                   (make-variable-transformer
                    (lambda (stx)
                      (with-syntax
                          ((id   (datum->syntax #'nm id-sym))
                           (ctc+ (datum->syntax #'nm ctc-sym+))
                           (ctc- (datum->syntax #'nm ctc-sym-))
                           (mod  (datum->syntax #'nm
                                                (module-name
                                                 (current-module)))))
                        (syntax-case stx (set!)
                          [(set! i arg)
                           #'(set! id (ctc- arg 'mod))]
                        
                          [(f arg (... ...))
                           #'((ctc+ id 'mod) arg (... ...))]
                            
                          [ident
                           (identifier? #'ident)
                           #'(ctc+ id 'mod)])))))))
                
                (export (list sym))))))
         nm)))))

           
