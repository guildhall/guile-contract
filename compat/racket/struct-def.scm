(define-module (compat racket struct-def)
  #:use-module (compat racket misc)
  #:use-module (compat racket struct)
  #:export (prop:procedure prop:procedure? get-prop:procedure
            prop:struct-info prop:struct-info? get-prop:struct-info
            struct:struct-info 
            make-impersonator-property))



(define-values (prop:procedure prop:procedure? get-prop:procedure)
  (make-struct-type-property 'prop:procedure))

(define-values (prop:struct-info prop:struct-info? get-prop:struct-info)
  (make-struct-type-property 'prop:struct-info))

(define-rstruct struct:struct-info ())

(define make-impersonator-property make-struct-type-property)
