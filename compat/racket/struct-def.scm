(define-module (compat racket struct-def)
  #:use-module (compat racket misc)
  #:use-module (compat racket struct)
  #:export (prop:procedure prop:procedure? get-prop:procedure
            prop:struct-info prop:struct-info? get-prop:struct-info

            impersonator-prop:application-mark 
            impersonator-prop:application-mark? 
            get-impersonator-prop:application-mark

            prop:chaperone prop:chaperone? get-prop:chaperone

            struct:struct-info 
            make-impersonator-property))



(define-values (prop:procedure prop:procedure? get-prop:procedure)
  (make-struct-type-property 'prop:procedure))

(define-values (impersonator-prop:application-mark 
                impersonator-prop:application-mark? 
                get-impersonator-prop:application-mark)
  (make-struct-type-property 'impersonator-prop:application-mark))

(define-values (prop:struct-info prop:struct-info? get-prop:struct-info)
  (make-struct-type-property 'prop:struct-info))

(define-values (prop:chaperone prop:chaperone? get-prop:chaperone)
  (make-struct-type-property 'prop:chaperone))

(define-rstruct struct:struct-info ())

(define make-impersonator-property make-struct-type-property)
