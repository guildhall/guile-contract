(define-module (ice-9 contract src arr-i)
  #:use-module (ice-9 contract src arr-i-meat)
  #:re-export (->i/m)
  #:export (->i))

(define-syntax ->i
  (syntax-rules ()
    ((_ . l) (->i/m . l))))

