(define-module (compat racket vector)
  #:use-module (compat racket misc)
  #:use-module (srfi srfi-11)
  #:export (vector->rvector rvector-ref rvector-set!))

(define *vector-api* (make-weak-key-hash-table))


(define (rvector-ref vec i)
  (aif (it) (hashq-ref *vector-api* vec)
       ((car it) vec i)
       (old-ref vec i)))

(define (rvector-set! vec i v)
  (aif (it) (hashq-ref *vector-api* vec)
       ((cdr it) vec i v)
       (old-set! vec i v)))

(define (vector->rvector mkref mkset vec)
  (hashq-set! *vector-api* vec (cons mkref mkset))
  vec)
