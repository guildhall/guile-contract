(define-module (compat racket vector)
  #:use-module (compat racket misc)
  #:use-module (srfi srfi-11)
  #:export (vector->rvector vector-ref vector-set!))

(define old-ref  vector-ref)
(define old-set! vector-set!)

(define *vector-api* (make-weak-key-hash-table))

(define (vector-ref vec i)
  (aif (it) (hashq-ref *vector-api* vec)
       ((car it) i)
       (old-ref vec i)))

(define (vector-set! vec i v)
  (aif (it) (hashq-ref *vector-api* vec)
       ((cdr i) i v)
       (old-set! vec i v)))

(define (vector->rvector mkref mkset vec)
  (let-values (((ref set)
                (aif (it) (hashq-ref *vector-api* vec)
                     (values (mkref (car it)) (mkset (cdr it)))
                     (values (mkref old-ref)  (mkset old-set!)))))

    (let ((vec-out (make-vector (vector-length vec))))
      (array-copy! vec vec-out)
      (hasq-set! *vector-api* vec-out (cons ref set))
      vec-out)))
