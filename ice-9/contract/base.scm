(define-module (ice-9 contract base)
  #:use-module (ice-9 contract src arrow)
  #:use-module (ice-9 contract src arr-i)
  #:use-module (ice-9 contract src base)
  #:use-module (ice-9 contract src box)
  #:use-module (ice-9 contract src hash)
  #:use-module (ice-9 contract src vector)
  #:use-module (ice-9 contract src struct)
  #:use-module (ice-9 contract src misc)
  #:use-module (ice-9 contract src out)
  #:use-module (ice-9 contract src guts)
  #:use-module (ice-9 contract src legacy)
  #:use-module (ice-9 contract src ds)
  #:use-module (ice-9 contract src opt)

  #:use-module (compat racket misc)
  #:re-export (opt/c define-opt/c))

(re-export-all (ice-9 contract src ds)
               #:except (lazy-depth-to-look))
(re-export-all (ice-9 contract src arrow)
               #:except (making-a-method
                         procedure-accepts-and-more?
                         check-procedure
                         check-procedure/more
                         make-contracted-function))

(re-export-all (ice-9 contract src arr-i))
(re-export-all (ice-9 contract src box))
(re-export-all (ice-9 contract src hash))
(re-export-all (ice-9 contract src vector))
(re-export-all (ice-9 contract src struct))
(re-export-all (ice-9 contract src misc)
               #:except (check-between/c
                         check-unary-between/c))
(re-export-all (ice-9 contract src out))
(re-export-all (ice-9 contract src base))
(re-export-all (ice-9 contract src legacy))
(re-export-all (ice-9 contract src guts)
               #:except (check-flat-contract
                         check-flat-named-contract))
