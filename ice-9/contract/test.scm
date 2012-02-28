(use-modules (compat racket misc))
(use-modules (ice-9 contract))
(use-modules (syntax parse))

(define-syntax test
  (syntax-rules ()
    ((_ x)
     (catch #t
       (lambda () x)
       (lambda (n p str l . a) (apply format #t str l) #t)))))

(define str/c (flat-contract string?))
(test (contract str/c 1 'positive 'negative))

(define str/c (flat-contract (lambda (x) (string? x))))
(test (contract str/c 1 'positive 'negative))

(define str/c (flat-named-contract 'str/c string?))
(test (contract str/c 1 'positive 'negative))

(test (contract any/c  1 'positive 'negative))
(test (contract none/c 1 'positive 'negative))


(test (contract (or/c none/c str/c) "a" 'positive 'negative))
(test (contract (or/c none/c str/c) 1   'positive 'negative))
(test (contract (or/c (not/c real?) positive?) -1.0   'positive 'negative))
(test (contract (or/c (-> number? number?)
                      (-> string? string? string?)
                      (-> string? string? string? string?))
                1
                'positive 'negative))
