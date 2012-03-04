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

(define a/c (or/c (-> number? number?)
                  (-> string? string? string?)))
(define (g x) "b")
(define f (contract a/c g 'positive 'negative))
(test (f 'a))
(test (f 1))

(test (contract (and/c number? integer?) 1.1 'a 'b))
(test (contract (not/c number?)          'a  'a 'b))
(test (contract (not/c number?)          1.1 'a 'b))
(test (contract (=/c 1.1)          1.1 'a 'b))
(test (contract (=/c 1.1)          1.2 'a 'b))
(test (contract (</c 1.1)          1.0 'a 'b))
(test (contract (</c 1.1)          1.1 'a 'b))
(test (contract (>/c 1.1)          1.0 'a 'b))
(test (contract (>/c 1.1)          1.2 'a 'b))
(test (contract (<=/c 1.1)          1.0 'a 'b))
(test (contract (<=/c 1.1)          1.2 'a 'b))
(test (contract (>=/c 1.1)          1.0 'a 'b))
(test (contract (>=/c 1.1)          1.2 'a 'b))
(test (contract (between/c 1.1 1.4) 1.0 'a 'b))
(test (contract (between/c 1.1 1.4) 1.2 'a 'b))
(test (contract (between/c 1.1 1.4) 1.5 'a 'b))
(test (contract (string-len/c 3)    "aaa" 'a 'b))
(test (contract (string-len/c 3)    "aaaa" 'a 'b))
(test (contract (string-len/c 3)    "aa" 'a 'b))
(test (contract printable/c    "aa" 'a 'b))
(test (contract (one-of/c 1 2 3)    "aa" 'a 'b))
(test (contract (one-of/c 1 2 3)    1 'a 'b))
(test (contract (symbols 'a 'b 'c)    'd 'a 'b))
(test (contract (symbols 'a 'b 'c)    'c 'a 'b))
#;(test (contract (vectorof number?)   #(1) 'a 'b))
(test (contract (listof number?)    '(1) 'a 'b))
(test (contract (listof number?)    '(1 a) 'a 'b))
(test (contract (non-empty-listof number?)    '(1) 'a 'b))
(test (contract (non-empty-listof number?)    '() 'a 'b))
(test (contract (cons/c number? null?)    '(1) 'a 'b))
(test (contract (cons/c number? null?)    '(a) 'a 'b))
(test (contract (list/c number? symbol?)  '(1 a) 'a 'b))
(test (contract (list/c number? symbol?)  '(1 1) 'a 'b))
(test (contract (syntax/c 'a) #'a 'a 'b))
(test (contract (syntax/c 'a)  'a 'a 'b))
(test (contract (flat-rec-contract 
                 sexp
                 (cons/c sexp sexp)
                 number?
                 null?
                 symbol?)

                '(let ((a 1)) a) 

                'a 'b))

(test (contract (flat-rec-contract 
                 sexp
                 (cons/c sexp sexp)
                 number?
                 null?
                 symbol?)

                '(let ((a 1)) "a") 

                'a 'b))

(define flat
  (flat-murec-contract ((plus  (cons/c (symbols '+) minus))
                              (minus (cons/c (symbols '-) (or/c plus null?))))
      (list/c plus minus)))

(test (contract flat (list '(+ - + -) '(-)) 'a 'b))
(test (contract flat (list '(+ - + -) '(- +)) 'a 'b))

(define f (contract (-> string? any) (lambda (x) (values 1 2)) 'a 'b))
(test (f "a"))

(define f (contract (-> string? (values number? number?))
                    (lambda (x) (values 1 2)) 'a 'b))

(test (f "a"))
(define f (contract (-> string? (values number? number?))
                    (lambda (x) (values 1 2 3)) 'a 'b))

(test (f "a"))


;; these demand some kind of object like features
;; vector/c
;; struct/c
;; parameter/c
;; hash/c    not implemented
;; promice/c not implemented


(define f/c (->* (number? number?) (number? #:z number?) any))

(define f (contract f/c (lambda* (x q #:optional (y 0) #:key (z 1)) 
                                 (+ x y z q)) 'a 'b))

(f 1 2)
(f 1 2 3)
(f 1 2 3 #:z 4)
(f 1 2 #:z 3)
(test (f 1 2 'a))

(define f/c (->* (number? number?) (number? #:z number?) any))
(define f (contract f/c (case-lambda* ((x) x) ((x y #:key (z 2)) x) ((x y z) x)) 'a 'b))

(f 1 2 #:z 3)
(test (f 1 2 #:z 'a))


