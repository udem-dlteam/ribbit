;;; bn.scm

;; bignums operations (two's complement, little endian representation)

;; TODO

;; normalization (bn-norm & co)[x] 
;; fold (bn-fold)              [x] 
;; fixnum?                     [-] 
;; bignum?                     [-] 
;; fixnum->bignum              [x]
;; bignum->fixnum              [x]
;; bn-encode                   [x] 

;; addition (bn+)              [x] 
;; variadic addition           [x]
;; substraction (bn-)          [x] variadic substraction not defined in ribbit
;; unary substraction (bn-u)   [x]

;; multiplication (bn*)        [x] need a better algorithm
;; variadic multiplication     [x]

;; quotient (bn-quotient)      [x] need a better algorithm
;; remainder (bn-remainder)    [x]
;; modulo (bn-modulo)          [x]

;; equal (bn=)                 [x]
;; variadic equal              [x]
;; less (bn<)                  [x]
;; variadic less               [x]
;; less or equal (bn<=)        [x]
;; variadic less or equal      [x]
;; greater (bn>)               [x]
;; variadic greater            [x]
;; greater or equal (bn>=)     [x]
;; variadic greater or equal   [x]

;; negation (bn-neg)           [x]

;; bn bitwise not (bn~)        [x] 

;; bn-zero?                    [x]
;; bn-positive?                [x]
;; bn-negative?                [x]
;; bn-even?                    [x]
;; bn-odd?                     [x]
;; bn-max                      [x]
;; variadic max                [x]
;; bn-min                      [x]
;; variadic min                [x]
;; bn-abs                      [x]
;; bn-gcd                      [x]
;; variadic gcd                [x]
;; bn-lcm                      [x]
;; variadic lcm                [x]


;;------------------------------------------------------------------------------

;; utilities

;; (define base 32768)

(define base 8)

;; (define base 2)

(define base-1 (- base 1))


(define bn0 (cons 0 '_))
(set-cdr! bn0 bn0)

(define bn-1 (cons base-1 '_))
(set-cdr! bn-1 bn-1)

(define bn1 (cons 1 bn0))

(define (end? n)
  (or (eq? n bn0) (eq? n bn-1)))

(define (bn-fold func base lst)
  (if (pair? lst)
      (bn-fold func (func (car lst) base) (cdr lst))
      base))

;; FIXME bn-scan-until-false should be a macro

(define (bn-scan-until-false func base state lst)
  (if (and (pair? lst) state)
    (bn-scan-until-false func (car lst) (func base (car lst)) (cdr lst))
    state))

(define (fixnum? n) ;; temp, for our current representation
  (number? n))

(define (in-range? n)
  (and (< n base) (> n (- 0 base))))

;; FIXME should fixnum-tc? be a macro?

(define (fixnum-tc? . args) ;; checks that all args are fixnums and in range
  (let loop ((args args))
    (if (null? args)
        #t
        (and (fixnum? (car args))
             (in-range? (car args))
             (loop (cdr args))))))

;; need to integrate this with fixnum-tc?

(define (var-fixnum-tc? lst)
  (if (null? lst) #t (and (fixnum-tc? (car lst)) (var-fixnum-tc? (cdr lst)))))

(define (bignum? n) ;; temp, for our current representation
  (pair? n))

(define (fixnum->bignum n)
  (if (fixnum? n)
      (if (in-range? n)
          (if (eqv? 0 n)
              bn0
              (if (eqv? -1 n)
                  bn-1
                  (if (< n 0)
                      (cons (+ base n) bn-1) 
                      (cons n bn0))))
          (bn-encode n))
      (if (bignum? n)
          n
          #f)))

(define bn2 (fixnum->bignum 2))

(define (bignum->fixnum n)
  (if (bignum? n)
      (if (end? (cdr n))
          (if (eq? bn0 (cdr n))
              (car n)
              (if (eqv? 0 (car n))  ;; (0 $-1 $-1 ...) is not a fixnum
                  #f
                  (- 0 (bn-neg n))))
          #f) ;; conversion is not possible
      (if (and (fixnum? n) (in-range? n))
          n
          #f)))

(define (bn-encode n)
  
  (define (_bn-encode n)
    (if (and (> n (- 0 base)) (< n base))
        (cons n bn0)
        (cons (remainder n base) (_bn-encode (quotient n base)))))
  
  (let ((_n (_bn-encode (abs n))))
    (if (<= 0 n) _n (bn-neg _n))))


;; helper functions to encode bignums

(define $ bn-encode)

(define ($$ . var) 
  (let loop ((var var))
    (if (null? (cdr var))
      (car var)
      (cons (car var) (loop (cdr var))))))


;;------------------------------------------------------------------------------

;; normalization

;; Removes unnecessary symbols from a bignum e.g. (0 1 0 0 bn0) => (0 1 bn0)
;; and transforms the resulting bignum into a fixnum if possible.

;; The algorithm uses two "pointers" (p1, p2), one (p1) that memorizes the
;; previous symbol encountered and its last position in the list, while
;; the other one (p2) seeks the first occurence of a new symbol. If p2
;; encounters bn0 (bn-1), we strip off the unnecessary symbols. Example:

;; 0 0 0 1 1 1 0 0 0 0 0 bn0  =>  0 0 0 1 1 1 0 0 0 0 0 bn0
;; ^ ^                            ^     ^
;;                            =>  0 0 0 1 1 1 0 0 0 0 0 bn0
;;                                    ^ ^
;;                            =>  0 0 0 1 1 1 0 0 0 0 0 bn0
;;                                    ^       ^
;;                            =>  0 0 0 1 1 1 0 0 0 0 0 bn0
;;                                          ^ ^
;;                            =>  0 0 0 1 1 1 0 0 0 0 0 bn0
;;                                          ^            ^
;;                            =>  0 0 0 1 1 1 bn0

(define (bn-norm n) 

  (define (__bn-norm _p1 _p2)
    (if (eqv? (car (cdr _p1)) (car _p2)) 
        (cons (car _p1) (_bn-norm (cdr _p1) _p2)) 
        (cons (car _p1) (__bn-norm (cdr _p1) _p2))))

  (define (_bn-norm p1 p2)
    (if (eqv? (car p1) (car p2))
        (if (end? p2)
            p2
            (_bn-norm p1 (cdr p2))) ;; find next 
        (__bn-norm p1 p2)))         ;; memorize prev

  (if (fixnum? n)
      (if (in-range? n)
          n
          (fixnum->bignum n))
      (let ((_n (_bn-norm n (cdr n))))
        (or (bignum->fixnum _n) _n)))) ;; return a fixnum if possible


;;------------------------------------------------------------------------------

;; FIXME incomplete and redundant type checks 

;; number->string

(define (bn-number->string a)
  (if (fixnum-tc? a)
      (number->string a)
      (##bn-number->string (fixnum->bignum a))))

(define (##bn-number->string a)

  (define (##bn-number->string-aux _a radix)
    (let* ((q (bn-quotient _a radix))
           (d (bn-remainder _a radix)) 
           (c (+ 48 d))) 
      (cons c (if (bn= bn0 q)
                  '()
                  (##bn-number->string-aux q radix)))))
  
  (let ((chars (##bn-number->string-aux (##bn-abs a) (fixnum->bignum 10))))
    (if (##bn< a bn0)
        (list->string (cons (integer->char 45) ;; negative number, 45 = "#\-"
                            (reverse (map integer->char chars)))) 
        (list->string (reverse (map integer->char chars))))))


;; string->number

(define (bn-string->number str)

  (define (convert c) ;; decimal only
    (let ((_c (char->integer c)))
      (if (and (< 47 _c) (< _c 58))
          (- _c 48)
          #f)))

  (define (bn-string->number-aux lst n)
    (if (pair? lst)
        (let* ((c (car lst))
               (x (convert c)))
          (if x
              (bn-string->number-aux (cdr lst) (bn+ (bn* 10 n) x))
              #f))
        n))

  (let ((lst (string->list str)))
    (if (null? lst)
        #f
        (if (equal? (car lst) (integer->char 45))
            (let ((n (bn-string->number-aux (cdr lst) 0)))
              (and n (bn-u n)))
            (bn-string->number-aux lst 0)))))


;;------------------------------------------------------------------------------

;; addition

(define (bn+ a b)
  (if (fixnum-tc? a b)
      (bn-norm (+ a b))
      (bn-norm (##bn+ (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn+ a b) ;; no normalization, assumes a and b are bignums
  
  (define (_bn+ a b carry)
    (if (and (end? a) (end? b))
        (if (eq? a b)
            (if (eqv? 0 carry)
                (if (eq? a bn-1) ;; (if (eq? a bn0) bn0 bn-1)
                    (cons (- base 2) bn-1) 
                    bn0)
                (if (eq? a bn-1)
                    bn-1
                    (cons carry (_bn+ (cdr a) (cdr b) 0))))
            (if (eqv? 0 carry) bn-1 bn0))
        (let* ((_a (car a))
               (_b (car b))
               (res (+ _a _b carry))
               (rem (modulo res base))
               (quo (quotient res base)))
          (cons rem (_bn+ (cdr a) (cdr b) quo)))))
  
  (_bn+ a b 0))

(define (var-bn+ . args) ;; note: (var-bn+) => 0, consistent with gambit's `+`
  (if (var-fixnum-tc? args)
      (bn-norm (bn-fold + 0 args))
      (bn-norm (bn-fold ##bn+ bn0 (map fixnum->bignum args)))))


;; substraction

(define (bn- a b)
  (if (fixnum-tc? a b)
      (bn-norm (- a b))
      (bn-norm (##bn- (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn- a b)
  (##bn+ a (##bn-neg b)))


;; unary substraction

(define (bn-u a)
  (if (fixnum-tc? a)
      (- a) ;; no need to normalize the result
      (bn-norm (##bn-u (fixnum->bignum a)))))

(define (##bn-u a)
  (##bn- bn0 a))


;;------------------------------------------------------------------------------

;; multiplication (need a better algorithm)

;; faster algorithms

;;  - Booth's multiplication algorithm (specifically for two's complement)

;;  - Karatsuba O(n^(ln 3)) ≈ O(n^1.585)

;;  - Schönhage–Strassen based on fft O(n log(n) log(log(n)))

;;  - Tom-Cook (or Tom-3)

(define (bn* a b)
  (if (fixnum-tc? a b)
      (bn-norm (* a b))
      (bn-norm (##bn* (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn* a b)
  
  (define (_bn* a b carry)  ;; one line multiplication
    (if (end? a)
        (if (eqv? 0 carry) bn0 (cons carry bn0))
        (let* ((_a (car a))
               (_b (car b))
               (res (+ (* _a _b) carry))
               (rem (modulo res base))
               (quo (quotient res base)))
          (cons rem (_bn* (cdr a) b quo)))))
  
  (define (__bn* a b) ;; full (positive) mutiplication
    (if (end? b)
        bn0 
        (let ((res (_bn* a b 0))
              (pad (cons 0 a)))
          (##bn+ res (__bn* pad (cdr b))))))

  (let ((_a (##bn-abs a))
        (_b (##bn-abs b)))
    (if (eqv? (bn< a bn0) (bn< b bn0))  
        (__bn* _a _b)
        (##bn-neg (__bn* _a _b)))))

(define (var-bn* . args) ;; note: (var-bn*) => 1, consistent with gambit's `*`
  (if (var-fixnum-tc? args)
      (bn-norm (bn-fold * 1 args))
      (bn-norm (bn-fold ##bn* bn1 (map fixnum->bignum args)))))


;;------------------------------------------------------------------------------

;; quotient

;; FIXME quotient is very slow and makes bn-number->string too slow to be usable

(define (bn-quotient a b)
  (if (fixnum-tc? a b)
      (quotient a b) ;; no need to normalize the result
      (let ((_a (fixnum->bignum a))
            (_b (fixnum->bignum b)))
        (if (eq? _b bn0)   ;; division by 0
            (quotient 0 0) 
            (bn-norm (##bn-quotient _a _b))))))

(define (##bn-quotient a b)

  (define (_bn-quotient _a _b quo)
    (if (##bn< _a _b)
        quo
        (_bn-quotient (##bn- _a _b) _b (##bn+ quo bn1))))

  (let ((_a (##bn-abs a))
        (_b (##bn-abs b)))
    (if (eqv? (##bn< bn0 a) (##bn< bn0 b))
        (_bn-quotient _a _b bn0)
        (##bn-neg (_bn-quotient _a _b bn0)))))


;; remainder

(define (bn-remainder a b)
  (if (fixnum-tc? a b)
      (remainder a b) ;; no need to normalize the result
      (bn-norm (##bn-remainder (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-remainder a b)
  (##bn- a (##bn* b (##bn-quotient a b))))


;; modulo

(define (bn-modulo a b)
  (if (fixnum-tc? a b)
      (modulo a b) ;; no need to normalize the result
      (bn-norm (##bn-modulo (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-modulo a b)
  (let ((r (##bn-remainder a b)))
    (if (eq? r bn0)
        bn0
        (if (eqv? (##bn< a bn0) (##bn< b bn0))
            r
            (##bn+ r b)))))
            
      
;;------------------------------------------------------------------------------

;; equality

(define (bn= a b)
  (if (fixnum-tc? a b)
      (= a b)
      (##bn= a b)))

(define (##bn= a b)  ;; FIXME
  (eq? (bn- a b) 0)) ;; have to use bn- here e.g. (eq? (0 $0 $0 ...) bn0) => #f

(define (var-bn= a . rest) 
  (if (and (fixnum-tc? a) (var-fixnum-tc? rest))
      (bn-scan-until-false = a #t rest)
      (bn-scan-until-false
       ##bn= (fixnum->bignum a) #t (map fixnum->bignum rest))))


;; less than

(define (bn< a b)
  (if (fixnum-tc? a b)
      (< a b)
      (##bn< (fixnum->bignum a) (fixnum->bignum b))))

(define (##bn< a b)
  (##bn-negative? (##bn- a b)))

(define (var-bn< a . rest) 
  (if (and (fixnum-tc? a) (var-fixnum-tc? rest))
      (bn-scan-until-false < a #t rest)
      (bn-scan-until-false
       ##bn< (fixnum->bignum a) #t (map fixnum->bignum rest))))


;; less or equal

(define (bn<= a b)
  (if (fixnum-tc? a b)
      (<= a b)
      (##bn<= (fixnum->bignum a) (fixnum->bignum b))))

(define (##bn<= a b)
  (or (##bn= a b) (##bn< a b)))

(define (var-bn<= a . rest) 
  (if (and (fixnum-tc? a) (var-fixnum-tc? rest))
      (bn-scan-until-false <= a #t rest)
      (bn-scan-until-false
       ##bn<= (fixnum->bignum a) #t (map fixnum->bignum rest))))


;; greater than

(define (bn> a b)
  (if (fixnum-tc? a b)
      (> a b)
      (##bn> (fixnum->bignum a) (fixnum->bignum b))))

(define (##bn> a b)
  (not (##bn<= a b)))

(define (var-bn> a . rest) 
  (if (and (fixnum-tc? a) (var-fixnum-tc? rest))
      (bn-scan-until-false > a #t rest)
      (bn-scan-until-false
       ##bn> (fixnum->bignum a) #t (map fixnum->bignum rest))))


;; greater or equal

(define (bn>= a b)
  (if (fixnum-tc? a b)
      (>= a b)
      (##bn>= (fixnum->bignum a) (fixnum->bignum b))))

(define (##bn>= a b)
  (not (##bn< a b)))

(define (var-bn>= a . rest) 
  (if (and (fixnum-tc? a) (var-fixnum-tc? rest))
      (bn-scan-until-false >= a #t rest)
      (bn-scan-until-false
       ##bn>= (fixnum->bignum a) #t (map fixnum->bignum rest))))


;;------------------------------------------------------------------------------

;; bitwise operations

(define (bn~ a)
  (if (fixnum-tc? a)
      ;; FIXME need to implement bitwise-not in ribbit
      (bn-norm (##bn~ (fixnum->bignum a))) ;; (bn-norm (bitwise-not a))
      (bn-norm (##bn~ (fixnum->bignum a)))))

(define (##bn~ a)
  (if (end? a)
      (if (eq? a bn0) bn-1 bn0)
      (let ((_a (- base-1 (car a))))
        (cons _a (##bn~ (cdr a))))))


;;------------------------------------------------------------------------------

;; negation

(define (bn-neg a)
  (if (fixnum-tc? a)
      (- a)
      (bn-norm (##bn-neg (fixnum->bignum a)))))

(define (##bn-neg a)
  (##bn+ (##bn~ a) bn1))


;; zero?

(define (bn-zero? a)
  (if (fixnum-tc? a)
      (zero? a)
      (##bn-zero? (fixnum->bignum a))))

(define (##bn-zero? a)
  (##bn= a bn0))


;; positive?

(define (bn-positive? a)
  (if (fixnum-tc? a)
      (positive? a)
      (##bn-positive? (fixnum->bignum a))))

(define (##bn-positive? a)
  (##bn< bn0 a))


;; negative?

(define (bn-negative? a)
  (if (fixnum-tc? a)
      (negative? a)
      (##bn-negative? (fixnum->bignum a))))

(define (##bn-negative? a)
  (if (eq? a bn0)
      #f
      (or (eq? a bn-1) (##bn-negative? (cdr a)))))


;; even?

(define (bn-even? a)
  (if (fixnum-tc? a)
      (even? a)
      (##bn-even? (fixnum->bignum a))))

(define (##bn-even? a)
  (##bn= bn0 (##bn-modulo (##bn-abs a) bn2)))


;; odd?

(define (bn-odd? a)
  (if (fixnum-tc? a)
      (odd? a)
      (##bn-odd? (fixnum->bignum a))))

(define (##bn-odd? a)
  (not (##bn-even? a)))


;; maximum

(define (bn-max a b)
  (if (fixnum-tc? a b)
      (max a b)
      (bn-norm (##bn-max (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-max a b)
  (if (##bn< a b) b a))

(define (var-bn-max a . rest) 
  (if (and (fixnum-tc? a) (var-fixnum-tc? rest))
      (bn-fold max a rest)
      (bn-norm (bn-fold
                ##bn-max (fixnum->bignum a) (map fixnum->bignum rest)))))


;; mininum

(define (bn-min a b)
  (if (fixnum-tc? a b)
      (min a b)
      (bn-norm (##bn-min (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-min a b)
  (if (##bn< a b) a b))

(define (var-bn-min a . rest) 
  (if (and (fixnum-tc? a) (var-fixnum-tc? rest))
      (bn-fold min a rest)
      (bn-norm (bn-fold
                ##bn-min (fixnum->bignum a) (map fixnum->bignum rest)))))


;; absolute value

(define (bn-abs a)
  (if (fixnum-tc? a)
      (bn-norm (abs a))
      (bn-norm (##bn-abs a))))

(define (##bn-abs a)
  (if (##bn< a bn0) (##bn- bn0 a) a))


;; gcd

(define (bn-gcd a b)
  (if (fixnum-tc? a b)
      (gcd a b)
      (bn-norm (##bn-gcd (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-gcd a b)
  (let ((_a (##bn-abs a))
        (_b (##bn-abs b)))
    (if (##bn< _a _b)
        (##bn-gcd-aux _a _b)
        (##bn-gcd-aux _b _a))))

(define (##bn-gcd-aux a b)
  (if (##bn= a bn0) 
      b
      (##bn-gcd-aux (##bn-remainder b a) a)))

(define (var-bn-gcd . args) ;; (var-bn-gcd) => 0, consistent with gambit's `gcd`
  (if (var-fixnum-tc? args)
      (bn-fold gcd 0 args)
      (bn-norm (bn-fold ##bn-gcd bn0 (map fixnum->bignum args)))))
           

;; lcm

(define (bn-lcm a b)
  (if (fixnum-tc? a b)
      (lcm a b)
      (bn-norm (##bn-lcm (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-lcm a b)
  (if (##bn= b bn0)
      bn0
      (let ((_a (##bn-abs a))
            (_b (##bn-abs b)))
        (##bn* (##bn-quotient _a (##bn-gcd _a _b)) _b))))

(define (var-bn-lcm . args) ;; (var-bn-lcm) => 1, consistent with gambit's `lcm`
  (if (var-fixnum-tc? args)
      (bn-norm (bn-fold lcm 1 args))
      (bn-norm (bn-fold ##bn-lcm bn1 (map fixnum->bignum args)))))


;;------------------------------------------------------------------------------

;; (define-macro (test a b)
;;   `(let ((_a ,a)
;;          (_b ,b))
;;      (if (not (bn= _a _b))
;;          (begin
;;            (display "results not matching: ") (pp ',a)
;;            (display "a: ") (_pp _a)
;;            (display "b: ") (_pp _b)
;;            (newline)))))

(define (test a b)
  (if (not (bn= a b))
      (begin
        (display "results not matching: ") 
        (display "a: ") (_pp a)
        (display "b: ")(_pp b)
        (newline))
      ;;(_pp a)))
      ))

;; for comparison tests

;; (define-macro (test2 a b)
;;   `(let ((_a ,a)
;;          (_b ,b))
;;      (if (not (equal? _a _b))
;;          (begin
;;            (display "results not matching: ") (pp ',a)
;;            (display "a: ") (pp _a)
;;            (display "b: ") (pp _b)
;;            (newline)))))

(define (test2 a b)
  (if (not (equal? a b))
      (begin
        (display "results not matching: ") (newline)
        (display "a: ") (display a) (newline)
        (display "b: ") (display b) (newline)
        (newline))))

(define (_pp obj)
  (define (__pp lst)
    (if (end? lst)
        (begin
          ;; (display (if (eq? lst bn0) "#($0 #($0 ..." "#($-1 #($-1 ..."))
          (display (if (eq? lst bn0) "$0 $0 ...)"
                       (if (eq? lst bn-1) "$-1 $-1 ...)" ")")))
          (newline))
        (begin
          ;; (display "#(")
          (display (car lst))
          (display " ")
          (__pp (cdr lst)))))
  (if (integer? obj)
      (begin
        (display obj)
        (newline))
      (begin
        (display "(")
        (__pp obj))))


;;------------------------------------------------------------------------------

;; unit tests


;; normalization

;; excessive symbols at the end + list starts with repetition
(test (bn-norm (cons 0 (cons 0 (cons (- base 1) (cons (- base 1) bn-1)))))
      (cons 0 (cons 0 bn-1)))

;; no symbols to strip off
(test (bn-norm (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 1) (cons (- base 1) bn0)))

;; positive fixnum
(test (bn-norm (cons (- base 1) (cons 0 (cons 0 bn0))))
      (- base 1))

;; negative fixnum
(if (< 2 base)
    (test (bn-norm (cons (- base 2) bn-1))
          -2))

;; not a fixnum (should not return 0)
(test (bn-norm (cons 0 (cons (- base 1) (cons (- base 1) bn-1))))
      (cons 0 bn-1))

;; representation of 0
(test (bn-norm bn0) 0)

;; representation of -1
(test (bn-norm bn-1) -1)

;; already a fixnum
(test (bn-norm (- base 1)) (- base 1))



;; conversions

;; fixnum->bignum: simple case positive
(test (fixnum->bignum 0) bn0)

;; fixnum->bignum: simple case negative
(test (fixnum->bignum -1) (cons (- base 1) bn-1))

;; fixnum->bignum: already a bignum
(test (fixnum->bignum (cons (- base 1) bn0)) (cons (- base 1) bn0))

;; fixnum->bignum: invalid conversion
(test2 (fixnum->bignum #t) #f)

;; bignum->fixnum: simple case positive
(test (bignum->fixnum (cons (- base 1) bn-1)) -1)

;; bignum->fixnum: simple case negative
(test (bignum->fixnum (cons (- base 1) bn0)) (- base 1))

;; bignum->fixnum: already a fixnum
(test (bignum->fixnum -1) -1)

;; bignum->fixnum: invalid conversion
(test2 (bignum->fixnum base) #f)



;; number->string

(test2 (bn-number->string ($ 0)) "0")

(test2 (bn-number->string ($ -1)) "-1")

(test2 (bn-number->string ($ 100)) "100")

(test2 (bn-number->string ($ -100)) "-100")

(test2 (bn-number->string ($ 100000)) "100000")

(test2 (bn-number->string ($ -100000)) "-100000")
 


;; string->number

(test (bn-string->number "0") bn0)

(test (bn-string->number "-1") bn-1)

(test (bn-string->number "100") ($ 100))

(test (bn-string->number "-100") ($ -100))

(test (bn-string->number "1000000000") ($ 1000000000))

(test (bn-string->number "-1000000000") ($ -1000000000))

(test2 (bn-string->number "") #f)



;; addition

;; different parity, carry (pos res)
(test (bn+ (cons (- base 1) bn-1)
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons (- base 1) bn0)))

;; different parity, carry (res = 0)
(test (bn+ (cons (- base 1) bn-1)
           (cons (- base 1) bn0))
      (- base 2))

;; different parity, no carry (neg res)
(test (bn+ (cons 0  bn-1)
           (cons (- base 1) bn0))
      -1)

;; same parity (pos), carry
(test (bn+ (cons (- base 1) (cons (- base 1) bn0))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons (- base 1) (cons 1 bn0))))

;; same parity (pos), no carry
(test (bn+ (cons (- base 1) (cons 0 bn0))
           (cons 0 (cons 0 bn0)))
      (- base 1))
      ;; (cons base-1 bn0))

;; same parity (neg), carry
(test (bn+ (cons (- base 1) (cons (- base 1) bn-1))
           (cons (- base 1) (cons (- base 1) bn-1)))
      -2)

;; same parity (neg), no carry
(test (bn+ (cons (- base 1) (cons 0 bn-1))
           (cons (- base 1) (cons 0 bn-1)))
      (cons (- base 2) (cons 1 (cons (- base 2) bn-1))))

(test (bn+ bn0 bn0) bn0)

(test (bn+ bn0 bn-1) -1) ;; bn-1)

(test (bn+ bn-1 bn0) -1) ;; bn-1)

(test (bn+ (cons (- base 1) bn-1) (cons (- base 1) bn-1)) -2)

(test (bn+ ($ 1234) ($ 56789)) ($ 58023))

(test (bn+ ($ -1234) ($ 56789)) ($ 55555))

(test (bn+ ($ 1234) ($ -56789)) ($ -55555))

(test (bn+ ($ -1234) ($ -56789)) ($ -58023))



;; variadic addition

(test (var-bn+ ($ 10000) ($ 20000) ($ 30000) ($ 40000) ($ 50000)) ($ 150000))

(test (var-bn+ ($ -10000) ($ -20000) ($ -30000) ($ -40000) ($ -50000))
      ($ -150000))

(test (var-bn+ ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) bn0)



;; substraction

;; logic for these tests is no longer relevant, but they should still work

;; same parity (positive), no n-carry 
(test (bn- (cons (- base 1) (cons (- base 1) bn0))
           (cons 0 (cons (- base 1) bn0)))
      (- base 1))

;; same parity (negative), no n-carry 
(test (bn- (cons (- base 1) (cons (- base 1) bn-1))
           (cons 0 (cons (- base 1) bn-1)))
      (- base 1))
      
;; same parity (positive), n-carry 
(test (bn- (cons (- base 1) (cons 0 bn0))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons 0 (cons 1 bn-1)))
      
;; same parity (negative), n-carry
(test (bn- (cons (- base 1) (cons 0 bn-1))
           (cons (- base 1) (cons (- base 1) bn-1)))
      (cons 0 (cons 1 bn-1)))
      
;; different parity (pos, neg), no n-carry
(test (bn- (cons (- base 1) (cons (- base 1) bn0))
           (cons (- base 1) (cons (- base 1) bn-1)))
      (cons 0 (cons 0 (cons 1 bn0))))
      
;; different parity (neg, pos), no n-carry
(test (bn- (cons (- base 1) (cons (- base 1) bn-1))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons 0 (cons 0 bn-1)))
      
;; different parity (pos, neg), n-carry
(test (bn- (cons (- base 1) bn0)
           (cons (- base 1) (cons (- base 1) (cons 0 bn-1))))
      (cons 0 (cons 1 (cons base-1 bn0))))
      
(test (bn- bn0 bn0) 0) ;; bn0)

(test (bn- bn0 bn-1) 1) ;; (cons 1 bn0))

(test (bn- bn-1 bn0) -1) ;; bn-1)

(test (bn- bn-1 bn-1) 0) ;; bn0)

(test (bn- ($ 1234) ($ 56789)) ($ -55555))

(test (bn- ($ -1234) ($ 56789)) ($ -58023))

(test (bn- ($ 1234) ($ -56789)) ($ 58023))

(test (bn- ($ -1234) ($ -56789)) ($ 55555))



;; unary substraction

(test (bn-u bn0) 0) ;; bn0)

(test (bn-u (cons (- base 1) bn-1)) 1) ;; (cons 1 bn0))

(test (bn-u ($ 123456789)) ($ -123456789))

(test (bn-u ($ -123456789)) ($ 123456789))



;; multiplication 

;; simple test, pos * pos => pos
(test (bn* (cons (- base 1) (cons (- base 1) bn0))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons 1 (cons 0 (cons (- base 2) (cons (- base 1) bn0)))))

;; simple test, pos * neg => neg
(test (bn* (cons (- base 1) (cons (- base 1) bn0))
           (cons (- base 1) (cons (- base 1) bn-1)))
      (cons 1 (cons 0 bn-1)))

;; simple test, neg * pos => neg     
(test (bn* (cons (- base 1) (cons (- base 1) bn-1))
           (cons (- base 1) (cons (- base 1) bn0)))
      (cons 1 (cons 0 bn-1)))

;; simple test, neg * neg => neg
(test (bn* (cons 1 bn-1)
           (cons (- base 1) (cons 0 bn-1)))
      (cons (- base 1) (cons 1 (cons (- base 2) bn0))))

(test (bn* -1 -1) 1) ;; (cons 1 bn0))

;; anything * 0 = 0
(test (bn* bn0 -1) 0) ;; bn0)
 
(test (bn* -1 bn0) 0) ;; bn0)

(test (bn* ($ 1234) ($ 56789)) ($ 70077626))

(test (bn* ($ -1234) ($ 56789)) ($ -70077626))

(test (bn* ($ 1234) ($ -56789)) ($ -70077626))

(test (bn* ($ -1234) ($ -56789)) ($ 70077626))



;; variadic multiplication

(test (var-bn* ($ 100) ($ 200) ($ 300) ($ 400)) ($ 2400000000))

(test (var-bn* ($ -100) ($ -200) ($ -300)) ($ -6000000))

(test (var-bn* ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) bn0)



;; quotient 

;; (quotient pos pos) => pos 
(test (bn-quotient (cons (- base 1) (cons (- base 1) bn0))
                   (cons 0 (cons (- base 1) bn0)))
      (cons 1 bn0))

;; (quotient neg pos) => neg
(test (bn-quotient (cons (- base 1) (cons 0 bn-1))
                   (cons 0 (cons (- base 1) bn0)))
      -1)

;; (quotient pos neg) => neg
(test (bn-quotient (cons 0 (cons (- base 1) bn0))
                   (cons 0 (cons (- base 1) bn-1)))
      (- (- base 1)))

;; (quotient neg neg) => pos
(test (bn-quotient (cons (- base 1) (cons 0 bn-1))
                   (cons 0 bn-1))
      (- base 1))

;; a and b equal => 1
(test (bn-quotient (cons (- base 1) (cons (- base 1) bn0))
                   (cons (- base 1) (cons (- base 1) bn0)))
      1)

(test (bn-quotient bn0 bn-1) bn0)

(test (bn-quotient bn-1 bn-1) (cons 1 bn0))


(test (bn-quotient (cons (- base 1) (cons (- base 1) (cons (- base 1) bn0)))
                   (cons (- base 1) (cons (- base 1) bn0)))
      (cons 0 (cons 1 bn0)))

(test (bn-quotient (cons (- base 1) (cons (- base 1) (cons (- base 1) bn0)))
                   (cons (- base 1) (cons 0 bn-1)))
      (cons 0 (cons (- base 1) bn-1)))

(test (bn-quotient (cons (- base 1) (cons 0 (cons 0 bn-1)))
                   (cons (- base 1) (cons (- base 1) bn0)))
      (cons 0 (cons (- base 1) bn-1)))

(test (bn-quotient (cons (- base 1) (cons 0 (cons 0 bn-1)))
                   (cons (- base 1) (cons 0 bn-1)))
      (cons 0 (cons 1 bn0)))

(test (bn-quotient ($ 123456) ($ 789)) ($ 156))

(test (bn-quotient ($ 123456) ($ -789)) ($ -156))

(test (bn-quotient ($ -123456) ($ 789)) ($ -156))

(test (bn-quotient ($ -123456) ($ -789)) ($ 156))
 

;; remainder

;; (remainder pos pos) => pos
(test (bn-remainder (cons (- base 1) (cons 0 (cons (- base 1) bn0)))
                    (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons 1 (cons 0 bn0))))

;; (remainder pos neg) => pos
(test (bn-remainder (cons (- base 1) (cons (- base 1) bn0))
                    (cons 0 (cons (- base 1) bn-1)))
      (- base 1))

;; (remainder neg pos) => neg
(test (bn-remainder (cons (- base 1) (cons (- base 1) (cons 0 bn-1)))
                    (cons (- base 1) (cons (- base 1) bn0)))
      (cons 0 bn-1))

;; (remainder neg neg) => neg
(test (bn-remainder (cons (- base 1) (cons (- base 1) (cons 0 bn-1)))
                    (cons (- base 1) (cons 0 bn-1)))
      (cons (- base 2) (cons 1 (cons (- base 1) bn-1))))

;; (remainder a b) where |a| == |b| => 0
(test (bn-remainder (cons (- base 1) bn0)
                    (cons (- base 1) bn-1))
      0)

(test (bn-remainder ($ 123456) ($ 789)) ($ 372))

(test (bn-remainder ($ 123456) ($ -789)) ($ 372))

(test (bn-remainder ($ -123456) ($ 789)) ($ -372))

(test (bn-remainder ($ -123456) ($ -789)) ($ -372))



;; modulo 

;; (modulo pos pos) => pos
(test (bn-modulo (cons (- base 1) (cons 0 (cons (- base 1) bn0)))
                 (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons 1 bn0)))

;; (modulo pos neg) => neg
(test (bn-modulo (cons 1 (cons (- base 2) bn0))
                 (cons 0 (cons 1 bn-1)))
      (cons 1 (cons (- base 1) bn-1)))

;; please ignore this
(if (> base 2)
    ;; (modulo neg pos) => pos
    (test (bn-modulo (cons 0 (cons 0 (cons (- base 1) bn-1)))
                     (cons 0 (cons 1 bn0)))
          (cons 0 (cons 1 bn0))))

;; (modulo a b), a,b < 0, |a| > |b| => neg
(test (bn-modulo (cons (- base 1) (cons (- base 1) (cons 0 bn-1)))
                 (cons (- base 1) (cons 0 bn-1)))
      (cons (- base 2) (cons 1 (cons (- base 1) bn-1))))

;; (modulo a b), a,b < 0, |a| = |b| => 0
(test (bn-modulo bn-1 bn-1) bn0)

;; division by 0
;; (test (bn-modulo bn0 bn0) error)

(test (bn-modulo ($ 123456) ($ 789)) ($ 372))

(test (bn-modulo ($ 123456) ($ -789)) ($ -417))

(test (bn-modulo ($ -123456) ($ 789)) ($ 417))

(test (bn-modulo ($ -123456) ($ -789)) ($ -372))



;; equality

(test2 (bn= bn0 bn0) #t) 

(test2 (bn= (cons (- base 1) bn-1) (cons (- base 1) bn-1)) #t)

(test2 (bn= bn0 (cons (- base 1) bn-1)) #f)

(test2 (bn= (cons (- base 1) bn-1) bn0) #f)

(test2 (bn= (cons (- base 1) bn0)
            (cons (- base 1) bn0))
       #t)

(test2 (bn= (cons (- base 1) bn0)
            (cons 0 bn0))
       #f)

(test2 (bn= ($ 100) ($ 100)) #t)

(test2 (bn= ($ -100) ($ -100)) #t)

(test2 (bn= ($ 100) ($ -100)) #f)



;; variadic equality

(test2 (var-bn= ($ 500) ($ 500) ($ 500) ($ 300) ($ 500)) #f)

(test2 (var-bn= ($ 3000) ($ 3000) ($ 3000)) #t)

(test2 (var-bn= ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) #t)

(test2 (var-bn= -1 (bn-norm (cons (- base 1) bn-1)) (bn-neg 1)) #t)



;; less than

(test2 (bn< bn0 bn0) #f)

(test2 (bn< (cons (- base 1) bn-1) (cons (- base 1) bn-1)) #f)

(test2 (bn< bn0 bn-1) #f)

(test2 (bn< (cons (- base 1) bn-1) bn0) #t)

(test2 (bn< (cons (- base 1) bn0) bn0) #f)

(test2 (bn< bn0 (cons (- base 1) bn0)) #t)

(test2 (bn< (cons 0 (cons (- base 1) bn-1))
            (cons 0 (cons (- base 1) bn0)))
       #t)

(test2 (bn< (cons 0 (cons (- base 1) bn0))
            (cons 0 (cons (- base 1) bn-1)))
       #f)

(test2 (bn< (cons 0 (cons (- base 1) bn0))
            (cons (- base 1) (cons (- base 1) bn0)))
       #t)

(test2 (bn< (cons 1 bn-1) (cons 2 bn-1)) #t)

(test2 (bn< ($ 100) ($ 100)) #f)

(test2 (bn< ($ -100) ($ 100)) #t)

(test2 (bn< ($ 100) ($ -100)) #f)



;; variadic less than

(test2 (var-bn< ($ 100) ($ 200) ($ 300) ($ 400)) #t)

(test2 (var-bn< ($ -100) ($ -200) ($ -300) ($ -400)) #f)



;; less or equal

(test2 (bn<= bn0 bn0) #t)

(test2 (bn<= (cons (- base 1) bn-1) (cons (- base 1) bn-1)) #t)

(test2 (bn<= bn0 (cons (- base 1) bn-1)) #f)

(test2 (bn<= (cons (- base 1) bn-1) bn0) #t)

(test2 (bn<= ($ -100) ($ 101)) #t)

(test2 (bn<= ($ -100) 0) #t)

(test2 (bn<= ($ -100) ($ 100)) #t)

(test2 (bn<= ($ 100) ($ 100)) #t)

(test2 (bn<= ($ 100) ($ -100)) #f)



;; variadic less or equal

(test2 (var-bn<= ($ 100) ($ 200) ($ 300) ($ 400)) #t)

(test2 (var-bn<= ($ -100) ($ -200) ($ -300) ($ -400)) #f)

(test2 (var-bn<= ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) #t)



;; greater than

(test2 (bn> bn0 bn0) #f)

(test2 (bn> (cons (- base 1) bn-1) (cons (- base 1) bn-1)) #f)

(test2 (bn> bn0 (cons (- base 1) bn-1)) #t)

(test2 (bn> (cons (- base 1) bn-1) bn0) #f)

(test2 (bn> ($ -100) ($ 101)) #f)

(test2 (bn> ($ -100) 0) #f)

(test2 (bn> ($ -100) ($ 100)) #f)

(test2 (bn> ($ 100) ($ 100)) #f)

(test2 (bn> ($ 101) ($ -100)) #t)



;; variadic greater than

(test2 (var-bn> ($ 100) ($ 200) ($ 300) ($ 400)) #f)

(test2 (var-bn> ($ -100) ($ -200) ($ -300) ($ -400)) #t)

(test2 (var-bn> ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) #f)



;; greater or equal

(test2 (bn>= bn0 bn0) #t)

(test2 (bn>= (cons (- base 1) bn-1) (cons (- base 1) bn-1)) #t)

(test2 (bn>= bn0 (cons (- base 1) bn-1)) #t)

(test2 (bn>= (cons (- base 1) bn-1) bn0) #f)

(test2 (bn>= ($ -100) ($ 101)) #f)

(test2 (bn>= ($ -100) 0) #f)

(test2 (bn>= ($ -100) ($ 100)) #f)

(test2 (bn>= ($ 100) ($ 100)) #t)

(test2 (bn>= ($ 101) ($ -100)) #t)



;; variadic greater than

(test2 (var-bn>= ($ 100) ($ 200) ($ 300) ($ 400)) #f)

(test2 (var-bn>= ($ -100) ($ -200) ($ -300) ($ -400)) #t)

(test2 (var-bn>= ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) #t)



;; negation

(test (bn-neg bn0) bn0)

(test (bn-neg 0) 0)

(test (bn-neg (cons (- base 1) bn-1)) 1)

(test (bn-neg (cons 1 bn0)) -1)

(test (bn-neg ($ 123456789)) ($ -123456789))

(test (bn-neg ($ -123456789)) ($ 123456789))



;; bitwise not

(test (bn~ 0) -1)

(test (bn~ ($ 10000)) ($ -10001))

(test (bn~ ($ -10000)) ($ 9999))



;; bn-zero?

(test2 (bn-zero? 0) #t)

(test2 (bn-zero? (- 0 base)) #f)

(test2 (bn-zero? (+ 0 base)) #f)



;; bn-positive?

(test2 (bn-positive? 0) #f)

(test2 (bn-positive? ($ -1000)) #f)

(test2 (bn-positive? ($ 1000)) #t)



;; bn-negative?

(test2 (bn-negative? 0) #f)

(test2 (bn-negative? ($ -1000)) #t)

(test2 (bn-negative? ($ 10000)) #f)



;; bn-even?

(test2 (bn-even? ($ 100)) #t)

(test2 (bn-even? ($ -100)) #t)

(test2 (bn-even? ($ 101)) #f)

(test2 (bn-even? ($ -101)) #f)



;; bn-odd?

(test2 (bn-odd? ($ 100)) #f)

(test2 (bn-odd? ($ -100)) #f)

(test2 (bn-odd? ($ 101)) #t)

(test2 (bn-odd? ($ -101)) #t)



;; bn-max

(test (bn-max ($ 0) bn0) ($ 0))

(test (bn-max ($ 0) ($ 100)) ($ 100))

(test (bn-max ($ 0) ($ -100)) ($ 0))



;; variadic max

(test (var-bn-min ($ 0) ($ 0) ($ 0) ($ 0) bn0) ($ 0))

(test (var-bn-max ($ 100) ($ 200) ($ 300) ($ 400) ($ 500)) ($ 500))

(test (var-bn-max ($ -100) ($ -200) ($ -300) ($ -400) ($ -500)) ($ -100))



;; bn-min

(test (bn-min ($ 0) bn0) ($ 0))

(test (bn-min ($ 0) ($ 100)) ($ 0))

(test (bn-min ($ 0) ($ -100)) ($ -100))



;; variadic min

(test (var-bn-min ($ 0) ($ 0) ($ 0) ($ 0) bn0) ($ 0))

(test (var-bn-min ($ 100) ($ 200) ($ 300) ($ 400) ($ 500)) ($ 100))

(test (var-bn-min ($ -100) ($ -200) ($ -300) ($ -400) ($ -500)) ($ -500))



;; bn-abs

(test (bn-abs bn0) 0)

(test (bn-abs ($ 100)) ($ 100))

(test (bn-abs ($ -100)) ($ 100))



;; bn-gcd

(test (bn-gcd 0 0) 0)

(test (bn-gcd ($ 18) ($ 24)) ($ 6))

(test (bn-gcd ($ -18) ($ 24)) ($ 6))

(test (bn-gcd ($ 18) ($ -24)) ($ 6))

(test (bn-gcd ($ -18) ($ -24)) ($ 6))

(test (bn-gcd ($ 7) ($ 11)) ($ 1))



;; variadic bn-gcd

(test (var-bn-gcd 0 0 0 0 0 0) 0)

(test (var-bn-gcd ($ 18) ($ 24) ($ 36)) ($ 6))

(test (var-bn-gcd ($ -18) ($ 24) ($ -36)) ($ 6))

(test (var-bn-gcd ($ 18) ($ -24) ($ -36)) ($ 6))

(test (var-bn-gcd ($ -18) ($ -24) ($ -36)) ($ 6))

(test (var-bn-gcd ($ 7) ($ 11) ($ 13)) ($ 1))



;; bn-lcm

(test (bn-lcm ($ 3) ($ 3)) ($ 3))

(test (bn-lcm ($ 18) ($ 24)) ($ 72))

(test (bn-lcm ($ -18) ($ 24)) ($ 72))

(test (bn-lcm ($ 18) ($ -24)) ($ 72))

(test (bn-lcm ($ -18) ($ -24)) ($ 72))



;; variadic bn-lcm

(test (var-bn-lcm ($ 3) ($ 3) ($ 3) ($ 3) ($ 3)) ($ 3))

(test (var-bn-lcm ($ 18) ($ 24) ($ 6)) ($ 72))

(test (var-bn-lcm ($ -18) ($ 24) ($ 36)) ($ 72))

(test (var-bn-lcm ($ 18) ($ -24) ($ -6)) ($ 72))

(test (var-bn-lcm ($ -18) ($ -24) ($ -36)) ($ 72))
