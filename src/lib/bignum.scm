;;; bignum.scm

;; bignums operations (two's complement, little endian representation)

;; fixnum? and bignum? checks  [x] 
;; fixnum->bignum              [x]
;; bignum->fixnum              [ ]
;; bn-encode                   [x]

;; fold (bn-fold)              [x] 
;; bn-scan-until-false         [x]
;; map (bn-map)                [x]

;; normalization (bn-norm)     [x] 

;; fixnum number->string       [ ]
;; bn-number->string           [ ]
;; fixnum string->number       [ ]
;; bn-string->number           [ ]

;; addition (bn+)              [x] 
;; variadic addition           [x]
;; substraction (bn-)          [x]
;; variadic substraction?      [ ]
;; unary substraction (bn-u)   [x]

;; multiplication (bn*)        [x] need a better algorithm
;; variadic multiplication     [x]

;; quotient (bn-quotient)      [x] need a better algorithm
;; fixnum remainder            [x]
;; remainder (bn-remainder)    [x]
;; fixnum modulo               [x]
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
;; ...other bitwise operators? [ ]

;; bn-abs                      [x]
;; bn-zero?                    [x]
;; bn-positive?                [x]
;; bn-negative?                [x]
;; bn-even?                    [x]
;; bn-odd?                     [x]
;; bn-max                      [x]
;; variadic max                [x]
;; bn-min                      [x]
;; variadic min                [x]
;; fixnum gcd (##gcd)          [x]
;; bn-gcd                      [x]
;; variadic gcd                [x]
;; fixnum lcm (##lcm)          [x]
;; bn-lcm                      [x]
;; variadic lcm                [x]


;; Maybe add a cond-expand to detect which library is imported so that we
;; can remove procedures that aren't necessary?

;; IGNORE THIS FOR NOW:
;;  - the max-fixnum check for bignum->fixnum
;;  - string->number and number->string (not adapted yet)


;;------------------------------------------------------------------------------

;; fixnum operators, can't assume they will be imported
;; and needed for `bn-encode` and `bn+`, among others

(define (##remainder x y)
  (##- x (##* y (##quotient x y))))

(define (##modulo x y)
  (let ((q (##quotient x y)))
    (let ((r (##- x (##* y q))))
      (if (##eqv? r 0)
          0
          (if (##eqv? (##< x 0) (##< y 0))
              r
              (##+ r y))))))


;;------------------------------------------------------------------------------

;; utilities

;;(define-feature _ (use scheme-bignum))

(define bignum-type 7)

(define (bn-cons car cdr)
  (##rib car cdr bignum-type))

(define bn-digit ##field0)

(define bn-next ##field1)


(define base (##+ 1023 1)) ;; `base` should be represented as a fixnum

(define max-fixnum 4096) ;; (##* 4 (##+ 1 1023))) ;; FIXME

(define bn0 ##bn0)

(define bn-1 ##bn-1)

(define (end? n)
  (or (##eqv? n bn0) (##eqv? n bn-1)))

(define bn1 (bn-cons 1 bn0))


(define (fixnum? n)
  (not (##rib? n)))

(define (var-fixnum? lst)
  (cond ((null? lst) #f)
	((null? (##field1 lst)) (fixnum? (##field0 lst)))
	(else (and (fixnum? (##field0 lst))
		   (var-fixnum? (##field1 lst))))))

(define (bignum? n)
  (and (##rib? n) (##eqv? (##field2 n) bignum-type)))

(define (number? n)
  (or (fixnum? n) (bignum? n)))


(define (fixnum->bignum n)
  (if (fixnum? n)
      (if (and (##< n base) (##< (##- 0 (##+ base 1)) n))
	  (cond ((##eqv? 0 n) bn0)
		((##eqv? -1 n) bn-1)
		((##< 0 n) (bn-cons n bn0))         ;; positive fixnum
		(else (bn-cons (##+ base n) bn-1))) ;; negative fixnum
	  (bn-encode n))
      (if (bignum? n)
          n
          #f)))

(define (bignum->fixnum n)
  (if (bignum? n)
      (if (end? (bn-next n))
          (if (##eqv? bn0 (bn-next n))
              (bn-digit n)
              (if (##eqv? 0 (bn-digit n))  ;; (0 $-1 $-1 ...) is not a fixnum
                  #f 
                  (##- 0 (bn-neg n))))
          #f) ;; conversion is not possible
      (if (and (fixnum? n) (##< n base) (##< (##- 0 (##+ base 1)) n))
          n
          #f)))

;; (define (bignum->fixnum n)
;;   (display-rib n 3)
;;   (if (bignum? n)
;;       (if (end? (bn-next n))
;;           (if (##eqv? bn0 (bn-next n))
;;               (bn-digit n)
;;               (if (##eqv? 0 (bn-digit n))  ;; (0 $-1 $-1 ...) is not a fixnum
;;                   base
;;                   (##- 0 (bn-neg n))))
;;           (if (##bn< n max-fixnum) ;; conversion is not possible
;; 	      (##+ (bn-digit n) (##* base (bignum->fixnum (bn-next n))))))
;;       (if (and (fixnum? n) (##< n max-fixnum) (##< (##- (##+ max-fixnum 1)) n))
;;           n
;;           #f)))


(define (bn-encode n)
  
  (define (_bn-encode n)
    (if (and (##< (##- (##- 0 1) base) n) (##< n base))
        (bn-cons n bn0)
        (bn-cons (##remainder n base) (_bn-encode (##quotient n base)))))

  (if (bignum? n)
      n
      (let ((_n (_bn-encode (if (##< n 0) (##- 0 n) n))))
	(if (##< (##- 0 1) n) _n (##bn-neg _n)))))

;; helper function to encode bignums

(define $ bn-encode)


(define (bn-fold fn base lst)
  (if (null? lst)
      base
      (bn-fold fn (fn (##field0 lst) base) (##field1 lst))))

(define (bn-scan-until-false fn base state lst)
  (if (and (not (null? lst)) state)
      (bn-scan-until-false fn (##field0 lst)
			   (fn base (##field0 lst))
			   (##field1 lst))
      state))

(define (bn-map fn lst)
  (if (null? lst)
      '()
      (##rib (fn (##field0 lst)) (bn-map fn (##field1 lst)) 0)))


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
    (if (##eqv? (bn-digit (bn-next _p1)) (bn-digit _p2)) 
        (bn-cons (bn-digit _p1) (_bn-norm (bn-next _p1) _p2)) 
        (bn-cons (bn-digit _p1) (__bn-norm (bn-next _p1) _p2))))

  (define (_bn-norm p1 p2)
    (if (##eqv? (bn-digit p1) (bn-digit p2))
        (if (end? p2)
            p2
            (_bn-norm p1 (bn-next p2))) ;; find next 
        (__bn-norm p1 p2)))         ;; memorize prev
  
  (if (fixnum? n)
      (if (and (##< n base) (##< (##- 0 (##+ base 1)) n))
          n
          (fixnum->bignum n))
      (let ((_n (_bn-norm n (bn-next n))))
        (or (bignum->fixnum _n) _n)))) ;; return a fixnum if possible


;; ;;------------------------------------------------------------------------------

;; ;; FIXME incomplete and redundant type checks 

;; ;; number->string

;; (define (bn-number->string a)
;;   (if (fixnum? a)
;;       (number->string a)
;;       (##bn-number->string (fixnum->bignum a))))

;; (define (##bn-number->string a)

;;   (define (##bn-number->string-aux _a radix)
;;     (let* ((q (bn-quotient _a radix))
;;            (d (bn-remainder _a radix)) 
;;            (c (+ 48 d))) 
;;       (cons c (if (bn= bn0 q)
;;                   '()
;;                   (##bn-number->string-aux q radix)))))
  
;;   (let ((chars (##bn-number->string-aux (##bn-abs a) (fixnum->bignum 10))))
;;     (if (##bn< a bn0)
;;         (list->string (cons (integer->char 45) ;; negative number, 45 = "#\-"
;;                             (reverse (map integer->char chars)))) 
;;         (list->string (reverse (map integer->char chars))))))


;; ;; string->number

;; (define (bn-string->number str)

;;   (define (convert c) ;; decimal only
;;     (let ((_c (char->integer c)))
;;       (if (and (< 47 _c) (< _c 58))
;;           (- _c 48)
;;           #f)))

;;   (define (bn-string->number-aux lst n)
;;     (if (pair? lst)
;;         (let* ((c (car lst))
;;                (x (convert c)))
;;           (if x
;;               (bn-string->number-aux (cdr lst) (bn+ (bn* 10 n) x))
;;               #f))
;;         n))

;;   (let ((lst (string->list str)))
;;     (if (null? lst)
;;         #f
;;         (if (equal? (car lst) (integer->char 45))
;;             (let ((n (bn-string->number-aux (cdr lst) 0)))
;;               (and n (bn-u n)))
;;             (bn-string->number-aux lst 0)))))


;;------------------------------------------------------------------------------

;; addition

(define (bn+ a b)
  (if (and (fixnum? a) (fixnum? b))
      (bn-norm (##+ a b))
      (bn-norm (##bn+ (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn+ a b) ;; no normalization, assumes a and b are bignums
  
  (define (_bn+ a b carry)
    (if (and (end? a) (end? b))
        (if (##eqv? a b)
            (if (##eqv? 0 carry)
                (if (##eqv? a bn-1) ;; (if (eq? a bn0) bn0 bn-1)
                    (bn-cons (##- base 2) bn-1) 
                    bn0)
                (if (##eqv? a bn-1)
                    bn-1
                    (bn-cons carry (_bn+ (bn-next a) (bn-next b) 0))))
            (if (##eqv? 0 carry) bn-1 bn0))
        (let* ((_a (bn-digit a))
               (_b (bn-digit b))
               (res (##+ _a (##+ _b carry)))
               (rem (##modulo res base))
               (quo (##quotient res base)))
          (bn-cons rem (_bn+ (bn-next a) (bn-next b) quo)))))

  (_bn+ a b 0))

(define (var-bn+ . args) ;; note: (var-bn+) => 0, consistent with gambit's `+`
  (if (var-fixnum? args)
      (bn-norm (bn-fold ##+ 0 args))
      (bn-norm (bn-fold ##bn+ bn0 (bn-map fixnum->bignum args)))))


;; substraction

(define (bn- a b)
  (if (and (fixnum? a) (fixnum? b))
      (bn-norm (##- a b))
      (bn-norm (##bn- (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn- a b)
  (##bn+ a (##bn-neg b)))


;; unary substraction

(define (bn-u a)
  (if (fixnum? a)
      (##- 0 a) ;; no need to normalize the result
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
  (if (and (fixnum? a) (fixnum? b))
      (bn-norm (##* a b))
      (bn-norm (##bn* (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn* a b)
  
  (define (_bn* a b carry)  ;; one line multiplication
    (if (end? a)
        (if (##eqv? 0 carry) bn0 (bn-cons carry bn0))
        (let* ((_a (bn-digit a))
               (_b (bn-digit b))
               (res (##+ (##* _a _b) carry))
               (rem (##modulo res base))
               (quo (##quotient res base)))
          (bn-cons rem (_bn* (bn-next a) b quo)))))
  
  (define (__bn* a b) ;; full (positive) mutiplication
    (if (end? b)
        bn0 
        (let ((res (_bn* a b 0))
              (pad (bn-cons 0 a)))
          (##bn+ res (__bn* pad (bn-next b))))))

  (let ((_a (##bn-abs a))
        (_b (##bn-abs b)))
    (if (##eqv? (##bn< a bn0) (##bn< b bn0))  
        (__bn* _a _b)
        (##bn-neg (__bn* _a _b)))))

(define (var-bn* . args) ;; note: (var-bn*) => 1, consistent with gambit's `*`
  (if (var-fixnum? args)
      (bn-norm (bn-fold ##* 1 args))
      (bn-norm (bn-fold ##bn* bn1 (bn-map fixnum->bignum args)))))


;;------------------------------------------------------------------------------

;; quotient

;; FIXME quotient is very slow and makes bn-number->string too slow to be usable

(define (bn-quotient a b)
  (if (and (fixnum? a) (fixnum? b))
      (##quotient a b) ;; no need to normalize the result
      (let ((_a (fixnum->bignum a))
            (_b (fixnum->bignum b)))
        (if (##eqv? _b bn0)   ;; division by 0
            (##quotient 0 0) 
            (bn-norm (##bn-quotient _a _b))))))

(define (##bn-quotient a b)

  (define (_bn-quotient _a _b quo)
    (if (##bn< _a _b)
        quo
        (_bn-quotient (##bn- _a _b) _b (##bn+ quo bn1))))

  (let ((_a (##bn-abs a))
        (_b (##bn-abs b)))
    (if (##eqv? (##bn< bn0 a) (##bn< bn0 b))
        (_bn-quotient _a _b bn0)
        (##bn-neg (_bn-quotient _a _b bn0)))))


;; remainder

(define (bn-remainder a b)
  (if (and (fixnum? a) (fixnum? b))
      (##remainder a b) ;; no need to normalize the result
      (bn-norm (##bn-remainder (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-remainder a b)
  (##bn- a (##bn* b (##bn-quotient a b))))


;; modulo

(define (bn-modulo a b)
  (if (and (fixnum? a) (fixnum? b))
      (##modulo a b) ;; no need to normalize the result
      (bn-norm (##bn-modulo (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-modulo a b)
  (let ((r (##bn-remainder a b)))
    (if (##eqv? r bn0)
        bn0
        (if (##eqv? (##bn< a bn0) (##bn< b bn0))
            r
            (##bn+ r b)))))

;;------------------------------------------------------------------------------

;; equality

(define (bn= a b)
  (if (and (fixnum? a) (fixnum? b))
      (##eqv? a b)
      (##bn= a b)))

(define (##bn= a b) 
  (##eqv? (bn- a b) 0)) ;; FIXME tests won't work with (bn-norm (##bn- a b) bn0)

(define (var-bn= a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-scan-until-false ##eqv? a #t rest)
      (bn-scan-until-false
       ##bn= (fixnum->bignum a) #t (bn-map fixnum->bignum rest))))


;; less than

(define (bn< a b)
  (if (and (fixnum? a) (fixnum? b))
      (##< a b)
      (##bn< (fixnum->bignum a) (fixnum->bignum b))))

(define (##bn< a b)
  (##bn-negative? (##bn- a b)))

(define (var-bn< a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-scan-until-false ##< a #t rest)
      (bn-scan-until-false
       ##bn< (fixnum->bignum a) #t (bn-map fixnum->bignum rest))))


;; less or equal

(define (bn<= a b)
  (if (and (fixnum? a) (fixnum? b))
      (or (##eqv? a b) (##< a b))
      (##bn<= (fixnum->bignum a) (fixnum->bignum b))))

(define (##bn<= a b)
  (or (##bn= a b) (##bn< a b)))

(define (var-bn<= a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-scan-until-false (lambda (x y) (or (##eqv? x y) (##< x y))) a #t rest)
      (bn-scan-until-false
       ##bn<= (fixnum->bignum a) #t (bn-map fixnum->bignum rest))))


;; greater than

(define (bn> a b)
  (if (and (fixnum? a) (fixnum? b))
      (##< b a)
      (##bn> (fixnum->bignum a) (fixnum->bignum b))))

(define (##bn> a b)
  (##bn< b a))

(define (var-bn> a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-scan-until-false (lambda (x y) (##< y x)) a #t rest)
      (bn-scan-until-false
       ##bn> (fixnum->bignum a) #t (bn-map fixnum->bignum rest))))


;; greater or equal

(define (bn>= a b)
  (if (and (fixnum? a) (fixnum? b))
      (or (##eqv? a b) (##< b a))
      (##bn>= (fixnum->bignum a) (fixnum->bignum b))))

(define (##bn>= a b)
  (or (##bn= a b) (##bn< b a)))

(define (var-bn>= a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-scan-until-false (lambda (x y) (or (##eqv? x y) (##< y x))) a #t rest)
      (bn-scan-until-false
       ##bn>= (fixnum->bignum a) #t (bn-map fixnum->bignum rest))))


;;------------------------------------------------------------------------------

;; bitwise operations

(define (bn~ a)
  (if (fixnum? a)
      ;; FIXME need to implement bitwise-not in ribbit
      (bn-norm (##bn~ (fixnum->bignum a))) ;; (bn-norm (bitwise-not a))
      (bn-norm (##bn~ (fixnum->bignum a)))))

(define (##bn~ a)
  (if (end? a)
      (if (##eqv? a bn0) bn-1 bn0)
      (let ((_a (##- (##- base 1) (bn-digit a))))
        (bn-cons _a (##bn~ (bn-next a))))))


;;------------------------------------------------------------------------------

;; negation

(define (bn-neg a)
  (if (fixnum? a)
      (##- 0 a) 
      (bn-norm (##bn-neg (fixnum->bignum a)))))

(define (##bn-neg a)
  (##bn+ (##bn~ a) bn1))


;; absolute value

(define (bn-abs a)
  (if (fixnum? a)
      (bn-norm (if (##< a 0) (##- 0 a) a))
      (bn-norm (##bn-abs a))))

(define (##bn-abs a)
  (if (##bn< a bn0) (##bn- bn0 a) a))


;; zero?

(define (bn-zero? a)
  (if (fixnum? a)
      (##eqv? 0 a)
      (##bn-zero? (fixnum->bignum a))))

(define (##bn-zero? a)
  (##bn= a bn0))


;; positive?

(define (bn-positive? a)
  (if (fixnum? a)
      (##< 0 a)
      (##bn-positive? (fixnum->bignum a))))

(define (##bn-positive? a)
  (##bn< bn0 a))


;; negative?

(define (bn-negative? a)
  (if (fixnum? a)
      (##< a 0)
      (##bn-negative? (fixnum->bignum a))))

(define (##bn-negative? a)
  (if (##eqv? a bn0)
      #f
      (or (##eqv? a bn-1) (##bn-negative? (bn-next a)))))


;; even?

(define (bn-even? a)
  (if (fixnum? a)
      (##eqv? 0 (##modulo a 2))
      (##bn-even? (fixnum->bignum a))))

(define (##bn-even? a)
  (##eqv? 0 (##modulo (bn-digit a) 2)))


;; odd?

(define (bn-odd? a)
  (if (fixnum? a)
      (##eqv? 1 (##modulo a 2))
      (##bn-odd? (fixnum->bignum a))))

(define (##bn-odd? a)
  (##eqv? 1 (##modulo (bn-digit a) 2)))


;; maximum

(define (bn-max a b)
  (if (and (fixnum? a) (fixnum? b))
      (if (##< a b) b a)
      (bn-norm (##bn-max (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-max a b)
  (if (##bn< a b) b a))

(define (var-bn-max a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-fold (lambda (x y) (if (##< x y) y x)) a rest)
      (bn-norm (bn-fold
                ##bn-max (fixnum->bignum a) (bn-map fixnum->bignum rest)))))


;; mininum

(define (bn-min a b)
  (if (and (fixnum? a) (fixnum? b))
      (if (##< a b) a b)
      (bn-norm (##bn-min (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-min a b)
  (if (##bn< a b) a b))

(define (var-bn-min a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-fold (lambda (x y) (if (##< x y) x y)) a rest)
      (bn-norm (bn-fold
                ##bn-min (fixnum->bignum a) (bn-map fixnum->bignum rest)))))


;; gcd

(define (##gcd a b)
  (let ((_a (abs a))
	(_b (abs b)))
    (if (##< _a _b) (##gcd-aux _a _b) (##gcd-aux _b _a))))

(define (##gcd-aux a b)
  (if (##eqv? a 0)
      b
      (##gcd-aux (##remainder b a) a)))

(define (bn-gcd a b)
  (if (and (fixnum? a) (fixnum? b))
      (##gcd a b)
      (bn-norm (##bn-gcd (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-gcd a b)
  (let ((_a (##bn-abs a))
        (_b (##bn-abs b)))
    (if (##bn< _a _b) (##bn-gcd-aux _a _b) (##bn-gcd-aux _b _a))))

(define (##bn-gcd-aux a b)
  (if (##bn= a bn0) 
      b
      (##bn-gcd-aux (##bn-remainder b a) a)))

(define (var-bn-gcd . args) ;; (var-bn-gcd) => 0, consistent with gambit's `gcd`
  (if (var-fixnum? args)
      (bn-fold ##gcd 0 args)
      (bn-norm (bn-fold ##bn-gcd bn0 (bn-map fixnum->bignum args)))))
           

;; lcm

(define (##lcm a b)
  (if (##eqv? b 0)
      0
      (let ((_a (abs a))
	    (_b (abs b)))
        (##* (##quotient _a (##gcd _b)) _b))))

(define (bn-lcm a b)
  (if (and (fixnum? a) (fixnum? b))
      (bn-norm (##lcm a b))
      (bn-norm (##bn-lcm (fixnum->bignum a) (fixnum->bignum b)))))

(define (##bn-lcm a b)
  (if (##bn= b bn0)
      bn0
      (let ((_a (##bn-abs a))
            (_b (##bn-abs b)))
        (##bn* (##bn-quotient _a (##bn-gcd _a _b)) _b))))

(define (var-bn-lcm . args) ;; (var-bn-lcm) => 1, consistent with gambit's `lcm`
  (if (var-fixnum? args)
      (bn-norm (bn-fold ##lcm 1 args))
      (bn-norm (bn-fold ##bn-lcm bn1 (bn-map fixnum->bignum args)))))


;;------------------------------------------------------------------------------

;; (if-feature no-variadics
;;    (begin
;;      (define +    bn+)
;;      (define -    bn-)
;;      (define *    bn*)
;;      (define =    bn=)
;;      (define <    bn<)
;;      (define <=   bn<=)
;;      (define >    bn>)
;;      (define >=   bn>=)
;;      (define max  bn-max)
;;      (define min  bn-min)
;;      (define gcd  bn-gcd) 
;;      (define lcm  bn-lcm))
;;    (begin
;;      (define +    var-bn+)
;;      (define -    var-bn-)
;;      (define *    var-bn*)
;;      (define =    var-bn=)
;;      (define <    var-bn<)
;;      (define <=   var-bn<=)
;;      (define >    var-bn>)
;;      (define >=   var-bn>=)
;;      (define max  var-bn-max)
;;      (define min  var-bn-min)
;;      (define gcd  var-bn-gcd)
;;      (define lcm  var-bn-lcm)))

(define +    bn+)
(define -    bn-)
(define *    bn*)
(define =    bn=)
(define <    bn<)
(define <=   bn<=)
(define >    bn>)
(define >=   bn>=)
(define max  bn-max)
(define min  bn-min)
(define gcd  bn-gcd) 
(define lcm  bn-lcm)

(define quotient  bn-quotient) 
(define remainder bn-remainder)
(define modulo    bn-modulo)
(define zero?     bn-zero?)
(define positive? bn-positive?)
(define negative? bn-negative?)
(define even?     bn-even?)
(define odd?      bn-odd?)
(define abs       bn-abs)


;;------------------------------------------------------------------------------

(define (##display-rib rib depth)
  (if (> depth 0)
    (begin
      (display "[")
      (cond ((not (##rib? rib))
             (display rib))
            ((##rib? (##field0 rib))
             (##display-rib (##field0 rib) (##- depth 1)))
            (else
              (display (##field0 rib))))
      (display " ")
      (cond ((not (##rib? rib))
             (display rib))
            ((##rib? (##field1 rib))
             (##display-rib (##field1 rib) (##- depth 1)))
            (else
              (display (##field1 rib))))
      (display " ")
      (cond ((not (##rib? rib))
             (display rib))
            ((##rib? (##field2 rib))
             (##display-rib (##field2 rib) (##- depth 1)))
            (else
              (display (##field2 rib))))
      (display "]"))
    (display "...")))

(define (display-rib rib depth)
  (##display-rib rib depth)
  (newline))

(define (test a b)
  (if (not (bn= a b))
      (begin
        (display "results not matching: ") 
        (display "a: ") (display-rib a 5)
	(display "b: ") (display-rib b 5)
        (newline))
      ;;(_pp a)))
      (display "test passed...\n")
      ))

(define (test2 a b)
  (if (not (equal? a b))
      (begin
        (display "results not matching: ") (newline)
        (display "a: ") (display a) (newline)
        (display "b: ") (display b) (newline)
	)
      (display "test passed...\n")))


;;------------------------------------------------------------------------------

;; unit tests


;; normalization

;; excessive symbols at the end + list starts with repetition
(test (bn-norm (bn-cons 0 (bn-cons 0 (bn-cons (- base 1) (bn-cons (- base 1) bn-1)))))
      (bn-cons 0 (bn-cons 0 bn-1)))

;; no symbols to strip off
(test (bn-norm (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons (- base 1) (bn-cons (- base 1) bn0)))

;; positive fixnum
(test (bn-norm (bn-cons (- base 1) (bn-cons 0 (bn-cons 0 bn0))))
      (- base 1))

;; negative fixnum
(if (< 2 base)
    (test (bn-norm (bn-cons (- base 2) bn-1))
          -2))

;; not a fixnum (should not return 0)
(test (bn-norm (bn-cons 0 (bn-cons (- base 1) (bn-cons (- base 1) bn-1))))
      (bn-cons 0 bn-1))

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
(test (fixnum->bignum -1) (bn-cons (- base 1) bn-1))

;; fixnum->bignum: already a bignum
(test (fixnum->bignum (bn-cons (- base 1) bn0)) (bn-cons (- base 1) bn0))

;; fixnum->bignum: invalid conversion
(test2 (fixnum->bignum #t) #f)

;; bignum->fixnum: simple case positive
(test (bignum->fixnum (bn-cons (- base 1) bn-1)) -1)

;; bignum->fixnum: simple case negative
(test (bignum->fixnum (bn-cons (- base 1) bn0)) (- base 1))

;; bignum->fixnum: already a fixnum
(test (bignum->fixnum -1) -1)

;; bignum->fixnum: invalid conversion
(test2 (bignum->fixnum base) #f)



;; ;; number->string

;; (test2 (bn-number->string ($ 0)) "0")

;; (test2 (bn-number->string ($ -1)) "-1")

;; (test2 (bn-number->string ($ 100)) "100")

;; (test2 (bn-number->string ($ -100)) "-100")

;; (test2 (bn-number->string ($ 100000)) "100000")

;; (test2 (bn-number->string ($ -100000)) "-100000")
 


;; ;; string->number

;; (test (bn-string->number "0") bn0)

;; (test (bn-string->number "-1") bn-1)

;; (test (bn-string->number "100") ($ 100))

;; (test (bn-string->number "-100") ($ -100))

;; (test (bn-string->number "1000000000") ($ 1000000000))

;; (test (bn-string->number "-1000000000") ($ -1000000000))

;; (test2 (bn-string->number "") #f)



;; addition

;; different parity, carry (pos res)
(test (+ (bn-cons (- base 1) bn-1)
           (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons (- base 2) (bn-cons (- base 1) bn0)))

;; different parity, carry (res = 0)
(test (+ (bn-cons (- base 1) bn-1)
           (bn-cons (- base 1) bn0))
      (- base 2))

;; different parity, no carry (neg res)
(test (+ (bn-cons 0  bn-1)
           (bn-cons (- base 1) bn0))
      -1)

;; same parity (pos), carry
(test (+ (bn-cons (- base 1) (bn-cons (- base 1) bn0))
           (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons (- base 2) (bn-cons (- base 1) (bn-cons 1 bn0))))

;; same parity (pos), no carry
(test (+ (bn-cons (- base 1) (bn-cons 0 bn0))
           (bn-cons 0 (bn-cons 0 bn0)))
      (- base 1))
      ;; (cons base-1 bn0))

;; same parity (neg), carry
(test (+ (bn-cons (- base 1) (bn-cons (- base 1) bn-1))
           (bn-cons (- base 1) (bn-cons (- base 1) bn-1)))
      -2)

;; same parity (neg), no carry
(test (+ (bn-cons (- base 1) (bn-cons 0 bn-1))
           (bn-cons (- base 1) (bn-cons 0 bn-1)))
      (bn-cons (- base 2) (bn-cons 1 (bn-cons (- base 2) bn-1))))

(test (+ bn0 bn0) 0)

(test (+ bn0 bn-1) -1) ;; bn-1)

(test (+ bn-1 bn0) -1) ;; bn-1)

(test (+ (bn-cons (- base 1) bn-1) (bn-cons (- base 1) bn-1)) -2)

(test (+ ($ 1234) ($ 56789)) ($ 58023))

(test (+ ($ -1234) ($ 56789)) ($ 55555))

(test (+ ($ 1234) ($ -56789)) ($ -55555))

(test (+ ($ -1234) ($ -56789)) ($ -58023))



;; variadic addition

(test (var-bn+ ($ 10000) ($ 20000) ($ 30000) ($ 40000) ($ 50000)) ($ 150000))

(test (var-bn+ ($ -10000) ($ -20000) ($ -30000) ($ -40000) ($ -50000))
      ($ -150000))

(test (var-bn+ ($ 1000000) 1 1) ($ 1000002))

(test (var-bn+ ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) bn0)


;; substraction

;; logic for these tests is no longer relevant, but they should still work

;; same parity (positive), no n-carry 
(test (- (bn-cons (- base 1) (bn-cons (- base 1) bn0))
           (bn-cons 0 (bn-cons (- base 1) bn0)))
      (- base 1))

;; same parity (negative), no n-carry 
(test (- (bn-cons (- base 1) (bn-cons (- base 1) bn-1))
           (bn-cons 0 (bn-cons (- base 1) bn-1)))
      (- base 1))

;; same parity (positive), n-carry 
(test (- (bn-cons (- base 1) (bn-cons 0 bn0))
           (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons 0 (bn-cons 1 bn-1)))
      
;; same parity (negative), n-carry
(test (- (bn-cons (- base 1) (bn-cons 0 bn-1))
           (bn-cons (- base 1) (bn-cons (- base 1) bn-1)))
      (bn-cons 0 (bn-cons 1 bn-1)))
      
;; different parity (pos, neg), no n-carry
(test (- (bn-cons (- base 1) (bn-cons (- base 1) bn0))
           (bn-cons (- base 1) (bn-cons (- base 1) bn-1)))
      (bn-cons 0 (bn-cons 0 (bn-cons 1 bn0))))
      
;; different parity (neg, pos), no n-carry
(test (- (bn-cons (- base 1) (bn-cons (- base 1) bn-1))
           (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons 0 (bn-cons 0 bn-1)))
      
;; different parity (pos, neg), n-carry
(test (- (bn-cons (- base 1) bn0)
         (bn-cons (- base 1) (bn-cons (- base 1) (bn-cons 0 bn-1))))
      (bn-cons 0 (bn-cons 1 (bn-cons (- base 1) bn0))))
      
(test (- bn0 bn0) 0) ;; bn0)

(test (- bn0 bn-1) 1) ;; (bn-cons 1 bn0))

(test (- bn-1 bn0) -1) ;; bn-1)

(test (- bn-1 bn-1) 0) ;; bn0)

(test (- ($ 1234) ($ 56789)) ($ -55555))

(test (- ($ -1234) ($ 56789)) ($ -58023))

(test (- ($ 1234) ($ -56789)) ($ 58023))

(test (- ($ -1234) ($ -56789)) ($ 55555))



;; unary substraction

(test (bn-u bn0) 0) ;; bn0)

(test (bn-u (bn-cons (- base 1) bn-1)) 1) ;; (cons 1 bn0))

(test (bn-u ($ 123456789)) ($ -123456789))

(test (bn-u ($ -123456789)) ($ 123456789))



;; multiplication 

;; simple test, pos * pos => pos
(test (* (bn-cons (- base 1) (bn-cons (- base 1) bn0))
           (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons 1 (bn-cons 0 (bn-cons (- base 2) (bn-cons (- base 1) bn0)))))

;; simple test, pos * neg => neg
(test (* (bn-cons (- base 1) (bn-cons (- base 1) bn0))
           (bn-cons (- base 1) (bn-cons (- base 1) bn-1)))
      (bn-cons 1 (bn-cons 0 bn-1)))

;; simple test, neg * pos => neg     
(test (* (bn-cons (- base 1) (bn-cons (- base 1) bn-1))
           (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons 1 (bn-cons 0 bn-1)))

;; simple test, neg * neg => neg
(test (* (bn-cons 1 bn-1)
           (bn-cons (- base 1) (bn-cons 0 bn-1)))
      (bn-cons (- base 1) (bn-cons 1 (bn-cons (- base 2) bn0))))

(test (* -1 -1) 1) ;; (bn-cons 1 bn0))

;; anything * 0 = 0
(test (* bn0 -1) 0) ;; bn0)
 
(test (* -1 bn0) 0) ;; bn0)

(test (* ($ 1234) ($ 56789)) ($ 70077626))

(test (* ($ -1234) ($ 56789)) ($ -70077626))

(test (* ($ 1234) ($ -56789)) ($ -70077626))

(test (* ($ -1234) ($ -56789)) ($ 70077626))



;; variadic multiplication

(test (var-bn* ($ 100) ($ 200) ($ 300) ($ 400)) ($ 2400000000))

(test (var-bn* ($ -100) ($ -200) ($ -300)) ($ -6000000))

(test (var-bn* ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) bn0)



;; quotient 

;; (quotient pos pos) => pos 
(test (quotient (bn-cons (- base 1) (bn-cons (- base 1) bn0))
                (bn-cons 0 (bn-cons (- base 1) bn0)))
      1)

;; (quotient neg pos) => neg
(test (quotient (bn-cons (- base 1) (bn-cons 0 bn-1))
                   (bn-cons 0 (bn-cons (- base 1) bn0)))
      -1)

;; (quotient pos neg) => neg
(test (quotient (bn-cons 0 (bn-cons (- base 1) bn0))
                   (bn-cons 0 (bn-cons (- base 1) bn-1)))
      (- 0 (- base 1)))

;; (quotient neg neg) => pos
(test (quotient (bn-cons (- base 1) (bn-cons 0 bn-1))
                   (bn-cons 0 bn-1))
      (- base 1))

;; a and b equal => 1
(test (quotient (bn-cons (- base 1) (bn-cons (- base 1) bn0))
                   (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      1)

(test (quotient bn0 bn-1) 0)

(test (quotient bn-1 bn-1) 1)


(test (quotient (bn-cons (- base 1) (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
                   (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons 0 (bn-cons 1 bn0)))

(test (quotient (bn-cons (- base 1) (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
                   (bn-cons (- base 1) (bn-cons 0 bn-1)))
      (bn-cons 0 (bn-cons (- base 1) bn-1)))

(test (quotient (bn-cons (- base 1) (bn-cons 0 (bn-cons 0 bn-1)))
                   (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons 0 (bn-cons (- base 1) bn-1)))

(test (quotient (bn-cons (- base 1) (bn-cons 0 (bn-cons 0 bn-1)))
                   (bn-cons (- base 1) (bn-cons 0 bn-1)))
      (bn-cons 0 (bn-cons 1 bn0)))

(test (quotient ($ 123456) ($ 789)) ($ 156))

(test (quotient ($ 123456) ($ -789)) ($ -156))

(test (quotient ($ -123456) ($ 789)) ($ -156))

(test (quotient ($ -123456) ($ -789)) ($ 156))
 

;; remainder

;; (remainder pos pos) => pos
(test (remainder (bn-cons (- base 1) (bn-cons 0 (bn-cons (- base 1) bn0)))
                    (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons (- base 2) (bn-cons 1 (bn-cons 0 bn0))))

;; (remainder pos neg) => pos
(test (remainder (bn-cons (- base 1) (bn-cons (- base 1) bn0))
                    (bn-cons 0 (bn-cons (- base 1) bn-1)))
      (- base 1))

;; (remainder neg pos) => neg
(test (remainder (bn-cons (- base 1) (bn-cons (- base 1) (bn-cons 0 bn-1)))
                    (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons 0 bn-1))

;; (remainder neg neg) => neg
(test (remainder (bn-cons (- base 1) (bn-cons (- base 1) (bn-cons 0 bn-1)))
                    (bn-cons (- base 1) (bn-cons 0 bn-1)))
      (bn-cons (- base 2) (bn-cons 1 (bn-cons (- base 1) bn-1))))

;; (remainder a b) where |a| == |b| => 0
(test (remainder (bn-cons (- base 1) bn0)
                    (bn-cons (- base 1) bn-1))
      0)

(test (remainder ($ 123456) ($ 789)) ($ 372))

(test (remainder ($ 123456) ($ -789)) ($ 372))

(test (remainder ($ -123456) ($ 789)) ($ -372))

(test (remainder ($ -123456) ($ -789)) ($ -372))



;; modulo 

;; (modulo pos pos) => pos
(test (modulo (bn-cons (- base 1) (bn-cons 0 (bn-cons (- base 1) bn0)))
                 (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
      (bn-cons (- base 2) (bn-cons 1 bn0)))

;; (modulo pos neg) => neg
(test (modulo (bn-cons 1 (bn-cons (- base 2) bn0))
                 (bn-cons 0 (bn-cons 1 bn-1)))
      (bn-cons 1 (bn-cons (- base 1) bn-1)))

;; please ignore this
(if (> base 2)
    ;; (modulo neg pos) => pos
    (test (modulo (bn-cons 0 (bn-cons 0 (bn-cons (- base 1) bn-1)))
                     (bn-cons 0 (bn-cons 1 bn0)))
          (bn-cons 0 (bn-cons 1 bn0))))

;; (modulo a b), a,b < 0, |a| > |b| => neg
(test (modulo (bn-cons (- base 1) (bn-cons (- base 1) (bn-cons 0 bn-1)))
                 (bn-cons (- base 1) (bn-cons 0 bn-1)))
      (bn-cons (- base 2) (bn-cons 1 (bn-cons (- base 1) bn-1))))

;; (modulo a b), a,b < 0, |a| = |b| => 0
(test (modulo bn-1 bn-1) 0)

;; division by 0
;; (test (modulo bn0 bn0) error)

(test (modulo ($ 123456) ($ 789)) ($ 372))

(test (modulo ($ 123456) ($ -789)) ($ -417))

(test (modulo ($ -123456) ($ 789)) ($ 417))

(test (modulo ($ -123456) ($ -789)) ($ -372))



;; equality

(test2 (= bn0 bn0) #t) 

(test2 (= (bn-cons (- base 1) bn-1) (bn-cons (- base 1) bn-1)) #t)

(test2 (= bn0 (bn-cons (- base 1) bn-1)) #f)

(test2 (= (bn-cons (- base 1) bn-1) bn0) #f)

(test2 (= (bn-cons (- base 1) bn0)
            (bn-cons (- base 1) bn0))
       #t)

(test2 (= (bn-cons (- base 1) bn0)
            (bn-cons 0 bn0))
       #f)

(test2 (= ($ 100) ($ 100)) #t)

(test2 (= ($ -100) ($ -100)) #t)

(test2 (= ($ 100) ($ -100)) #f)



;; variadic equality

(test2 (var-bn= ($ 500) ($ 500) ($ 500) ($ 300) ($ 500)) #f)

(test2 (var-bn= ($ 3000) ($ 3000) ($ 3000)) #t)

(test2 (var-bn= ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) #t)

(test2 (var-bn= -1 (bn-norm (cons (- base 1) bn-1)) (bn-neg 1)) #t)

(test2 (var-bn= ($ 500000) ($ 500000) ($ 300000)) #f)

(test2 (var-bn= ($ 3000000) ($ 3000000) ($ 3000000)) #t)




;; less than

(test2 (< bn0 bn0) #f)

(test2 (< (bn-cons (- base 1) bn-1) (bn-cons (- base 1) bn-1)) #f)

(test2 (< bn0 bn-1) #f)

(test2 (< (bn-cons (- base 1) bn-1) bn0) #t)

(test2 (< (bn-cons (- base 1) bn0) bn0) #f)

(test2 (< bn0 (bn-cons (- base 1) bn0)) #t)

(test2 (< (bn-cons 0 (bn-cons (- base 1) bn-1))
            (bn-cons 0 (bn-cons (- base 1) bn0)))
       #t)

(test2 (< (bn-cons 0 (bn-cons (- base 1) bn0))
            (bn-cons 0 (bn-cons (- base 1) bn-1)))
       #f)

(test2 (< (bn-cons 0 (bn-cons (- base 1) bn0))
            (bn-cons (- base 1) (bn-cons (- base 1) bn0)))
       #t)

(test2 (< (bn-cons 1 bn-1) (bn-cons 2 bn-1)) #t)

(test2 (< ($ 100) ($ 100)) #f)

(test2 (< ($ -100) ($ 100)) #t)

(test2 (< ($ 100) ($ -100)) #f)



;; variadic less than

(test2 (var-bn< ($ 100) ($ 200) ($ 300) ($ 400)) #t)

(test2 (var-bn< ($ -100) ($ -200) ($ -300) ($ -400)) #f)

(test2 (var-bn< ($ 100000) ($ 200000) ($ 300000) ($ 400000)) #t)

(test2 (var-bn< ($ -100000) ($ -200000) ($ -300000) ($ -400000)) #f)



;; less or equal

(test2 (<= bn0 bn0) #t)

(test2 (<= (bn-cons (- base 1) bn-1) (bn-cons (- base 1) bn-1)) #t)

(test2 (<= bn0 (bn-cons (- base 1) bn-1)) #f)

(test2 (<= (bn-cons (- base 1) bn-1) bn0) #t)

(test2 (<= ($ -100) ($ 101)) #t)

(test2 (<= ($ -100) 0) #t)

(test2 (<= ($ -100) ($ 100)) #t)

(test2 (<= ($ 100) ($ 100)) #t)

(test2 (<= ($ 100) ($ -100)) #f)



;; variadic less or equal

(test2 (var-bn<= ($ 100) ($ 200) ($ 300) ($ 400)) #t)

(test2 (var-bn<= ($ -100) ($ -200) ($ -300) ($ -400)) #f)

(test2 (var-bn<= ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) #t)

(test2 (var-bn<= ($ 100000) ($ 200000) ($ 300000) ($ 400000)) #t)

(test2 (var-bn<= ($ -100000) ($ -200000) ($ -300000) ($ -400000)) #f)



;; greater than

(test2 (> bn0 bn0) #f)

(test2 (> (bn-cons (- base 1) bn-1) (bn-cons (- base 1) bn-1)) #f)

(test2 (> bn0 (bn-cons (- base 1) bn-1)) #t)

(test2 (> (bn-cons (- base 1) bn-1) bn0) #f)

(test2 (> ($ -100) ($ 101)) #f)

(test2 (> ($ -100) 0) #f)

(test2 (> ($ -100) ($ 100)) #f)

(test2 (> ($ 100) ($ 100)) #f)

(test2 (> ($ 101) ($ -100)) #t)



;; variadic greater than

(test2 (var-bn> ($ 100) ($ 200) ($ 300) ($ 400)) #f)

(test2 (var-bn> ($ -100) ($ -200) ($ -300) ($ -400)) #t)

(test2 (var-bn> ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) #f)

(test2 (var-bn> ($ 100000) ($ 200000) ($ 300000) ($ 400000)) #f)

(test2 (var-bn> ($ -100000) ($ -200000) ($ -300000) ($ -400000)) #t)


;; greater or equal

(test2 (>= bn0 bn0) #t)

(test2 (>= (bn-cons (- base 1) bn-1) (bn-cons (- base 1) bn-1)) #t)

(test2 (>= bn0 (bn-cons (- base 1) bn-1)) #t)

(test2 (>= (bn-cons (- base 1) bn-1) bn0) #f)

(test2 (>= ($ -100) ($ 101)) #f)

(test2 (>= ($ -100) 0) #f)

(test2 (>= ($ -100) ($ 100)) #f)

(test2 (>= ($ 100) ($ 100)) #t)

(test2 (>= ($ 101) ($ -100)) #t)



;; variadic greater than

(test2 (var-bn>= ($ 100) ($ 200) ($ 300) ($ 400)) #f)

(test2 (var-bn>= ($ -100) ($ -200) ($ -300) ($ -400)) #t)

(test2 (var-bn>= ($ 0) ($ 0) ($ 0) ($ 0) ($ 0)) #t)

(test2 (var-bn>= ($ 100000) ($ 200000) ($ 300000) ($ 400000)) #f)

(test2 (var-bn>= ($ -100000) ($ -200000) ($ -300000) ($ -400000)) #t)



;; bitwise not

(test (bn~ 0) -1)

(test (bn~ ($ 10000000)) ($ -10000001))

(test (bn~ ($ -10000000)) ($ 9999999))



;; negation

(test (bn-neg bn0) 0)

(test (bn-neg 0) 0)

(test (bn-neg (bn-cons (- base 1) bn-1)) 1)

(test (bn-neg (bn-cons 1 bn0)) -1)

(test (bn-neg ($ 123456789)) ($ -123456789))

(test (bn-neg ($ -123456789)) ($ 123456789))



;; bn-abs

(test (abs bn0) 0)

(test (abs ($ 100)) ($ 100))

(test (abs ($ -100)) ($ 100))

(test (abs ($ 1000000000)) ($ 1000000000))

(test (abs ($ -1000000000)) ($ 1000000000))


;; bn-zero?

(test2 (zero? ($ 0)) #t)

(test2 (zero? (- 0 base)) #f)

(test2 (zero? (+ 0 base)) #f)



;; bn-positive?

(test2 (positive? 0) #f)

(test2 (positive? ($ -10000000000000)) #f)

(test2 (positive? ($ 10000000000000)) #t)



;; bn-negative?

(test2 (negative? 0) #f)

(test2 (negative? ($ -10000000000000)) #t)

(test2 (negative? ($ 10000000000000)) #f)



;; bn-even?

(test2 (even? ($ 100)) #t)

(test2 (even? ($ -100)) #t)

(test2 (even? ($ 101)) #f)

(test2 (even? ($ -101)) #f)

(test2 (even? ($ 1000000000)) #t)

(test2 (even? ($ -1000000000)) #t)

(test2 (even? ($ 1000000001)) #f)

(test2 (even? ($ -1000000001)) #f)



;; bn-odd?

(test2 (odd? ($ 100)) #f)

(test2 (odd? ($ -100)) #f)

(test2 (odd? ($ 101)) #t)

(test2 (odd? ($ -101)) #t)

(test2 (odd? ($ 1000000000)) #f)

(test2 (odd? ($ -1000000000)) #f)

(test2 (odd? ($ 1000000001)) #t)

(test2 (odd? ($ -1000000001)) #t)



;; bn-max

(test (max ($ 0) bn0) ($ 0))

(test (max ($ 0) ($ 100)) ($ 100))

(test (max ($ 0) ($ -100)) ($ 0))

(test (max ($ 0) ($ 10000000)) ($ 10000000))

(test (max ($ 0) ($ -10000000)) ($ 0))



;; variadic max

(test (var-bn-min ($ 0) ($ 0) ($ 0) ($ 0)) ($ 0))

(test (var-bn-max ($ 100) ($ 200) ($ 300) ($ 400) ($ 500)) ($ 500))

(test (var-bn-max ($ -100) ($ -200) ($ -300) ($ -400) ($ -500)) ($ -100))

(test (var-bn-max ($ 100000) ($ 200000) ($ 300000)) ($ 300000))

(test (var-bn-max ($ -100000) ($ -200000) ($ -300000)) ($ -100000))



;; bn-min

(test (min ($ 0) bn0) ($ 0))

(test (min ($ 0) ($ 100)) ($ 0))

(test (min ($ 0) ($ -100)) ($ -100))

(test (min ($ 0) ($ 10000000)) ($ 0))

(test (min ($ 0) ($ -10000000)) ($ -10000000))


;; variadic min

(test (var-bn-min ($ 0) ($ 0) ($ 0) ($ 0) bn0) ($ 0))

(test (var-bn-min ($ 100) ($ 200) ($ 300) ($ 400) ($ 500)) ($ 100))

(test (var-bn-min ($ -100) ($ -200) ($ -300) ($ -400) ($ -500)) ($ -500))

(test (var-bn-min ($ 100000) ($ 200000) ($ 300000)) ($ 100000))

(test (var-bn-min ($ -100000) ($ -200000) ($ -300000)) ($ -300000))


;; bn-gcd

(test (gcd 0 0) 0)

(test (gcd ($ 18) ($ 24)) ($ 6))

(test (gcd ($ -18) ($ 24)) ($ 6))

(test (gcd ($ 18) ($ -24)) ($ 6))

(test (gcd ($ -18) ($ -24)) ($ 6))

(test (gcd ($ 7) ($ 11)) ($ 1))

(test (gcd ($ 180000) ($ 240000)) ($ 60000))

(test (gcd ($ -180000) ($ 240000)) ($ 60000))

(test (gcd ($ 180000) ($ -240000)) ($ 60000))

(test (gcd ($ -180000) ($ -240000)) ($ 60000))



;; variadic bn-gcd

(test (var-bn-gcd 0 0 0 0 0 0) 0)

(test (var-bn-gcd ($ 18) ($ 24) ($ 36)) ($ 6))

(test (var-bn-gcd ($ -18) ($ 24) ($ -36)) ($ 6))

(test (var-bn-gcd ($ 18) ($ -24) ($ -36)) ($ 6))

(test (var-bn-gcd ($ -18) ($ -24) ($ -36)) ($ 6))

(test (var-bn-gcd ($ 7) ($ 11) ($ 13)) ($ 1))

(test (var-bn-gcd ($ 180000) ($ 240000) ($ 360000)) ($ 60000))

(test (var-bn-gcd ($ -180000) ($ 240000) ($ -360000)) ($ 60000))

(test (var-bn-gcd ($ 180000) ($ -240000) ($ -360000)) ($ 60000))

(test (var-bn-gcd ($ -180000) ($ -240000) ($ 360000)) ($ 60000))



;; bn-lcm

(test (lcm ($ 3) ($ 3)) ($ 3))

(test (lcm ($ 18) ($ 24)) ($ 72))

(test (lcm ($ -18) ($ 24)) ($ 72))

(test (lcm ($ 18) ($ -24)) ($ 72))

(test (lcm ($ -18) ($ -24)) ($ 72))

(test (lcm ($ 300000) ($ 300000)) ($ 300000))

(test (lcm ($ 180000) ($ 240000)) ($ 720000))

(test (lcm ($ -180000) ($ 240000)) ($ 720000))

(test (lcm ($ 180000) ($ -240000)) ($ 720000))

(test (lcm ($ -180000) ($ -240000)) ($ 720000))



;; variadic bn-lcm

(test (var-bn-lcm ($ 3) ($ 3) ($ 3) ($ 3) ($ 3)) ($ 3))

(test (var-bn-lcm ($ 18) ($ 24) ($ 6)) ($ 72))

(test (var-bn-lcm ($ -18) ($ 24) ($ 36)) ($ 72))

(test (var-bn-lcm ($ 18) ($ -24) ($ -6)) ($ 72))

(test (var-bn-lcm ($ -18) ($ -24) ($ -36)) ($ 72))

(test (var-bn-lcm ($ 18000) ($ 24000) ($ 6000)) ($ 72000))

(test (var-bn-lcm ($ -18000) ($ 24000) ($ 36000)) ($ 72000))

(test (var-bn-lcm ($ 18000) ($ -24000) ($ -6000)) ($ 72000))

(test (var-bn-lcm ($ -18000) ($ -24000) ($ -36000)) ($ 72000))