;;; Flonum library

;;==============================================================================

;; Utilities

(##include-once (ribbit "prim-fl"))

(define (flonum? n)
  (and (##rib? n) (##eqv? (##field2 n) flonum-type)))


;; simple representation

;; field 0 contains the sign bit and the exponent
;; field 1 contains the mantissa
;; field 2 contains the flonum tag

(define simple-fl-min-e -149) 
(define simple-fl-max-e 255)
(define simple-b^p-1 8388608)
(define simple-len-mantissa 23)
(define simple-exp-bias 127)
(define simple-sign-value 256)
(define simple-max-subnormal-exp -38)
(define simple-max-subnormal-val 11754942106924411)

(define (simple-fl-nan? e f) (and (##eqv? e 255) (##< 0 f)))
(define (simple-fl-inf? e f) (and (##eqv? e 255) (##eqv? 0 f)))

(define (simple-fl-extract-sign v)
  (if (##< 255 (##field0 v)) 1 0))

(define (simple-fl-extract-exponent v)
  (let ((x (##field0 v)))
    (if (##< x 256) x (##- x 256))))

(define (simple-fl-extract-mantissa v)
  (##field1 v))


;; double representation

;; field 0 contains the sign bit and the exponent
;; field 1 contains half of the mantissa (26 bits)
;; field 2 contains half of the mantissa (26 bits) and the flonum tag

(define double-fl-min-e -1074)
(define double-fl-max-e 2047)
(define double-b^p-1 4503599627370496)
(define double-len-mantissa 52)
(define double-exp-bias 1023)
(define double-sign-value 2048)
(define double-max-subnormal-exp -308)
(define double-max-subnormal-val 22250738585072012)

(define (double-fl-nan? e f) (and (##eqv? e 2047) (< 0 f)))
(define (double-fl-inf? e f) (and (##eqv? e 2047) (##eqv? f 0)))

(define (double-fl-extract-sign v)
  (if (##< 2047 (##field0 v)) 1 0))

(define (double-fl-extract-exponent v)
  (let ((x (##field0 v)))
    (if (##< x 2048) x (##- x 2048))))

(define (double-fl-extract-mantissa v) ;; FIXME should be in field1 and field2
  (##field1 v))



(define fl-min-e            simple-fl-min-e)
(define fl-max-e            simple-fl-max-e)
(define b^p-1               simple-b^p-1)
(define sign-value          simple-sign-value)
(define exp-bias            simple-exp-bias)
(define len-mantissa        simple-len-mantissa)
(define max-subnormal-exp   simple-max-subnormal-exp)
(define max-subnormal-val   simple-max-subnormal-exp)
(define fl-nan?             simple-fl-nan?)
(define fl-inf?             simple-fl-inf?)
(define fl-extract-sign     ##fl-32-sign)
(define fl-extract-exponent ##fl-32-exponent) 
(define fl-extract-mantissa ##fl-32-mantissa)

;; (define fl-min-e            double-fl-min-e)
;; (define fl-max-e            double-fl-max-e)
;; (define b^p-1               double-b^p-1)
;; (define sign-value          double-sign-value)
;; (define exp-bias            double-exp-bias)
;; (define len-mantissa        double-len-mantissa)
;; (define max-subnormal-exp   double-max-subnormal-exp)
;; (define max-subnormal-val   double-max-subnormal-exp)
;; (define fl-nan?             double-fl-nan?)
;; (define fl-inf?             double-fl-inf?)
;; (define fl-extract-sign     ##fl-sign) ;; double-fl-extract-sign)
;; (define fl-extract-exponent ##fl-exponent) ;; double-fl-extract-exponent)
;; (define fl-extract-mantissa ##fl-mantissa) ;; simple-fl-extract-mantissa) ;; FIXME


;; FIXME should have a different value for ieee32 and ieee64
(define max-normal-val 340282356779733642748000000000000000000) 
(define min-normal-val -340282356779733642748000000000000000000)

;;------------------------------------------------------------------------------

;; special symbols and values

(define (nan-str sign) "+nan.0") ;; -nan.0 is not a symbol

(define (inf-str sign) (if (##eqv? sign 1) "-inf.0" "+inf.0"))

(define (zero-str sign) (if (##eqv? sign 1) "-0.0" "+0.0"))

;; single-precision
(define _0.0 (##rib 0 0 flonum-type))
(define _0.5 (##rib 126 0 flonum-type))
(define _1.0 (##rib 127 0 flonum-type))
(define _2.0 (##rib 128 0 flonum-type))
(define _-0.0 (##rib sign-value 0 flonum-type))
(define _-1.0 (##rib (##+ sign-value 127) 0 flonum-type))


;;------------------------------------------------------------------------------

;; unsafe conversions

(define 2^23 8388608)

(define (##exact-integer->inexact-integer n)

  ;; fails as soon as two consecutive numbers have the same representation
  ;; e.g. 16777217 and 16777218

  ;; (define (extract-exponent n e)
  ;;   (if (< n 2)
  ;;    e
  ;;    (extract-exponent (quotient n 2) (+ e 1))))

  ;; (let* ((s (if (< n 0) 1 0))
  ;;     (e (extract-exponent (abs n) 127))
  ;;     (2^e-bias (expt 2 (- e 127)))
  ;;     (f (bn-norm (* (- n 2^e-bias) (quotient 2^23 2^e-bias)))))
  ;;   (scm2flonum-32 s e f))) ;; host-specific encoding (primitive)

  ;; FIXME
  (cond ((< n min-normal-val) ;; -inf.0?
         (##rib (##+ sign-value fl-max-e) 0 flonum-type))
        ((< max-normal-val n) ;; +inf.0?
         (##rib fl-max-e 0 flonum-type))
        (else
         (let* ((str (number->string n))
                (flostr (##field0 str))
                (flolen (##field1 str)))
           (string->flonum
            (##rib 
             (##string-append flostr (##rib 46 (##rib 48 '() pair-type) pair-type))
             (##+ flolen 2)
             string-type))))))
  

(define (##inexact-integer->exact-integer n)
  ;; Unsafe conversion of an integer represented as a flonum to an integer
  ;; represented as a fixnum/bignum. The conversion is assumed to be possible,
  ;; meaning that n is normalized and so we always use the normal equation to
  ;; find the value represented by the flonum. The equation is adapted so that
  ;; we get an exact integer directly from the operations without having to do
  ;; a conversion.
  
  (let* ((s (fl-extract-sign n))
         (e (fl-extract-exponent n))
         (f (fl-extract-mantissa n)))

    (if (and (eqv? e 0) (eqv? f 0)) ;; FIXME (-)0.0 => (-)1.7014119e+38
        0
        (let ((res (quotient (* (+ 2^23 f) (expt 2 (- e 127))) 2^23)))
              ;; (res (* (quotient (+ 2^23 f) 2^23) (expt 2 (- e (- 127 23))))))
          (bn-norm (if (##eqv? 1 s) (- 0 res) res))))))


;;------------------------------------------------------------------------------

;; can't assume that r4rs will be imported

;; helper functions

(define (##string-length a)
  
  (define (##string-length-aux a counter)
    (if (##eqv? '() a) 
        counter
        (##string-length-aux (##field1 a) (##+ counter 1))))
  
  (if (and (##rib? a) (##eqv? (##field2 a) pair-type))
      (##string-length-aux a 0)
      #f))

(define (##string-append str1 str2)

  ;; Appends a string at the end of a string contain in the string-type
  ;; rib, not the string-type rib itself.
  
  (if (##eqv? (##field1 str1) '())
      (##rib (##field0 str1) str2 pair-type)
      (##rib (##field0 str1) (##string-append (##field1 str1) str2) pair-type)))


;;==============================================================================

;; Numerical operations

;;------------------------------------------------------------------------------

;; Numerical types predicates

;; number? => nums.scm
;; complex? => nums.scm
;; real? => nums.scm
;; rational? => nums.scm
;; integer? => nums.scm


;; Exactness predicates

;; exact? => nums.scm
;; inexact? => nums.scm
;; exact-integer? => nums.scm

(define (inexact-integer? a)
  (if (flonum? a)
      (if (and (not (nan? a)) (not (infinite? a)))
          (##fl= a (##fl-truncate a))
          #f)
      #f))


;; Flonum predicates

(define (finite? a) (and (not (infinite? a)) (not (nan? a))))

(define (infinite? a)
  (and (##eqv? (fl-extract-exponent a) fl-max-e)
       (##eqv? (fl-extract-mantissa a) 0)))

(define (nan? a)
  (and (##eqv? (fl-extract-exponent a) fl-max-e)
       (not (##eqv? (fl-extract-mantissa a) 0))))


;; ;; Comparison predicates

;; ;; ##fl= => prim-fl
;; ;; ##fl< => prim-fl

;; (define (##fl> a b) (##fl< b a))
(define (##fl<= a b) (or (##fl= a b) (##fl< a b)))
;; (define (##fl>= a b) (or (##fl= a b) (##fl< b a)))


;; ;; Numerical properties predicates

;; (define (##fl-zero? a) (##fl= a _0.0))
;; (define (##fl-positive? a) (##fl< _0.0 a))
;; (define (##fl-negative? a) (##fl< a _0.0))
;; (define (##fl-odd? a) (##fl= _1.0 (##fl-modulo a _2.0)))
;; (define (##fl-even? a) (##fl= _0.0 (##fl-modulo a _2.0)))


;; ;; Max and min

;; (define (##fl-max a b) (if (##fl< a b) b a))
;; (define (##fl-min a b) (if (##fl< a b) a b))


;; ;; Arithmetic operations: +, *, -, /

;; ;; ##fl+ => prim-fl
;; ;; ##fl* => prim-fl
;; ;; ##fl- => prim-fl
;; ;; ##fl/ => prim-fl


;; ;; Absolute value

;; ;; (define (##fl-abs a) (if (##fl< a _0.0) (##fl- _0.0 a) a))


;; ;; Quotient, remainder and modulo

;; (define (##fl-quotient a b) (##fl/ a b))

;; (define (##fl-remainder a b) (##fl- a (##fl* b (##fl/ a b))))

;; (define (##fl-modulo a b)
;;   (let ((q (##fl/ a b)))
;;     (let ((r (##fl- a (##fl* b q))))
;;       (if (##fl= r _0.0)
;;           _0.0
;;           (if (##fl= (##fl< a _0.0) (##fl< b _0.0))
;;               r
;;               (##fl+ r b))))))


;; ;; Gcd and lcm

;; (define (##fl-gcd a b)

;;   (define (##fl-gcd-aux a b)
;;     (if (##fl=? a _0.0)
;;         b
;;         (##fl-gcd-aux (##fl-remainder b a) a)))
  
;;   (let ((_a (##fl-abs a))
;;         (_b (##fl-abs b)))
;;     (if (##fl< _a _b) (##fl-gcd-aux _a _b) (##fl-gcd-aux _b _a))))


;; (define (##fl-lcm a b)
;;   (if (##fl= b _0.0)
;;       _0.0
;;       (let ((_a (##fl-abs a))
;;             (_b (##fl-abs b)))
;;         (##fl* (##fl/ _a (##fl-gcd _b)) _b))))


;; Floor, ceiling, truncate, and round

(define (##fl-floor a)
  (let ((_a (##fl-truncate a)))
    (if (or (##fl<= _0.0 a) (##fl= a _a))
        _a
        (##fl- _a _1.0))))
      
(define (##fl-ceiling a)
  (let ((_a (##fl-truncate a)))
    (if (or (##fl< a _0.0) (##fl= a _a))
        _a
        (##fl+ _a _1.0))))

(define (##fl-truncate a)
  ;; Can't use the host to truncate by doing a type cast. Could result in an
  ;; overflow: e.g. in C (float) (int) 1e+10 => 2147483700.0

  ;; FIXME could be much more efficient
  (cond ((or (infinite? a) (nan? a)) ;; FIXME
         a)
        ((or (##fl= _0.0 a) (##fl= _-0.0 a))
         a)
        ((and (##fl< a _1.0) (##fl< _0.0 a))
         (##rib 0 0 flonum-type))
        ((and (##fl< a _0.0) (##fl< _-1.0 a))
         (##rib sign-value 0 flonum-type))
        (else
         (##exact-integer->inexact-integer
          (##inexact-integer->exact-integer a)))))

(define (##fl-round a)
  (if (##fl< a _0.0)
      (let* ((_a (##fl-ceiling a))
             (diff (##fl- _a a)))
        (if (##fl< diff _0.5)
            _a
            (##fl- _a _1.0)))
      (let* ((_a (##fl-floor a))
             (diff (##fl- a _a)))
        (if (##fl< diff _0.5)
            _a
            (##fl+ _a _1.0)))))


;;==============================================================================

;; Numerical input and output



;;------------------------------------------------------------------------------

;; Flonum output


(define (##flonum->string v negative?)

  ;; This algorithm is derived from the paper "Printing Floating-Point
  ;; Numbers Quickly and Accurately" by Robert G. Burger and R. Kent Dybvig,
  ;; SIGPLAN'96 Conference on Programming Language Design an Implementation.


  (define (scale r s m- m+ k round?)

    ;; Finds the smallest integer k such that (r + m+)/2 <= B^k where
    ;; B is the output base (i.e. 10). In other words, finds the position
    ;; of the decimal point.

    ;; r, s, m-, and m+ could all be either fixnums or bignums

    (cond ((or (and round? ;; k is too low
                    (<= s (+ r m+))) 
               (and (not round?)
                    (< s (+ r m+))))
           (scale r (* s 10) m- m+ (+ k 1) round?))
          
          ((or (and round? ;; k is too high
                    (< (* (+ r m+) 10) s)) 
               (and (not round?)
                    (<= (* (+ r m+) 10) s)))
           (scale (* r 10) s (* m- 10) (* m+ 10) (- k 1) round?))
          
          (else  ;; k we good
           (##rib k (generate r s m- m+ round?) pair-type))))
           
    
  (define (generate r s m- m+ round?)

    ;; Generates the digits and returns a chain of rib containing those
    ;; digits without any decimal point.

    ;; r, s, m-, and m+ could all be either fixnums or bignums
    
    (let* ((d (##+ 48 (quotient (* r 10) s))) ;; digit representation in ASCII
           (rem (remainder (* r 10) s))
           (_m- (* m- 10))
           (_m+ (* m+ 10))
           (tc1 ((if round? <= <) rem _m-))
           (tc2 ((if round? >= >) (+ rem _m+) s)))
      (if (not tc1)
          (if (not tc2)
              (##rib d (generate rem s _m- _m+ round?) pair-type)
              (##rib (+ d 1) '() pair-type))
          (if (not tc2)
              (##rib d '() pair-type)
              (if (< (* r 2) s)
                  (##rib d '() pair-type)
                  (##rib (+ d 1) '() pair-type))))))
  

  (define (flonum->digits v)

    ;; Initializes the values of r, s, m+, and m- (see table 1 in the paper)

    (let* (;; (e (fl-extract-exponent v))
           ;; (f (fl-extract-mantissa v))
           (e (##field0 v)) ;; exponent, fixnum
           (f (##field1 v)) ;; mantissa, bignum or fixnum
           (round? (even? f)))
      (if (< e 0)
          (if (and (##< fl-min-e e)
                   (eqv? f b^p-1))
              (scale (* f 4) 
                     (* (expt 2 (##+ (##- 0 e) 1)) 2) 
                     1
                     2               
                     0
                     round?)                 
              (scale (* f 2)
                     (* (expt 2 (##- 0 e)) 2)
                     1
                     1
                     0
                     round?))
          (let ((base^e (expt 2 e)))
            (if (##eqv? f b^p-1)
                (let ((base^e+1 (* base^e 2)))
                  (scale (* f (* base^e+1 2))
                         4
                         base^e
                         base^e+1
                         0
                         round?))
                (scale (* f (* base^e 2))
                       2
                       base^e
                       base^e
                       0
                       round?))))))

  (define (##exp->string a)

    (define (##number->string-aux _a tail)
      (let* ((quo (quotient _a 10))              
             (rem (- _a (* 10 quo)))
             (chars (##rib (##+ 48 rem) tail pair-type)))
        (if (eqv? 0 quo) ;; could be a bignum
            chars
            (##number->string-aux quo chars))))

    (let ((chars (if (< a 0)
                     (##rib 45 (##number->string-aux (abs a) '()) pair-type)
                     (##rib 43 (##number->string-aux a '()) pair-type))))
      (##rib chars (##string-length chars) string-type)))
  

  (define (make-fl-rib str negative? length)
    (if negative?
        (##rib (##rib 45 str pair-type) (##+ length 1) string-type)
        (##rib str length string-type)))

  (define (insert-dot str pos)
    (if (##eqv? pos 0)
        (##rib 46 str pair-type)
        (##rib (##field0 str) (insert-dot (##field1 str) (##- pos 1)) pair-type)))

  (define (pad0 n)
    (if (##eqv? 0 n)
        (##rib 46 (##rib 48 '() pair-type) pair-type) ;; .0
        (##rib 48 (pad0 (##- n 1)) pair-type)))

  (define (pre-pad0 n str)
    (if (##eqv? n 0)
        str
        (##rib 48 (pre-pad0 (##+ n 1) str) pair-type))) ;; (+ n 1) because n will be negative
    
  
  (let* ((x (flonum->digits v))
         (k (##field0 x)) ;; position of the digit, exponent
         (d (##field1 x)) ;; digits
         (n (##string-length d))) ;; number of digits -- FIXME integrate this with algorithm

    ;; Following the same conventions as Gambit for formatting

    (cond ((and (##< -1 k) (##< k 11)) ;; 0 <= k <= 10
           
           (cond ((##eqv? k 0) ;; Format 1: k = 0 => 0.DDD
                  (make-fl-rib
                   (##rib 48 (##rib 46 d pair-type) pair-type)
                   negative?
                   (##+ n 2)))

                 ((##< k n) ;; Format 2: k > 0 and k < n => D.DDD up to DDD.D
                  (make-fl-rib
                   (insert-dot d k)
                   negative?
                   (##+ n 1)))

                 ((##eqv? k n) ;; Format 3: k > 0 and k = n => DDD.0
                  (make-fl-rib
                   (##string-append d (##rib 46 (##rib 48 '() pair-type) pair-type))
                   negative?
                   (##+ n 2)))

                 (else ;; Format 4: k > 0 and k > n => DDD0...0.0
                  (make-fl-rib
                   (##string-append d (pad0 (- k n)))
                   negative?
                   (##+ n (##+ k 1))))))

          ((or (##eqv? k -1) (##eqv? k -2)) ;; Format 5: k = -1 or k = -2  => 0.0DDD or 0.00DDD
            (make-fl-rib
             (##rib 48 (##rib 46 (pre-pad0 k d) pair-type) pair-type)
             negative?
             (##+ n (##+ (abs k) 2))))

          (else ;; Format 6: D.DDD e (sign) (k-1)
           (let ((num (insert-dot d 1))              ;; FIXME cleanup this mess
                 (exp (##exp->string (- k 1)))) 
             (make-fl-rib
              (##string-append
               num
               (##rib 101 (##field0 exp) pair-type))
              negative?
              (+ n (+ 2 (##field1 exp)))))))))


(define (flonum->string v)

  (if (flonum? v)
      (let* ((sign (fl-extract-sign v))
             (unbiased-e (fl-extract-exponent v))
             (unnormalized-f (fl-extract-mantissa v)))
        
        (cond ((and (##eqv? 0 unbiased-e) (##eqv? 0 unnormalized-f)) ;; 0.0?
               (let ((zero-str (##rib 48 ;; 0
                                      (##rib 46 ;; .
                                             (##rib 48 '() pair-type) ;; 0
                                             pair-type)
                                      pair-type)
                               pair-type))
                 (if (##eqv? sign 0)
                     (##rib zero-str 3 string-type)
                     (##rib (##rib 45 zero-str pair-type) 4 string-type))))
              
              ((fl-nan? unbiased-e unnormalized-f) ;; NaN?
               (nan-str sign))
              
              ((fl-inf? unbiased-e unnormalized-f) ;; inf?
               (inf-str sign))
              
              (else ;; floating-point number
               (let ((e (if (##eqv? 0 unbiased-e) ;; subnormal number?
                             fl-min-e
                             (##+ unbiased-e (##- fl-min-e 1)))) ;; FIXME
                     (f (if (##eqv? 0 unbiased-e) ;; subnormal number?
                            unnormalized-f ;; equivalent to 0.fraction
                            (+ unnormalized-f b^p-1)))) ;; 53 bits (1.fraction)
                 (##flonum->string (##rib e f flonum-type) (##eqv? 1 sign))))))
      #f))


(define ##fl-number->string flonum->string) ;; FIXME


;;------------------------------------------------------------------------------

;; Flonum input


(define (##string->flonum str)

  (define (convert char)
    (if (and (##< 47 char) (##< char 58)) ;; 0-9
        (##- char 48)
        #f))

  (define (##string->exp-aux lst num)
    (if (null? lst) ;; FIXME define null? ??
        num
        (let* ((char (##field0 lst))
               (digit (convert char)))
          (if digit
              (##string->exp-aux (##field1 lst)
                                 (+ (* 10 num) digit))
              #f))))

  (define (##string->exp lst)
    (if (null? lst)
        #f
        (cond ((##eqv? (##field0 lst) 45) ;; e-
               (let ((n (##string->exp-aux (##field1 lst) 0)))
                 (and n (- 0 n))))
              ((##eqv? (##field0 lst) 43) ;; e+
               (##string->exp-aux (##field1 lst) 0))
              (else ;; e => eqv to e+
               (##string->exp-aux lst 0)))))
  
  (define (fl-round num dec)

    ;; round to nearest
    
    (let* ((div (expt 10 dec))
           (quo (quotient num div))
           (rem (remainder num div)))
      (if (<= rem (quotient div 2)) ;; can't use primitive, could have an overflow
          quo
          (+ quo 1))))
  
  (define (calculate-exponent num exp lo hi)
    (if (and (<= lo num) (< num hi)) 
        exp
        (calculate-exponent num (+ exp 1) (* lo 2) (* hi 2))))

  (define (calculate-negative-exponent num exp hi)
    (let ((lo (quotient hi 2)))      
      (if (and (< lo num) (<= num hi)) 
          (if (##eqv? (##- 1 exp-bias) exp) exp (- exp 1)) ;; exception
          (calculate-negative-exponent num (- exp 1) lo))))

  
  (define (calculate-mantissa num dec e)

    ;; mantissa = binary (num * b^{m-e-1}) where
    ;;   m = 53 if double else m = 24
    ;;   b is our base, i.e. 2
    ;;   e is the exponent that was returned by calculate-exponent

    ;; Note that in the actual algorithm, num is a floating-point-number and
    ;; so will be the value of num * b^{m-e-1}. We simulate floating-point
    ;; operations with integers and strip off the unnecessary digits (dec).

    (let* ((len-m (##+ 1 len-mantissa))
           (exact-x (if (##< e len-m)
                        (* num (expt 2 (- len-m (+ e 1))))
                        (quotient num (expt 2 (- (+ e 1) len-m))))) ;; negative exponent
           (x (fl-round exact-x dec)))
      (if (< x b^p-1) ;; remove msb if x is not subnormal FIXME
          x
          (- x b^p-1))))
  
  (define (##string->flonum-aux-2 lst num dec intnum sign) ;; fractional part

    ;; num is the digits without the decimal point of the entire floating-point num
    ;; dec is the number of digits after the decimal point 
    ;; intnum is the integer part of the number
    ;; sign is surprisingly enough the sign of the number
    
    (cond ((null? lst)
           (if (eqv? num 0)
               (##rib
                (+ (* sign sign-value) 0)
                0
                flonum-type)
               
               (let* ((exp (if (eqv? intnum 0)                         
                               (calculate-negative-exponent num 0 (expt 10 dec))
                               (calculate-exponent intnum 0 1 2))))

                 ;; FIXME the way the flonum is stored depends on the host
                 (##rib
                  (+ (* sign sign-value) (+ exp exp-bias))
                  (calculate-mantissa num dec exp)
                  flonum-type))))

          ((##eqv? 101 (##field0 lst)) ;; e suffix?

           (let* ((_exp (##string->exp (##field1 lst)))
                  (_num (if (##< _exp 0)
                            num
                            (* num (expt 10 (- _exp dec))))) 
                  (_dec (if (##< dec _exp)
                            0
                            (- dec _exp)))
                  (exp (if (##< _exp 0)
                           (if (or (##< _exp max-subnormal-exp) ;; subnormal number? FIXME 
                                   (and (##eqv? _exp max-subnormal-exp)
                                        (##< num max-surbnormal-val)))
                               (##- 0 exp-bias)
                               (calculate-negative-exponent
                                num
                                0
                                (expt 10 (+ dec (abs _exp)))))
                           (calculate-exponent
                            (* intnum (expt 10 _exp))
                            0
                            1
                            2))))

             (if (##< 254 (+ exp exp-bias)) ;; FIXME inf?
                 (##rib (+ 255 (* sign sign-value)) 0 flonum-type)
                 (##rib
                  (+ (* sign sign-value) (+ exp exp-bias))
                  (calculate-mantissa _num _dec exp)
                  flonum-type))))
          
          (else
           (let ((digit (convert (##field0 lst))))
             (if digit
                 (##string->flonum-aux-2 (##field1 lst)
                                         (+ (* 10 num) digit)
                                         (+ dec 1)
                                         intnum
                                         sign)
                 #f)))))
               
  (define (##string->flonum-aux-1 lst num sign) ;; integer part
    (if (null? lst) ;; number was an integer FIXME need to deal with bignums as well
        num
        (let ((char (##field0 lst)))
          (cond ((##eqv? char 46) ;; dot?
                 (##string->flonum-aux-2 (##field1 lst)
                                      num
                                      0
                                      num ;; calculate exponent in aux-2 instead
                                      sign))
                ((##eqv? char 101) ;; e? 
                 (##string->flonum-aux-2 lst ;; e.g. to support 1e+10 instead of 1.0e+10
                                         num
                                         0
                                         num ;; calculate exponent in aux-2 instead
                                         sign))
                (else
                 (let ((digit (convert char)))
                   (if digit
                       (##string->flonum-aux-1 (##field1 lst) (+ (* 10 num) digit) sign)
                       #f)))))))

  
  (let* ((lst (##field0 str))
         (sign (if (##eqv? (##field0 lst) 45) 1 0)))
    
    (cond ((string=? str (nan-str sign)) ;; NaN?
           (##rib fl-max-e (- b^p-1 1) flonum-type))
          ((string=? str (inf-str sign)) ;; Inf?
           (##rib (+ (* sign sign-value) fl-max-e) 0 flonum-type))
          ((string=? str (zero-str sign)) ;; zero?
           (##rib (* sign sign-value) 0 flonum-type))
          (else
           (##string->flonum-aux-1 (if (##eqv? sign 0) lst (##field1 lst))
                                   0
                                   sign)))))

;; FIXME temporary solution for the type cast problem
(define (string->flonum str)
  (let ((florib (##string->flonum str)))
    (if (or (fixnum? florib) (bignum? florib)) ;; FIXME
        florib
        (scm2flonum-32 (simple-fl-extract-sign florib)
                       (simple-fl-extract-exponent florib)
                       (##field1 florib)))))

(define ##fl-string->number string->flonum) ;; FIXME
