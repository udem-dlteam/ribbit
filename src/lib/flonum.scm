;;; File: flonum.scm

;;------------------------------------------------------------------------------

;; TODO

;; - minor issues with subnormal numbers
;; - optimize
;; - adapt implementation to simple and double representation
;; - more tests, especially for simple representation
;; - cleanup

;; Note that some tests will fail for the moment because of the issues with
;; subnormal numbers. Some tests will also be slow because of the way the
;; exponent is calculated, will need to fix that.


;;------------------------------------------------------------------------------

(define pair-type      0)
;; (define procedure-type 1)
;; (define symbol-type    2)
(define string-type    3)
;; (define vector-type    4)
;; (define singleton-type 5)
;; (define char-type      6)
(define bignum-type    7)
(define flonum-type    8)


(define (flonum? n)
  (and (##rib? n) (##eqv? (##field2 n) flonum-type)))


;; simple representation

;; field 0 contains the sign bit and the exponent
;; field 1 contains the mantissa
;; field 2 contains the flonum tag

(define simple-fl-min-e -127) ;; FIXME

(define (simple-fl-nan? e f)
  (and (##eqv? e 255) (##< 0 f)))

(define (simple-fl-inf? e f)
  (and (##eqv? e 255) (##eqv? 0 f)))

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

(define double-b^p-1 4503599627370496)

(define (double-fl-nan? e f)
  (and (##eqv? e 2047) (< 0 f)))

(define (double-fl-inf? e f)
  (and (##eqv? e 2047) (##eqv? f 0)))

(define (double-fl-extract-sign v)
  (if (##< 2047 (##field0 v)) 1 0))

(define (double-fl-extract-exponent v)
  (let ((x (##field0 v)))
    (if (##< x 2048) x (##- x 2048))))

(define (double-fl-extract-mantissa v) ;; FIXME
  (##field1 v))


;; representation

;; (if simple 
;;    (begin
;;      (define fl-min-e            simple-fl-min-e)
;;      (define b^p-1               simple-b^p-1)
;;      (define fl-nan?             simple-fl-nan?)
;;      (define fl-inf?             simple-fl-inf?)
;;      (define fl-extract-sign     simple-fl-extract-sign)
;;      (define fl-extract-exponent simple-fl-extract-exponent)
;;      (define fl-extract-mantissa simple-fl-extract-mantissa))
;;    (begin
;;      (define fl-min-e            double-fl-min-e)
;;      (define b^p-1               double-b^p-1)
;;      (define fl-nan?             double-fl-nan?)
;;      (define fl-inf?             double-fl-inf?)
;;      (define fl-extract-sign     double-fl-extract-sign)
;;      (define fl-extract-exponent double-fl-extract-exponent)
;;      (define fl-extract-mantissa simple-fl-extract-mantissa))

(define fl-min-e            double-fl-min-e)
(define b^p-1               double-b^p-1)
(define fl-nan?             double-fl-nan?)
(define fl-inf?             double-fl-inf?)
(define fl-extract-sign     double-fl-extract-sign)
(define fl-extract-exponent double-fl-extract-exponent)
(define fl-extract-mantissa simple-fl-extract-mantissa)


;; special symbols

(define (nan-str sign) "+nan.0") ;; -nan.0 is not a symbol

(define (inf-str sign) (if (##eqv? sign 1) "-inf.0" "+inf.0"))


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


(define (expt x y)
  (if (eqv? y 0)
      1
      (let ((t (expt (* x x) (quotient y 2))))
        (if (odd? y)
            (* x t)
            t))))


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

    (let* ((e (fl-extract-exponent v)) ;; exponent, fixnum
	   (f (fl-extract-mantissa v)) ;; mantissa, bignum or fixnum
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

  ;; for now v is a rib with a flonum tag structured 
  ;;  - field0 contains the value (base 2) of the sign and the (unbiased) exponent
  ;;  - field1 contains the mantissa (in base 2), i.e. the fractional part

  (if (flonum? v)
      (let* ((sign (fl-extract-sign v))
	     (unbiased-e (fl-extract-exponent v))
	     (unnormalized-f (##field1 v)))
	
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
			    (+ unnormalized-f 4503599627370496)))) ;; 53 bits (1.fraction)
		 (##flonum->string (##rib e f flonum-type) (##eqv? 1 sign))))))
      #f))


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
	(if (##eqv? (##field0 lst) 45) ;; negative? 
            (let ((n (##string->exp-aux (##field1 lst) 0)))
	      (and n (- 0 n))) 
            (##string->exp-aux (##field1 lst) 0)))) ;; note that car will be `+`
  
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
	  (if (##eqv? -1022 exp) exp (- exp 1)) ;; exception
	  (calculate-negative-exponent num (- exp 1) lo))))

  
  (define (calculate-mantissa num dec e)

    ;; mantissa = binary (num * b^{m-e-1}) where
    ;;   m = 53 if double else m = 24
    ;;   b is our base, i.e. 2
    ;;   e is the exponent that was returned by calculate-exponent

    ;; Note that in the actual algorithm, num is a floating-point-number and
    ;; so will be the value of num * b^{m-e-1}. We simulate floating-point
    ;; operations with integers and strip off the unnecessary digits (dec).

    (let* ((exact-x (if (##< e 53)
			(* num (expt 2 (- 53 (+ e 1))))
			(quotient num (expt 2 (- (+ e 1) 53))))) ;; negative exponent
	   (x (fl-round exact-x dec)))       
      (if (< x 4503599627370496) ;; remove msb if x is not subnormal FIXME
	  x
	  (- x 4503599627370496))))

  
  (define (##string->flonum-aux-2 lst num dec intnum sign) ;; fractional part

    ;; num is the digits without the decimal point of the entire floating-point num
    ;; dec is the number of digits after the decimal point 
    ;; intnum is the integer part of the number
    ;; sign is surprisingly enough the sign of the number
    
    (cond ((null? lst)
	   (if (eqv? num 0)
	       (##rib
		(+ (* sign 2048) 0)
		0
		flonum-type)
	       
	       (let* ((exp (if (eqv? intnum 0)			       
			       (calculate-negative-exponent num 0 (expt 10 dec))
			       (calculate-exponent intnum 0 1 2))))
		 (##rib
		  (+ (* sign 2048) (+ exp 1023))
		  (calculate-mantissa num dec exp)
		  flonum-type))))

	  ((##eqv? 101 (##field0 lst)) ;; e+ or e- suffix?

	   (let* ((_exp (##string->exp (##field1 lst)))		      
		  (_num (if (##< _exp 0)
			    num
			    (* num (expt 10 (- _exp dec))))) 
		  (_dec (if (##< dec _exp)
			    0
			    (- dec _exp)))
		  (exp (if (##< _exp 0)
			   (if (or (##< _exp -308) ;; subnormal number? FIXME 
				   (and (##eqv? _exp -308)
					(##< num 22250738585072012)))
			       -1023
			       (calculate-negative-exponent
				num
				0
				(expt 10 (+ dec (abs _exp)))))
			   (calculate-exponent
			    (* intnum (expt 10 _exp))
			    0
			    1
			    2))))

	     (##rib
	      (+ (* sign 2048) (+ exp 1023))
	      (calculate-mantissa _num _dec exp)
	      flonum-type)))
	  
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
    (let ((char (##field0 lst)))
      (if (##eqv? char 46) ;; dot?
	  (##string->flonum-aux-2 (##field1 lst)
				  num
				  0
				  num ;; calculate exponent in aux-2 instead
				  sign)
	  (let ((digit (convert char)))
	    (if digit
		(##string->flonum-aux-1 (##field1 lst) (+ (* 10 num) digit) sign)
		#f)))))

  
  (let* ((lst (##field0 str))
	 (sign (if (##eqv? (##field0 lst) 45) 1 0)))
    
    (cond ((string=? str (nan-str sign)) ;; NaN?
	   (##rib 2047 4503599627370495 flonum-type))
	  ((string=? str (inf-str sign)) ;; Inf?
	   (##rib (+ (* sign 2048) 2047) 0 flonum-type))
	  (else
	   (##string->flonum-aux-1 (if (##eqv? sign 0) lst (##field1 lst))
				   0
				   sign)))))



;;------------------------------------------------------------------------------

;; unit tests: flonum->string

(define (test-fl->str a b)
  (if (not (equal? a b))
      (begin
	(display "test failed...\n")
	(display "expected: ") (display b) (newline)
	(display "result: ") (display a) (newline))
      (display "test passed...\n")))

;; initial values of r, s, m+, and m- in burger's algorithm

;; TODO


;; special values 

;; +0.0 => 0 00000000000 0000000000000000000000000000000000000000000000000000

(test-fl->str (flonum->string (##rib 0 0 flonum-type))
	      "0.0")

;; -0.0 => 1 00000000000 0000000000000000000000000000000000000000000000000000

(test-fl->str (flonum->string (##rib 2048 0 flonum-type))
	      "-0.0")

;; +inf.0 => 0 11111111111 0000000000000000000000000000000000000000000000000000

(test-fl->str (flonum->string (##rib 2047 0 flonum-type))
	      "+inf.0")

;; -inf.0 => 1 11111111111 0000000000000000000000000000000000000000000000000000 

(test-fl->str (flonum->string (##rib 4095 0 flonum-type))
	      "-inf.0")

;; nan.0 0 11111111111 [anything that's not 0]

(test-fl->str (flonum->string (##rib 2047 1 flonum-type))
	      "+nan.0")

(test-fl->str (flonum->string (##rib 2047 2251799813685249 flonum-type))
	      "+nan.0")

(test-fl->str (flonum->string (##rib 2047 4503599627370495 flonum-type))
	      "+nan.0")


;; Format 1: k = 0 => 0.DDD

;; 1/3 => 0 01111111101 0101010101010101010101010101010101010101010101010101

(test-fl->str (flonum->string (##rib 1021 1501199875790165 flonum-type))
	      "0.3333333333333333")


;; Format 2: k > 0 and k < n => D.DDD up to DDD.D
;; pi => 0 100000000000 1001001000011111101101010100010001000010110100011000

(test-fl->str (flonum->string (##rib 1024 2570638124657944 flonum-type))
	      "3.141592653589793")


;; Format 3: k > 0 and k = n => DDD.0

;; 2.0 => 0 10000000000 0000000000000000000000000000000000000000000000000000

(test-fl->str (flonum->string (##rib 1024 0 flonum-type))
	      "2.0")

;; -2.0 => 1 10000000000 0000000000000000000000000000000000000000000000000000

(test-fl->str (flonum->string (##rib 3072 0 flonum-type))
	      "-2.0")


;; Format 4: k > 0 and k > n => DDD0...0.0

;; 1000000 => 0 10000010010 1110100001001000000000000000000000000000000000000000

(test-fl->str (flonum->string (##rib 1042 4086334964629504 flonum-type))
	      "1000000.0")


;; Format 5: k = -1 or k = -2  => 0.0DDD or 0.00DDD

;; 0.01 => 0 01111111000 0100011110101110000101000111101011100001010001111011

(test-fl->str (flonum->string (##rib 1016 1261007895663739 flonum-type))
	      "0.01")

;; -0.001 => 1 01111110101 0000011000100100110111010010111100011010100111111100

(test-fl->str (flonum->string (##rib 3061 108086391056892 flonum-type))
	      "-0.001")


;; Format 6: D.DDD e (sign) (k-1)

;; min subnormal positive double: 2^−1074 ≈ 4.9406564584124654 × 10^−324
;; => 0 00000000000 0000000000000000000000000000000000000000000000000001

(test-fl->str (flonum->string (##rib 0 1 flonum-type))
	      "5.e-324")

;; max subnormal positive double: ≈ 2.2250738585072009 × 10^−308
;; => 0 00000000000 1111111111111111111111111111111111111111111111111111

(test-fl->str (flonum->string (##rib 0 4503599627370495 flonum-type))
	      "2.225073858507201e-308")

;; min normal positive double: ≈ 2.2250738585072014 × 10^−308
;; => 0 00000000001 0000000000000000000000000000000000000000000000000000

(test-fl->str (flonum->string (##rib 1 0 flonum-type))
	      "2.2250738585072014e-308")

;; max positive normal double : ≈ 1.7976931348623157 × 10^308
;; => 0 11111111110 1111111111111111111111111111111111111111111111111111

(test-fl->str (flonum->string (##rib 2046 4503599627370495 flonum-type))
	      "1.7976931348623157e+308")


;; misc

;; 1.0 => 0 01111111111 0000000000000000000000000000000000000000000000000000

(test-fl->str (flonum->string (##rib 1023 0 flonum-type))
	      "1.0")

;; smallest number > 1 ≈ 1.0000000000000002
;; => 0 01111111111 0000000000000000000000000000000000000000000000000001

(test-fl->str (flonum->string (##rib 1023 1 flonum-type))
	      "1.0000000000000002")

;; second smallest number > 1 ≈ 1.0000000000000004
;; => 0 01111111111 0000000000000000000000000000000000000000000000000010

(test-fl->str (flonum->string (##rib 1023 2 flonum-type))
	      "1.0000000000000004")


;; 1.448997445238699 = 6525704354437805 x 2^-52
;; 0 01111111111 0111001011110001011111110001111101001001101010101101
;; mantissa (53 digits): 10111001011110001011111110001111101001001101010101101

(test-fl->str (flonum->string (##rib 1023 2022104727067309 flonum-type))
	      "1.448997445238699")

;; 1.4489974452386991 = 6525704354437806 x 2^-52
;; mantissa (53 digits): 10111001011110001011111110001111101001001101010101110

(test-fl->str (flonum->string (##rib 1023 2022104727067310 flonum-type))
	      "1.4489974452386991")

;; 123456789.987654321 = 123456789.98765432834625244140625
;; 0 10000011001 1101011011110011010001010111111100110101101110101000

(test-fl->str (flonum->string (##rib 1049 3781445301787560 flonum-type))
	      "123456789.98765433")

;; 987654321.123456789 => 987654321.12345683574676513671875
;; 0 10000011100 1101011011110011010001011000100011111100110101101111

(test-fl->str (flonum->string (##rib 1052 3781445312040303 flonum-type))
	      "987654321.1234568")

;; 50.75 => 0 10000000100 1001011000000000000000000000000000000000000000000000

(test-fl->str (flonum->string (##rib 1028 2638827906662400 flonum-type))
	      "50.75")



;;------------------------------------------------------------------------------

;; unit tests: string->flonum

(define (test-str->fl a b)
  (if (not (equal? a b))
      (begin
	(display "test failed...\n")
	(display "expected: e = ") (display (##field0 b))
	(display " and f = ") (display (##field1 b)) (newline)
	(display "result: e = ") (display (##field0 a))
	(display " and f = ") (display (##field1 a)) (newline))
      (display "test passed...\n")))


;; special values 

;; +0.0 => 0 00000000000 0000000000000000000000000000000000000000000000000000

(test-str->fl (##string->flonum (flonum->string (##rib 0 0 flonum-type)))
	      (##rib 0 0 flonum-type))

;; -0.0 => 1 00000000000 0000000000000000000000000000000000000000000000000000

(test-str->fl (##string->flonum (flonum->string (##rib 2048 0 flonum-type)))
	      (##rib 2048 0 flonum-type))


;; +inf.0 => 0 11111111111 0000000000000000000000000000000000000000000000000000

(test-str->fl (##string->flonum (flonum->string (##rib 2047 0 flonum-type)))
	      (##rib 2047 0 flonum-type))

;; -inf.0 => 1 11111111111 0000000000000000000000000000000000000000000000000000 

(test-str->fl (##string->flonum (flonum->string (##rib 4095 0 flonum-type)))
	      (##rib 4095 0 flonum-type))
 

;; nan.0 0 11111111111 [anything that's not 0]

(test-str->fl (##string->flonum (flonum->string (##rib 2047 1 flonum-type)))
	      (##rib 2047 4503599627370495 flonum-type))
 

(test-str->fl (##string->flonum (flonum->string (##rib 2047 2251799813685249 flonum-type)))
	      (##rib 2047 4503599627370495 flonum-type))
 

(test-str->fl (##string->flonum (flonum->string (##rib 2047 4503599627370495 flonum-type)))
	      (##rib 2047 4503599627370495 flonum-type))
 

;; Format 1: k = 0 => 0.DDD

;; 1/3 => 0 01111111101 0101010101010101010101010101010101010101010101010101

(test-str->fl (##string->flonum (flonum->string (##rib 1021 1501199875790165 flonum-type)))
	      (##rib 1021 1501199875790165 flonum-type))


;; Format 2: k > 0 and k < n => D.DDD up to DDD.D
;; pi => 0 100000000000 1001001000011111101101010100010001000010110100011000

(test-str->fl (##string->flonum (flonum->string (##rib 1024 2570638124657944 flonum-type)))
	      (##rib 1024 2570638124657944 flonum-type))


;; Format 3: k > 0 and k = n => DDD.0

;; 2.0 => 0 10000000000 0000000000000000000000000000000000000000000000000000

(test-str->fl (##string->flonum (flonum->string (##rib 1024 0 flonum-type)))
	      (##rib 1024 0 flonum-type))
 

;; -2.0 => 1 10000000000 0000000000000000000000000000000000000000000000000000

(test-str->fl (##string->flonum (flonum->string (##rib 3072 0 flonum-type)))
	      (##rib 3072 0 flonum-type))


 
;; Format 4: k > 0 and k > n => DDD0...0.0

;; 1000000 => 0 10000010010 1110100001001000000000000000000000000000000000000000

(test-str->fl (##string->flonum (flonum->string (##rib 1042 4086334964629504 flonum-type)))
	      (##rib 1042 4086334964629504 flonum-type))
 

;; Format 5: k = -1 or k = -2  => 0.0DDD or 0.00DDD

;; 0.01 => 0 01111111000 0100011110101110000101000111101011100001010001111011

(test-str->fl (##string->flonum (flonum->string (##rib 1016 1261007895663739 flonum-type)))
	      (##rib 1016 1261007895663739 flonum-type))
 

;; -0.001 => 1 01111110101 0000011000100100110111010010111100011010100111111100

(test-str->fl (##string->flonum (flonum->string (##rib 3061 108086391056892 flonum-type)))
	      (##rib 3061 108086391056892 flonum-type))
 

;; Format 6: D.DDD e (sign) (k-1)

;; min subnormal positive double: 2^−1074 ≈ 4.9406564584124654 × 10^−324
;; => 0 00000000000 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 0 1 flonum-type)))
	      (##rib 0 1 flonum-type))

;; second min subnormal positive double
;; => 0 00000000000 0000000000000000000000000000000000000000000000000010

(test-str->fl (##string->flonum (flonum->string (##rib 0 4 flonum-type)))
	      (##rib 0 4 flonum-type))
 

;; max subnormal positive double: ≈ 2.2250738585072009 × 10^−308
;; => 0 00000000000 1111111111111111111111111111111111111111111111111111

(test-str->fl (##string->flonum (flonum->string (##rib 0 4503599627370495 flonum-type)))
	      (##rib 0 4503599627370495 flonum-type))
 

;; min normal positive double: ≈ 2.2250738585072014 × 10^−308
;; => 0 00000000001 0000000000000000000000000000000000000000000000000000

(test-str->fl (##string->flonum (flonum->string (##rib 1 0 flonum-type)))
	      (##rib 1 0 flonum-type))

;; SHOULD RETURN THE SAME AS ABOVE
;; min normal positive double: ≈ 2.2250738585072015 × 10^−308
;; => 0 00000000001 0000000000000000000000000000000000000000000000000000

(test-str->fl (##string->flonum (flonum->string (##rib 1 0 flonum-type)))
	      (##rib 1 0 flonum-type))

;; second min normal positive double: ≈ 2.2250738585072017 × 10^−308
;; => 0 00000000001 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 1 1 flonum-type)))
	      (##rib 1 1 flonum-type))

;; 4.450147717014404e-308
;; => 0 00000000010 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 2 1 flonum-type)))
	      (##rib 2 1 flonum-type))

;; 1.7800590868057615e-307
;; => 0 00000000100 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 4 1 flonum-type)))
	      (##rib 4 1 flonum-type))

;; 2.8480945388892184e-306
;; => 0 00000001000 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 8 1 flonum-type)))
	      (##rib 8 1 flonum-type))

;; 7.291122019556399e-304
;; => 0 00000100000 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 16 1 flonum-type)))
	      (##rib 16 1 flonum-type))

;; 4.778309726736482e-299
;; => 0 00001000000 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 32 1 flonum-type)))
	      (##rib 32 1 flonum-type))

;; 2.0522684006491886e-289
;; => 0 00010000000 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 64 1 flonum-type)))
	      (##rib 64 1 flonum-type))

;; 3.78576699573368e-270
;; => 0 00010000000 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 128 1 flonum-type)))
	      (##rib 128 1 flonum-type))

;; 1.288229753919427e-231
;; => 0 00100000000 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 256 1 flonum-type)))
	      (##rib 256 1 flonum-type))

;; 1.4916681462400417e-154
;; => 0 01000000000 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 512 1 flonum-type)))
	      (##rib 512 1 flonum-type))

;; max positive normal double : ≈ 1.7976931348623157 × 10^308
;; => 0 11111111110 1111111111111111111111111111111111111111111111111111

(test-str->fl (##string->flonum (flonum->string (##rib 2046 4503599627370495 flonum-type)))
	      (##rib 2046 4503599627370495 flonum-type))

;; second max positive normal double : ≈ 1.7976931348623156e+308
;; => 0 11111111110 1111111111111111111111111111111111111111111111111111

(test-str->fl (##string->flonum (flonum->string (##rib 2046 4503599627370494 flonum-type)))
	      (##rib 2046 4503599627370494 flonum-type))


;; 0 01111111111 1100110000110101100111100000011001111010001101001000

(test-str->fl (##string->flonum (flonum->string (##rib 1023 3592490504921928 flonum-type)))
	      (##rib 1023 3592490504921928 flonum-type))


(test-str->fl (##string->flonum (flonum->string (##rib 1006 1010493732445134 flonum-type)))
	      (##rib 1006 1010493732445134 flonum-type))


(test-str->fl (##string->flonum (flonum->string (##rib 1010 1010493732445134 flonum-type)))
	      (##rib 1010 1010493732445134 flonum-type))


;; misc

;; 1.0 => 0 01111111111 0000000000000000000000000000000000000000000000000000

(test-str->fl (##string->flonum (flonum->string (##rib 1023 0 flonum-type)))
	      (##rib 1023 0 flonum-type))
 

;; smallest number > 1 ≈ 1.0000000000000002
;; => 0 01111111111 0000000000000000000000000000000000000000000000000001

(test-str->fl (##string->flonum (flonum->string (##rib 1023 1 flonum-type)))
	      (##rib 1023 1 flonum-type))
 

;; second smallest number > 1 ≈ 1.0000000000000004
;; => 0 01111111111 0000000000000000000000000000000000000000000000000010

(test-str->fl (##string->flonum (flonum->string (##rib 1023 2 flonum-type)))
	      (##rib 1023 2 flonum-type))
 


;; 1.448997445238699 = 6525704354437805 x 2^-52
;; 0 01111111111 0111001011110001011111110001111101001001101010101101
;; mantissa (53 digits): 10111001011110001011111110001111101001001101010101101

(test-str->fl (##string->flonum (flonum->string (##rib 1023 2022104727067309 flonum-type)))
	      (##rib 1023 2022104727067309 flonum-type))
 

;; ;; 1.4489974452386991 = 6525704354437806 x 2^-52
;; ;; mantissa (53 digits): 10111001011110001011111110001111101001001101010101110

(test-str->fl (##string->flonum (flonum->string (##rib 1023 2022104727067310 flonum-type)))
	      (##rib 1023 2022104727067310 flonum-type))


;; 123456789.987654321 = 123456789.98765432834625244140625
;; 0 10000011001 1101011011110011010001010111111100110101101110101000

(test-str->fl (##string->flonum (flonum->string (##rib 1049 3781445301787560 flonum-type)))
	      (##rib 1049 3781445301787560 flonum-type))
 

;; 987654321.123456789 => 987654321.12345683574676513671875
;; 0 10000011100 1101011011110011010001011000100011111100110101101111

(test-str->fl (##string->flonum (flonum->string (##rib 1052 3781445312040303 flonum-type)))
	      (##rib 1052 3781445312040303 flonum-type))
 

;; ;; 50.75 => 0 10000000100 1001011000000000000000000000000000000000000000000000

(test-str->fl (##string->flonum (flonum->string (##rib 1028 2638827906662400 flonum-type)))
	      (##rib 1028 2638827906662400 flonum-type))
