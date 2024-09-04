;;; Bignum library

;;==============================================================================

;; Utilities

;;(define-feature _ (use scheme-bignum))

(define (bn-cons car cdr) (##rib car cdr bignum-type))
(define bn-digit ##field0)
(define bn-next ##field1)

(define base (##+ 32767 1)) ;; `base` represented as a fixnum
(define bn-base 32768) ;; `base` represented as a bignum
(define base-1 (##- base 1))
(define max-fixnum (##* base base)) ;; FIXME
(define min-fixnum (##- 0 max-fixnum)) ;; FIXME
(define bn0 ##bn0)
(define bn-1 ##bn-1)
(define bn1 (bn-cons 1 bn0))

(define (end? n) (or (##eqv? n bn0) (##eqv? n bn-1)))


(define (fixnum? n)
  (not (##rib? n)))

(define (var-fixnum? lst)
  (cond ((null? lst) #f)
        ((null? (##field1 lst)) (fixnum? (##field0 lst)))
        (else (and (fixnum? (##field0 lst))
                   (var-fixnum? (##field1 lst))))))

(define (bignum? n)
  (and (##rib? n) (##eqv? (##field2 n) bignum-type)))

;;------------------------------------------------------------------------------

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

;;------------------------------------------------------------------------------

(define (bn-encode n)
  
  (define (_bn-encode n)
    (if (and (##< (##- (##- 0 1) base) n) (##< n base))
        (bn-cons n bn0)
        (bn-cons (##fx-remainder n base) (_bn-encode (##quotient n base)))))

  (if (bignum? n)
      n
      (let ((_n (_bn-encode (if (##< n 0) (##- 0 n) n))))
        (if (##< (##- 0 1) n) _n (##bn-neg _n)))))

;; helper function to encode bignums

(define $ bn-encode)


(define (bn-decode n) ;; will overflow if n can't be represented as a fixnum

  (define (_bn-decode n e)
    (if (end? (bn-next n))
        (##* (bn-digit n) e)
        (##+ (##* (bn-digit n) e) (_bn-decode (bn-next n) (##* e base)))))

  (if (bignum? n)
      (if (##bn< n bn0)
          (##- 0 (_bn-decode (##bn-abs n) 1))
          (_bn-decode n 1))
      n)) ;; assumes n is a fixnum


;;------------------------------------------------------------------------------

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

(define (bn-length a)
  (define (_bn-length a counter)
    (if (end? a) 
        counter
        (_bn-length (bn-next a) (bn+ counter 1))))
  (if (fixnum? a)
      1
      (_bn-length a 0)))

(define (bn-str-length a)
  (define (_bn-str-length a counter)
    (if (##eqv? '() a) 
        counter
        (_bn-str-length (##field1 a) (##+ counter 1))))
  (if (and (##rib? a) (##eqv? (##field2 a) pair-type))
      (_bn-str-length a 0)
      #f))


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



;;==============================================================================

;; Numerical operations


;;------------------------------------------------------------------------------

;; Numerical predicates


;; Comparison predicates


(define (##bn= a b) 
  (##eqv? (bn- a b) 0)) ;; FIXME tests won't work with (bn-norm (##bn- a b) bn0)

(define (var-bn= a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-scan-until-false ##eqv? a #t rest)
      (bn-scan-until-false
       ##bn= (fixnum->bignum a) #t (bn-map fixnum->bignum rest))))


(define (##bn< a b)
  (##bn-negative? (##bn- a b)))

(define (var-bn< a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-scan-until-false ##< a #t rest)
      (bn-scan-until-false
       ##bn< (fixnum->bignum a) #t (bn-map fixnum->bignum rest))))


;; Numerical properties predicates

(define (##bn-zero? a)
  (##bn= a bn0))

(define (##bn-positive? a)
  (##bn< bn0 a))

(define (bn-negative? a)
  (if (fixnum? a)
      (##< a 0)
      (##bn-negative? a)))

(define (##bn-negative? a)
  (if (##eqv? a bn0)
      #f
      (or (##eqv? a bn-1) (##bn-negative? (bn-next a)))))


(define (##bn-even? a)
  (##eqv? 0 (##fx-modulo (bn-digit a) 2)))


(define (##bn-odd? a)
  (##eqv? 1 (##fx-modulo (bn-digit a) 2)))


;;------------------------------------------------------------------------------

;; Max and min

(define (##bn-max a b)
  (if (##bn< a b) b a))

(define (var-bn-max a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-fold (lambda (x y) (if (##< x y) y x)) a rest)
      (bn-norm (bn-fold
                ##bn-max (fixnum->bignum a) (bn-map fixnum->bignum rest)))))

(define (##bn-min a b)
  (if (##bn< a b) a b))

(define (var-bn-min a . rest) 
  (if (and (fixnum? a) (var-fixnum? rest))
      (bn-fold (lambda (x y) (if (##< x y) x y)) a rest)
      (bn-norm (bn-fold
                ##bn-min (fixnum->bignum a) (bn-map fixnum->bignum rest)))))


;;------------------------------------------------------------------------------

;; Arithmetic operations: +, *, -, and /


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
               (rem (##fx-modulo res base))
               (quo (##quotient res base)))
          (bn-cons rem (_bn+ (bn-next a) (bn-next b) quo)))))

  (_bn+ a b 0))

(define (var-bn+ . args) ;; note: (var-bn+) => 0, consistent with gambit's `+`
  (if (var-fixnum? args)
      (bn-norm (bn-fold ##+ 0 args))
      (bn-norm (bn-fold ##bn+ bn0 (bn-map fixnum->bignum args)))))


;; Multiplication

(define (##bn-fx* a b)

  ;; assumes a is a bignum and b is a fixnum

  (define (##bn-fx*-aux a b carry) ;; FIXME same as one line mult below
    (if (end? a)
        (if (##eqv? 0 carry) bn0 (bn-cons carry bn0))
        (let* ((_a (bn-digit a))
               (res (##+ (##* _a b) carry))
               (rem (##fx-modulo res base))
               (quo (##quotient res base)))
          (bn-cons rem (##bn-fx*-aux (bn-next a) b quo)))))

  (let ((_a (##bn-abs a))
        (_b (if (##< b 0) (##- 0 b) b)))
    (if (##eqv? (##bn< a bn0) (##< b 0))  
        (##bn-fx*-aux _a _b 0)
        (##bn-neg (##bn-fx*-aux _a _b 0)))))
  
;; TODO booth's multiplication algorithm, karatsuba, strassen, toom-cook...

(define (##bn* a b)
  
  (define (_bn* a b carry)  ;; one line multiplication
    (if (end? a)
        (if (##eqv? 0 carry) bn0 (bn-cons carry bn0))
        (let* ((_a (bn-digit a))
               (res (##+ (##* _a b) carry))
               (rem (##fx-modulo res base))
               (quo (##quotient res base)))
          (bn-cons rem (_bn* (bn-next a) b quo)))))
  
  (define (__bn* a b) ;; full (positive) mutiplication
    (if (end? b)
        bn0 
        (let ((res (_bn* a (bn-digit b) 0))
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


;; substraction

(define (##bn- a b)
  (##bn+ a (##bn-neg b)))


;; unary substraction

(define (bn-u a)
  (if (fixnum? a)
      (##- 0 a) ;; no need to normalize the result
      (bn-norm (##bn-u a))))

(define (##bn-u a)
  (##bn- bn0 a))


;;------------------------------------------------------------------------------

;; Absolute value

(define (##bn-abs a)
  (if (##bn< a bn0) (##bn- bn0 a) a))


;;------------------------------------------------------------------------------

;; Quotient, remainder, and modulo


(define (bn-get a j n) ;; returns a bignum made of j up to n from a's digits
  
  (define (_bn-get a n)
    (if (##eqv? 0 n)
        bn0
        (bn-cons (bn-digit a) (_bn-get (bn-next a) (##- n 1)))))

  (define (skip a j n)
    (if (##< 0 j)
        (skip (bn-next a) (##- j 1) n)
        (_bn-get a n)))

  (skip a j n))

(define (msb a) ;; returns the most significant digit as a fixnum
  (if (end? (bn-next a))
      (bn-digit a)
      (msb (bn-next a))))

(define (bn-append a b i)
  ;; should read "appends b to a at index i"
  (if (##eqv? 0 i)
      b
      (bn-cons (bn-digit a) (bn-append (bn-next a) b (##- i 1)))))


(define (##bn-quotient a b)
  (let* ((_a (##bn-abs a))
         (_b (##bn-abs b)))
    (cond ((##eqv? b bn0) ;; division by 0
           (##quotient 0 0))
          ((##eqv? b bn1) ;; should probably do the same for bn-1
           a)
          ((##bn< _a _b) ;; abs(a) < abs(b) => 0
           bn0)
          ((##eqv? (##bn< bn0 a) (##bn< bn0 b)) ;; positive quotient? (same parity)
          ;;  (##bn-quotient-aux _a _b))
          ;; (else ;; negative quotient
          ;;  (##bn-neg (##bn-quotient-aux _a _b))))))
           
           ;; if b is a fixnum, we need to pad both a and b with a 0 for the
           ;; algorithm to work since it requires a >= b >= bignum's base
           ;; (quotient will be the same since the ratio remains the same)   

           ;; FIXME leaving that here until end and bignum->fixnum is fixed
           
           (if (end? (bn-next _b)) ;; b was a fixnum?
               (##bn-quotient-aux (bn-cons 0 _a) (bn-cons 0 _b))
               (##bn-quotient-aux _a _b)))        
          (else ;; negative quotient
           (if (end? (bn-next _b)) ;; b was a fixnum?
               (##bn-neg (##bn-quotient-aux (bn-cons 0 _a) (bn-cons 0 _b)))
               (##bn-neg (##bn-quotient-aux _a _b)))))))

(define (##bn-fx-quotient a b)

  ;; quotient between bignum and fixnum

  (define (##bn-fx-quotient-aux a b n rem tail)
    (if (##< n 0)
        tail
        (let* ((rb+u_j (##+ (##* rem base) (msb a)))
               (quo (##quotient rb+u_j b))
               (_rem (##fx-remainder rb+u_j b)))
          (##bn-fx-quotient-aux (bn-get a 0 n) b (##- n 1) _rem (bn-cons quo tail)))))

  (let* ((_a (##bn-abs a))
         (_b (if (##< b 0) (##- 0 b) b)))
    (cond ((##eqv? b 0) ;; division by 0
           (##quotient 0 0))
          ((##eqv? b 1) ;; should probably do the same for -1
           a)
          ((##eqv? (##bn< bn0 a) (##< 0 b)) ;; positive quotient? (same parity)
           (##bn-fx-quotient-aux _a _b (##- (bn-length _a) 1) 0 bn0))
          (else
           (##bn-neg (##bn-fx-quotient-aux _a _b (##- (bn-length _a) 1) 0 bn0))))))


(define (##bn-quotient-aux a b)

  ;; algorithm D, Knuth's TAOCP vol. 2 ch. 4.3.1
  ;; assumes both a and b are bignums
  
  (define (calculate-q-hat top3-a b_n-1 b_n-2)
    
    ;; D3. first approximation of q-hat, returns a fixnum
    
    (let* ((a_j+n (bn-digit (bn-next (bn-next top3-a))))
           (a_j+n-1 (bn-digit (bn-next top3-a)))
           (a_j+n-2 (bn-digit top3-a))
           (div (##+ (##* base a_j+n) a_j+n-1)) ;; dividend < max-fixnum
           (q-hat (##quotient div b_n-1))
           ;; (r-hat (##fx-remainder div b_n-1)))
           (r-hat (##fx-modulo div b_n-1)))

      (if (or (##< base-1 q-hat)
              (##< (##+ (##* base r-hat) a_j+n-2)
                   (##* q-hat b_n-2)))
          (let ((q-hat (##- q-hat 1))
                (r-hat (##+ r-hat b_n-1)))
            (if (and (##< r-hat base)
                     (or (##< base-1 q-hat)
                         (##< (##+ (##* base r-hat) a_j+n-2)
                              (##* q-hat b_n-2))))
                (##- q-hat 1)
                q-hat))
          q-hat)))
  

  (define (add-back top-a b q-hat) 
    
    ;; D6. Decrement q-hat (q_j) and add back to the divisor until remainder
    ;; is no longer negative, returns a fixnum
    
    (if (##bn< (##bn- top-a (##bn-fx* b q-hat)) bn0)
        (add-back top-a b (##- q-hat 1))
        q-hat))

    
  (let* ((d (##quotient base (##+ (msb b) 1)))  ;; FIXME pick a power of 2 for d instead
         (_a (##bn-fx* a d)) ;; normalize a and b
         (_b (##bn-fx* b d))
         
         (length-a (bn-length _a))  ;; |a| = m + n 
         (length-b (bn-length _b))  ;; |b| = n
         (m (##- length-a length-b))  ;;  m = |a| - |b|
         
         (top2-b (bn-get _b (##- length-b 2) 2))
         (b_n-1 (bn-digit (bn-next top2-b)))
         (b_n-2 (bn-digit top2-b)))
    
    (let loop (;;(j (##- m 1))
               (j m)
               (q bn0)
               (a _a))
      (if (##< j 0)
          q
          (let* ((top-a (bn-get a j (##+ length-b 1))) ;; (a_j+n ... a_j) i.e. |n|+1 digits
                 (top3-a ;; top 3 digits from (a_j+n ... a_j), some could be 0s
                  (bn-get top-a (##- (bn-length top-a) 3) 3)))
            
            (let* ((q-hat-estimate (calculate-q-hat top3-a b_n-1 b_n-2))
                   (_new-top-a (##bn- top-a (##bn-fx* _b q-hat-estimate))))
              
              (if (##bn< _new-top-a bn0) ;; need to adjust q-hat's estimate?
                   (let* ((q-hat (add-back top-a _b q-hat-estimate))
                          (new-top-a (##bn- top-a (##bn-fx* _b q-hat))))
                     (loop (##- j 1)
                           (bn-cons q-hat q)
                           (bn-append a new-top-a j)))
                   (loop (##- j 1)
                         (bn-cons q-hat-estimate q)
                         (bn-append a _new-top-a j)))))))))




(define (##bn-fx-remainder a b)
  (##bn- a (##bn-fx* (##bn-fx-quotient a b) b)))

(define (##bn-remainder a b)
  (##bn- a (##bn* (##bn-quotient a b) b)))


(define (##bn-modulo a b)
  (let ((r (##bn-remainder a b)))
    (if (##eqv? r bn0)
        bn0
        (if (##eqv? (##bn< a bn0) (##bn< b bn0))
            r
            (##bn+ r b)))))


;;------------------------------------------------------------------------------

;; Gcd and lcm

(define (##bn-gcd a b)
  (let ((_a (##bn-abs a))
        (_b (##bn-abs b)))
    (if (##bn< _a _b) (##bn-gcd-aux _a _b) (##bn-gcd-aux _b _a))))

(define (##bn-gcd-aux a b)
  (display a) (newline)
  (if (##bn= a bn0) 
      b
      (##bn-gcd-aux (##bn-remainder b a) a)))

(define (var-bn-gcd . args) ;; (var-bn-gcd) => 0, consistent with gambit's `gcd`
  (if (var-fixnum? args)
      (bn-fold ##gcd 0 args)
      (bn-norm (bn-fold ##bn-gcd bn0 (bn-map fixnum->bignum args)))))

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

;; bitwise operations

(define (bn~ a)
  (if (fixnum? a)
      ;; FIXME need to implement bitwise-not in ribbit
      (##bn~ (fixnum->bignum a)) ;; (bn-norm (bitwise-not a))
      (##bn~ a)))

(define (##bn~ a)
  (if (end? a)
      (if (##eqv? a bn0) bn-1 bn0)
      (let ((_a (##- (##- base 1) (bn-digit a))))
        (bn-cons _a (##bn~ (bn-next a))))))


;; negation

(define (bn-neg a)
  (if (fixnum? a)
      (##- 0 a) 
      (bn-norm (##bn-neg a))))

(define (##bn-neg a)
  (##bn+ (##bn~ a) bn1))


;;==============================================================================

;; Numerical input and output

;; FIXME incomplete and redundant type checks

;; need these for now because I can't assume the type of some argument in io
;; functions.... FIXME

(define (bn+ a b)
  (if (and (fixnum? a) (fixnum? b))
      (bn-norm (##+ a b))
      (bn-norm (##bn+ (fixnum->bignum a) (fixnum->bignum b)))))

(define (bn* a b)
  (cond ((and (fixnum? a) (fixnum? b))
         (bn-norm (##* a b)))
        ((fixnum? b)
         (bn-norm (##bn-fx* a b)))
        ((fixnum? a)
         (bn-norm (##bn-fx* b a)))
        (else
         (bn-norm (##bn* a b)))))

(define (bn- a b)
  (if (and (fixnum? a) (fixnum? b))
      (bn-norm (##- a b))
      (bn-norm (##bn- (fixnum->bignum a) (fixnum->bignum b)))))


(define (bn-quotient a b)
  (cond ((and (fixnum? a) (fixnum? b))
         (##quotient a b)) ;; no need to normalize
        ((fixnum? b)
         (bn-norm (##bn-fx-quotient a b)))
        (else
         (bn-norm (##bn-quotient (fixnum->bignum a) b)))))


(define (##bn-number->string a)

  (define (##bn-number->string-aux _a tail radix)
    (let* ((quo (bn-quotient _a radix)) ;; FIXME redundant type check
           ;;(rem (bn-remainder _a radix)) ;; FIXME redundant type check
           (rem (bn- _a (bn* radix quo)))
           (chars (##rib (##+ 48 rem) tail pair-type)))
      (if (##eqv? 0 quo) ;; won't work if quo is not normalized 
          chars
          (##bn-number->string-aux quo chars radix))))

  (let* ((radix 10)
         (chars (if (##bn< a bn0)
                    (##rib 45
                           (##bn-number->string-aux (##bn-abs a) '() radix)
                           pair-type)
                    (##bn-number->string-aux a '() radix))))
    
    (##rib chars (bn-str-length chars) string-type))) ;; FIXME ##length?


;; bignum's string->number

(define (##bn-string->number str)

  (define (convert char)
    (if (and (##< 47 char) (##< char 58)) ;; 0-9
        (##- char 48)
        #f))

  (define (##bn-string->number-aux lst number)
    (if (null? lst) 
        number
        (let* ((char (##field0 lst))
               (digit (convert char)))
          (if digit
              (##bn-string->number-aux (##field1 lst)
                                     (bn+ (bn* 10 number) digit)) ;; FIXME ##bn?
              #f))))

  (let ((lst (##field0 str)))
    (if (null? lst)
        #f
        (if (##eqv? (##field0 lst) 45) ;; negative?
            (let ((n (##bn-string->number-aux (##field1 lst) 0)))
              (and n (bn-u n))) 
            (##bn-string->number-aux lst 0))))) ;; FIXME normalize?

