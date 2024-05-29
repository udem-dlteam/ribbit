;;; bn.scm

;; bignums operations (two's complement, little endian representation)

;; TODO

;; normalization (bn-norm & co)[x] special case for -1 doesn't change anything
;; fold (bn-fold)              [x] same as the fold defined in ribbit
;; fixnum?                     [-] temporary def
;; bignum?                     [-] temporary def
;; fixnum->bignum              [x]
;; bignum->fixnum              [x]
;; bn-encode                   [x] need to integrate in fixnum->bignum

;; addition (bn+)              [x] logic works but needs some cleanup
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

;; bitwise not (bn~)           [x] no bitwise operators in ribbit

;; bn-zero?                    [x]
;; bn-positive?                [x]
;; bn-negative?                [x]
;; bn-even?                    [x]
;; bn-odd?                     [x]
;; bn-max                      [x]
;; variadic min                [x]
;; bn-min                      [x]
;; variadic min                [x]
;; bn-abs                      [x]
;; bn-gcd                      [x]
;; variadic gcd                [ ]
;; bn-lcm                      [x]
;; variadic lcm                [ ]


;; functions accept both fixnums and bignums  [x]

;; normalize all primary functions            [x]

;; tests (inc. stress tests for mult and quo) [ ]


;; ALWAYS REMOVE TABS BEFORE PUSHING OR MODIFY YOUR CONFIG FILE



;;------------------------------------------------------------------------------

;; utilities


;; (cond-expand
 
;;  (gambit
  
;;   (define (rib field0 field1 field2)
;;     (let ((r (make-vector 3)))
;;       (vector-set! r 0 field0)
;;       (vector-set! r 1 field1)
;;       (vector-set! r 2 field2)
;;       r))

;;   (define (rib? o) (vector? o))
;;   (define (field0 o) (vector-ref o 0))
;;   (define (field1 o) (vector-ref o 1))
;;   (define (field2 o) (vector-ref o 2))
;;   (define (field0-set! o x) (vector-set! o 0 x) o)
;;   (define (field1-set! o x) (vector-set! o 1 x) o)
;;   (define (field2-set! o x) (vector-set! o 2 x) o)
  
;;   ))


;; (define bn0 (rib 0 '_ 0))
;; (field1-set! bn0 bn0)

;; (define bn-1 (rib (- base 1) '_ 0))
;; (field1-set! bn-1 bn-1)

;; (define (car lst) (field0 lst))
;; (define (cdr lst) (field1 lst))
;; (define (cons a b) (rib a b 0))


;; ;; for ribbit only
;; (define (set-cdr! lst x)                
;;   (##field1-set! lst x))


;; constants

;; (define base 32768)

(define base 8)

;; (define base 2)

(define base-1 (- base 1))

;; (define BN_1 (cons 1 bn0))


(define bn0 (cons 0 '_))
(set-cdr! bn0 bn0)

(define bn-1 (cons base-1 '_))
(set-cdr! bn-1 bn-1)

(define (end? n)
  (or (eq? n bn0) (eq? n bn-1)))

(define (bn-fold func base lst)
  (if (pair? lst)
      (bn-fold func (func (car lst) base) (cdr lst))
      base))

(define (scan-until-false func base state lst)
  (if (and (pair? lst) state)
    (scan-until-false func (car lst) (func base (car lst)) (cdr lst))
    state))

(define (fixnum? n) ;; temp, for our current representation
  (number? n))

(define (bignum? n) ;; temp, for our current representation
  (pair? n))

(define (fixnum->bignum n)
  (if (fixnum? n)
      (if (and (< n base) (> n (- 0 base)))
          (if (eqv? 0 n)
              bn0
              (if (eqv? -1 n)
                  (cons base-1 bn-1) ;; special representation for -1
                  (if (< n 0)
                      (cons (+ base n) bn-1) 
                      (cons n bn0))))
          (bn-encode n))
      (if (bignum? n)
          n
          #f)))

(define (bignum->fixnum n)
  (if (bignum? n)
      (if (end? (cdr n))
          (if (eq? bn0 (cdr n))
              (car n)
              (if (eqv? 0 (car n))  ;; (0 $-1 $-1 ...) is not a fixnum
                  #f
                  (- 0 (bn-neg n))))
          #f) ;; conversion is not possible
      (if (fixnum? n)
          n
          #f)))


;; FIXME need to cleanup this

(define (bn-encode n)
  (define (_bn-encode n)
    (cond ((and (> n (- 0 base)) (< n base))
           (cons n bn0))
          (else
           (cons (remainder n base) (_bn-encode (quotient n base))))))
  (let ((_n (_bn-encode (abs n))))
    (if (<= 0 n)
        _n
        (bn-neg _n))))


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
      (if (and (< n base) (> n (- 0 base)))
          n
          (fixnum->bignum n))
      (let ((_n (_bn-norm n (cdr n))))
        (or (bignum->fixnum _n) _n)))) ;; return a fixnum if possible


;;------------------------------------------------------------------------------

;; ;; conversion

;; (define (bn-number->string a radix)
  
;;   (define (bn-number->string-aux a)
;;     (let* ((q (bn-norm (bn-quotient a base)))
;;         (d (bn-norm (bn+ 48 (bn-remainder a base)))))
;;       (cons d (if (bn< 0 q) (bn-number->string-aux (cdr a)) '()))))

;;   (to-string (bn-number->string-aux (fixnum->bignum a))))



;; FIXME ignore this section for now



(define (bn-number->string a)

  ;; works I guess
  (define (to-string lst)
  (fold string-append "" (map string (map integer->char lst))))

  (define (bn-number->string-aux a radix)
    (let* ((q (bn-quotient a radix))
           (d (bn-remainder a radix))
           ;; (c (if (bn< 9 d) (bn+ 65 (- d 10)) (bn+ 48 d))))
           (c (if (bn< 9 d) (+ 65 (- d 10)) (+ 48 d))))
      (cons c (if (bn< 0 q) (bn-number->string-aux (cdr a) base) '()))))

  (let ((chars (if (bn< a 0)
                   ;; (cons #\- (bn-number->string-aux (bn-abs a) base))
                   (cons 45 (bn-number->string-aux (bn-abs a) 10))
                   (bn-number->string-aux (bn-norm a) 10))))
    (to-string chars)))


(define (bn-string->number str radix)

  (define (convert-16 c) ;; not sure what the 16 refers to tbh
    (cond 
      ((and (bn< 47 c) (bn< c 58)) (bn- c 48))   ;; 0-9
      ((and (bn< 64 c) (bn< c 71)) (bn- c 65))   ;; A-F
      ((and (bn< 96 c) (bn< c 103)) (bn- c 97))  ;; a-f
      (else #f)))

  (define (convert c)
    (if (and (bn< 47 c) (bn< c 58))
        (bn- c 48)
        #f))

  (define (string->number-aux lst)
    (if (null? lst) ;; FIXME: remove the null? check
        #f
        (string->number-aux2 lst 0 (if (eqv? radix 16) convert-16 convert))))

  (define (string->number-aux2 lst n converter)
    (if (pair? lst)
      (let* ((c (car lst))
             (x (converter c)))
        (if x
            (string->number-aux2 
             (cdr lst) ;; cdr
             (bn+ (bn* radix n) x)
              converter)
            #f))
      n))

  (let ((lst (string->list str)))
    (if (null? lst)
        #f
        (if (eqv? (car lst) 45) ;; negative number?
            (let ((n (string->number-aux (cdr lst))))
              (and n (bn- 0 n)))
            (string->number-aux lst)))))


;;------------------------------------------------------------------------------

;; arithmetic


;; possible cases for addition 

;; - parity(a) != parity(b) => no "overflow" possible 
;;    - carry => result will be positive => bn0
;;    - no carry => result will be negative => bn-1

;; - parity(a) == parity(b) => possible "overflow"
;;    - carry => add carry and then same as below 
;;    - no carry => result will have the same parity as both a and b => parity(a)
;;        (intuition: sum of pos (neg) numbers will always be pos (neg))

(define (bn+ a b)
  (if (and (fixnum? a) (fixnum? b))
      (bn-norm (+ a b))
      (let ((_a (fixnum->bignum a))
            (_b (fixnum->bignum b)))
        (bn-norm (_bn+ _a _b 0)))))

;; bignum addition without normalization for intermediate calcualtions
(define (##bn+ a b)
  (if (and (fixnum? a) (fixnum? b))
      (fixnum->bignum (+ a b))
      (let ((_a (fixnum->bignum a))
            (_b (fixnum->bignum b)))
        (_bn+ _a _b 0))))

(define (_bn+ a b carry)
  (if (and (end? a) (end? b))
      (if (eq? a b)
          (if (eqv? 0 carry)
              (if (eq? a bn0) bn0 bn-1)
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


(define (var-bn+ . args)
  (bn-fold bn+ bn0 args))


(define (bn- a b)
  (bn+ a (bn-neg b)))


(define (bn-u a)
  (bn- bn0 a))


(define (bn-remainder a b)
  (bn- a (bn* b (bn-quotient a b))))


;; FIXME not sure if I need to modify anything here to deal with fixnums

(define (bn-modulo a b)
  (let ((r (bn-remainder a b)))
    (if (eq? r bn0)
        bn0
        (if (eqv? (bn< a bn0) (bn< b bn0))
            r
            (bn+ r b)))))

;;------------------------------------------------------------------------------

;; multiplication (need a better algorithm)


;; long multiplication in O(n^2) so that we have something that works for now

(define (bn* a b)

  ;;   (define (_bn* a b carry)  ;; one line multiplication
  ;;     (if (end? a)
  ;;         (if (eqv? 0 carry) bn0 (cons carry bn0))
  ;;         (let* (;; (_a (fixnum->bignum a))
  ;;                ;; (_b (fixnum->bignum b))
  ;;                (__a (car _a))
  ;;                (__b (car _b))
  ;;                (res (+ (* __a __b) carry))
  ;;                (rem (modulo res base))
  ;;                (quo (quotient res base)))
  ;;           (cons rem (_bn* (cdr _a) _b quo)))))
  
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
        (let* ((_a (fixnum->bignum a))
               (_b (fixnum->bignum b))
               (res (_bn* _a _b 0))
               (pad (cons 0 _a)))
          (bn+ res (__bn* pad (cdr _b))))))

  (if (and (fixnum? a) (fixnum? b))
      (bn-norm (* a b))
      (let ((_a (if (fixnum? a) (fixnum->bignum a) a))
            (_b (if (fixnum? b) (fixnum->bignum b) b)))
        (if (eqv? (bn< _a bn0) (bn< _b bn0))
            (bn-norm (__bn* (bn-abs _a) (bn-abs _b)))
            (bn-norm (bn-neg (__bn* (bn-abs _a) (bn-abs _b))))))))

(define (var-bn* . args)
  (bn-fold bn* (cons 1 bn0) args))


;; faster algorithms

;;  - Booth's multiplication algorithm (specifically for two's complement)

;;  - Karatsuba O(n^(ln 3)) ≈ O(n^1.585)

;;  - Schönhage–Strassen based on fft O(n log(n) log(log(n)))

;;  - Tom-Cook (or Tom-3)


;;------------------------------------------------------------------------------

;; quotient (need a better algorithm)

;; FIXME currently VERY VERY slow 

(define (bn-quotient a b)
  (define (_bn-quotient a b quo)
    (let ((_a (fixnum->bignum a))
          (_b (fixnum->bignum b)))
      (if (bn< (bn-abs a) (bn-abs b))
          quo
          (if (eqv? (bn< _a bn0) (bn< _b bn0))
              (_bn-quotient (bn- _a _b) _b (bn+ quo (cons 1 bn0)))
              (_bn-quotient (bn+ _a _b) _b (bn+ quo (cons 1 bn-1)))))))
  (if (and (fixnum? a) (fixnum? b))
      (quotient a b)
      (let ((_a (if (fixnum? a) (fixnum->bignum a) a))
            (_b (if (fixnum? b) (fixnum->bignum b) b)))
        (if (eq? _b bn0)
            (quotient 0 0) ;; just to trigger an error
            (bn-norm (_bn-quotient a b bn0))))))


;;------------------------------------------------------------------------------

;; comparison

;; (define (bn= a b)
;;   (if (and (pair? a) (pair? b))
;;       (if (and (end? a) (end? b))
;;           (eq? a b)
;;           (and (eqv? (car a) (car b)) (bn= (cdr a) (cdr b))))
;;       (if (number? a)
;;           (if (number? b)
;;               (eqv? a b)
;;               (bn= (fixnum->bignum a) b))
;;           (bn= a (fixnum->bignum b)))))

(define (bn= a b)
  (eqv? (bn- a b) 0))

(define (var-bn= a . rest)
  (scan-until-false eqv? a #t rest))

(define (bn< a b)
  (bn-negative? (bn- a b)))

(define (var-bn< a . rest) 
  (scan-until-false bn< a #t rest))

(define (bn<= a b)
  (or (bn= a b) (bn< a b)))

(define (var-bn<= a . rest)
  (scan-until-false (lambda (a b) (not (bn< b a))) a #t rest))

(define (bn> a b)
  (not (bn<= a b)))

(define (var-bn> a . rest) 
  (scan-until-false (lambda (a b) (bn< b a)) a #t rest))

(define (bn>= a b)
  (not (bn< a b)))

(define (var-bn>= a . rest) 
  (scan-until-false (lambda (a b) (not (bn< a b))) a #t rest))


;;------------------------------------------------------------------------------

;; bitwise operations

(define (bn~ a)
  (define (_bn~ a)
    (if (end? a)
        (if (eq? a bn0) bn-1 bn0)
        (let* ((_a (car a))
               (res (- base-1 _a)))
          (cons res (_bn~ (cdr a))))))
  (bn-norm (_bn~ (fixnum->bignum a))))

;; FIXME only works with gambit since ribbit doesn't support bitwise operations

;; (cond-expand

;;  (gambit

;;   (define (bn-and a b)
;;     (if (and (end? a) (end? b))
;;         (if (and (equal? a bn-1) (equal? b bn-1)) bn-1 bn0)
;;         (let ((res (bitwise-and (car a) (car b))))
;;           (cons res (bn-and (cdr a) (cdr b))))))

;;   (define (bn-ior a b)
;;     (if (and (end? a) (end? b))
;;         (if (or (equal? a bn-1) (equal? b bn-1)) bn-1 bn0)
;;         (let ((res (bitwise-ior (car a) (car b))))
;;           (cons res (bn-or (cdr a) (cdr b))))))

;;   (define (bn-xor a b)
;;     (if (and (end? a) (end? b))
;;         (if (equal? a b) bn0 bn-1)
;;         (let ((res (bitwise-xor (car a) (car b))))
;;           (cons res (bn-xor (cdr a) (cdr b))))))

;;  ))

;; TODO bit shifts currently only work with base 2

;; (define (bn-shl a)
;;   (cons 0 a))

;; (define (bn-shift-left a n) ;; should probably treat the case where n < 0
;;   (if (equal? n 0)
;;       a
;;       (bn-shift-left (bn-shl a) (- n 1))))

;; (define (bn-shr a) ;; (arithmetic shift)
;;   (cdr a))

;; (define (bn-shift-right a n) ;; should probably treat the case where n < 0
;;   (if (equal? n 0)
;;       a
;;       (bn-shift-right (bn-shr a) (- n 1))))


;;------------------------------------------------------------------------------

;; misc (mostly just adapted from ribbit's own definitions)

(define (bn-neg a)
  (bn+ (bn~ a) (cons 1 bn0)))

(define (bn-zero? a)
  (bn= a bn0))

(define (bn-positive? a)
  (bn< bn0 a))

(define (bn-negative? a) ;; can't define it with bn< since we use it there
  (let ((_a (fixnum->bignum a)))
    (if (eq? _a bn0)
        #f
        (or (eq? _a bn-1) (bn-negative? (cdr _a))))))

(define (bn-even? a)
  (let ((_a (bn-abs a))
        (_2 (fixnum->bignum 2)))
    (eqv? 0 (bn-modulo _a _2))))

(define (bn-odd? a)
  (not (bn-even? a)))

(define (bn-max a b)
  ;; (if (bn< a b) (bn-norm b) (bn-norm a)))
  (if (bn< a b) b a))

(define (var-bn-max a . rest)
  (fold (lambda (curr best)
          (if (bn< best curr)
              curr ;; (bn-norm curr)
              best)) ;; (bn-norm best)))
        a rest))

(define (bn-min a b)
  ;; (if (bn< a b) (bn-norm a) (bn-norm b)))
  (if (bn< a b) a b))

(define (var-bn-min a . rest)
  (fold (lambda (curr best)
          (if (bn< best curr)
              best ;; (bn-norm best)
              curr)) ;; (bn-norm curr)))
        a rest))

(define (bn-abs a)
  (let ((_a (fixnum->bignum a)))
    (if (bn< _a bn0) (bn-u _a) (bn-norm _a))))

(define (bn-gcd a b)
  (let ((_a (bn-abs a))
        (_b (bn-abs b)))
    (if (bn< _a _b)
        (bn-gcd-aux _a _b)
        (bn-gcd-aux _b _a))))

(define (bn-gcd-aux a b)
  (if (or (eq? a bn0) (eqv? a 0)) ;; (bn= a bn0)
      (bn-norm b)
      (bn-gcd-aux (bn-remainder b a) a)))

(define (bn-lcm a b)
  (if (or (eq? b bn0) (eqv? n 0)) ;; (bn= b bn0)
      (let ((_a (bn-abs a))
            (_b (bn-abs b)))
        (bn* (bn-quotient _a (bn-gcd _a _b)) _b))))


;;------------------------------------------------------------------------------

;; unit tests


;; utilities

;; FIXME define-macro doesn't work 

;; (define-macro (test a b)
;;   `(let ((_a ,a)
;;          (_b ,b))
;;      (if (not (bn= _a _b))
;;          (begin
;;            (display "results not matching: ") (pp ',a)
;;            (display "a: ") (_pp _a)
;;            (display "b: ")(_pp _b)
;;            (newline)))))

(define (test a b)
  (if (not (bn= a b))
      (begin
        (display "results not matching: ") 
        (display "a: ") (_pp a)
        (display "b: ")(_pp b)
        (newline))
      (_pp a)))
      ;;))

;; for comparison tests

;; (define-macro (test2 a b)
;;   `(let ((_a ,a)
;;          (_b ,b))
;;      (if (not (eq? _a _b))
;;          (begin
;;            (display "results not matching: ") (pp ',a)
;;            (display "a: ") (pp _a)
;;            (display "b: ")(pp _b)
;;            (newline)))))

(define (test2 a b)
  (if (not (eqv? a b))
      (begin
        (display "a comparison test failed")
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

;; utilities


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
(test (bn-norm bn0) bn0)

;; representation of -1
(test (bn-norm (cons (- base 1) bn-1)) -1)



;;------------------------------------------------------------------------------

;; arithmetic


;; addition

;; note on the first test: we represent -1 with (cons 1 bn-1) instead of just bn-1

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
      (cons (- base 2) (cons 1 bn-1)))

(test (bn+ bn0 bn0) bn0)

(test (bn+ bn0 bn-1) -1) ;; bn-1)

(test (bn+ bn-1 bn0) -1) ;; bn-1)

;; new representation of -1, we assume this case will never happen

;; (test (bn+ bn-1 bn-1) -2) ;; (cons (- base 2) bn-1))



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



;; unary substraction

(test (bn-u bn0) 0) ;; bn0)

(test (bn-u (cons (- base 1) bn-1)) 1) ;; (cons 1 bn0))



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

(test (bn* 1234 56789) 70077626)

(test (bn* -1234 56789) -70077626)

(test (bn* 1234 -56789) -70077626)

(test (bn* -1234 -56789) 70077626)


;; quotient 

;; (quotient pos pos) => pos 
(test (bn-quotient (cons (- base 1) (cons (- base 1) bn0))
                   (cons 0 (cons (- base 1) bn0)))
      (cons 1 bn0))

;; (quotient neg pos) => neg
(test (bn-quotient (cons (- base 1) (cons 0 bn-1))
                   (cons 0 (cons (- base 1) bn0)))
      (cons 1 bn-1))

;; (quotient pos neg) => neg
(test (bn-quotient (cons 0 (cons (- base 1) bn0))
                   (cons 0 (cons (- base 1) bn-1)))
      bn-1)

;; (quotient neg neg) => pos
(test (bn-quotient (cons (- base 1) (cons 0 bn-1))
                   (cons 0 bn-1))
      (cons (- base 1) bn0))

;; a and b equal => 1
(test (bn-quotient (cons (- base 1) (cons (- base 1) bn0))
                   (cons (- base 1) (cons (- base 1) bn0)))
      (cons 1 bn0))


;; division by 0, error
;; (test (bn-quotient bn0 bn0)
;;       "error: division by 0")


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


;; remainder

;; (remainder pos pos) => pos
(test (bn-remainder (cons (- base 1) (cons 0 (cons (- base 1) bn0)))
                    (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons 1 (cons 0 bn0))))

;; (remainder pos neg) => pos
(test (bn-remainder (cons (- base 1) (cons (- base 1) bn0))
                    (cons (- base 1) (cons (- base 2) bn-1)))
      (cons (- base 2) (cons (- base 2) bn0)))

;; (remainder neg pos) => neg
(test (bn-remainder (cons (- base 1) (cons (- base 1) (cons 0 bn-1)))
                    (cons (- base 1) (cons (- base 1) bn0)))
      (cons (- base 2) (cons base-1 (cons 1 bn-1))))

;; (remainder neg neg) => neg
(test (bn-remainder (cons (- base 1) (cons (- base 1) (cons 0 bn-1)))
                    (cons (- base 1) (cons 0 bn-1)))
      (cons (- base 2) (cons 1 (cons (- base 1) bn-1))))

;; (remainder a b) where |a| == |b| => 0
(test (bn-remainder (cons (- base 1) bn0)
                    bn-1)
      (cons (- base 2) bn0))



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


;;------------------------------------------------------------------------------

;; comparison


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


;; less or equal

(test2 (bn<= bn0 bn0) #t)

(test2 (bn<= (cons (- base 1) bn-1) (cons (- base 1) bn-1)) #t)

(test2 (bn<= bn0 (cons (- base 1) bn-1)) #f)

(test2 (bn<= (cons (- base 1) bn-1) bn0) #t)

(test2 (bn<= -100 101) #t)

(test2 (bn<= -100 0) #t)

(test2 (bn<= -100 100) #t)

(test2 (bn<= 100 100) #t)

(test2 (bn<= 100 -100) #f)


;; greater than

(test2 (bn> bn0 bn0) #f)

(test2 (bn> (cons (- base 1) bn-1) (cons (- base 1) bn-1)) #f)

(test2 (bn> bn0 (cons (- base 1) bn-1)) #t)

(test2 (bn> (cons (- base 1) bn-1) bn0) #f)

(test2 (bn> -100 101) #f)

(test2 (bn> -100 0) #f)

(test2 (bn> -100 100) #f)

(test2 (bn> 100 100) #f)

(test2 (bn> 101 -100) #t)


;; greater or equal

(test2 (bn>= bn0 bn0) #t)

(test2 (bn>= (cons (- base 1) bn-1) (cons (- base 1) bn-1)) #t)

(test2 (bn>= bn0 (cons (- base 1) bn-1)) #t)

(test2 (bn>= (cons (- base 1) bn-1) bn0) #f)

(test2 (bn>= -100 101) #f)

(test2 (bn>= -100 0) #f)

(test2 (bn>= -100 100) #f)

(test2 (bn>= 100 100) #t)

(test2 (bn>= 101 -100) #t)


;;------------------------------------------------------------------------------

;; misc

;; negation

(test (bn-neg bn0) bn0)

(test (bn-neg bn-1) (cons 1 bn0))

(test (bn-neg (cons 1 bn0)) bn-1)


;; absolute value

