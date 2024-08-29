;;; File: nums.scm

;;; Author: Frédéric Lahaie-Bertrand

;; * Nums
;;    - cond-expand macro (for dispatch)
;;    - variadics macro
;;    - typecheck macro
;;    - compact feature
;;    - new logic for overflow
;;    - support all RnRS procedures
;;    - error handling
;; * Fixnums
;;    - overflow primitives: ##+?, ##*?, ##-?, ##/?, ... 
;; * Bignums
;;    - remove type checking from bignum.scm
;;    - multiplication algorithm
;;    - specialized tests for algos
;;    - vector implementation
;; * Flonums
;;    - conversions between numeric types (fixnum->flonum, bignum->flonum)
;;    - support for odd?, even?, quotient, remainder, modulo, gcd, lcm
;;    - subnormal numbers (minor bug)
;;    - double-precision support
;;    - parsing algorithms (optimize)
;;    - tests
;;    - support for reference hosts: asm, c, hs, js, py (ieee32 and ieee64)
;; * Documentation
;;    - Cleanup readme and include new changes

;;==============================================================================

(##include-once (ribbit "define-macro"))

(if-feature (or nums/fixnum nums/bignum nums/flonum)
  (begin (##include-once (ribbit "nums/fixnum")))) 

(if-feature (or nums/bignum nums/flonum) 
  (begin (##include-once (ribbit "nums/bignum")))) 

(if-feature nums/flonum
  (begin (##include-once (ribbit "nums/flonum")))) 
                
;; should also activate scheme-bignum

(define pair-type      0)
;; (define procedure-type 1)
;; (define symbol-type    2)
(define string-type    3)
;; (define vector-type    4)
;; (define singleton-type 5)
;; (define char-type      6)
(define bignum-type    7)
(define flonum-type    8)


;;==============================================================================

;; Error handling

;; TODO

;; (define err_div0 (error "*** ERROR -- Division by 0"))
;; (define err_int  (error "*** ERROR -- Integer expected"))


;;==============================================================================

;; Dispatch

;; Numeric types; fixnums, bignums, flonums

;; Parametrics: variadics, compact, typecheck

(define-macro (num-dispatch bindings)
  (define table
    `((bignum (and nums/bignum (not nums/flonum)))
      (fixnum (and (not nums/flonum) (not nums/bignum)))
      (flonum (and nums/bignum nums/flonum nums/fixnum))))
  (map
    (lambda (binding)
      (let* ((num-type (car binding))
             (condition (cadr (assq num-type table))))
        `(if-feature ,condition
                     (begin ,@(cdr binding)))))
    bindings))


(define-macro (define-num-op signature binding)
  `(define ,signature
     (num-dispatch
      ,binding)))



;;------------------------------------------------------------------------------

;; Numerical predicates


;; Numerical types predicates

;; FIXME redefine these

(define (number? n) (complex? n))
(define (complex? n) (real? n)) ;; (or (real? n) (cpxnum? n)))
(define (real? n) (or (rational? n) (flonum? n)))
(define (rational? n) (integer? n)) ;; FIXME flonum AND finite
(define (integer? n) (or (fixnum? n) (bignum? n)))
  ;; (if-feature nums/flonum
  ;;  (or (fixnum.0? n) (or (fixnum? n) (bignum? n)))
  ;;  (or (fixnum? n) (bignum? n))))


;; Exactness predicates

(define (exact? n) (rational? n)) ;; what about complex?
(define (inexact? n) (and (real? n) (not (rational? n))))


;; Comparison predicates

(define (= a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##eqv? a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (and (fixnum? a) (fixnum? b))
          (##eqv? a b)
          (##bn= a b))
      (if-feature nums/flonum
        (cond ((and (fixnum? a) (fixnum? b))
               (##eqv? a b))
              ((or (flonum? a) (flonum? b))
               (##fl= (cond ((fixnum? a) (num->flonum a))
                            ((bignum? a) (num->flonum a))
                            (else a))
                      (cond ((fixnum? b) (num->flonum b))
                            ((bignum? b) (num->flonum b))
                            (else b))))
              (else
               (##bn= a b)))))))


(define (< a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##< a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (and (fixnum? a) (fixnum? b))
          (##< a b)
          (##bn< (if (bignum? a) a (fixnum->bignum a))
                 (if (bignum? b) b (fixnum->bignum b))))
      (if-feature nums/flonum
        (cond ((and (fixnum? a) (fixnum? b))
               (##< a b))
              ((or (flonum? a) (flonum? b))
               (##fl< (cond ((fixnum? a) (num->flonum a))
                            ((bignum? a) (num->flonum a))
                            (else a))
                      (cond ((fixnum? b) (num->flonum b))
                            ((bignum? b) (num->flonum b))
                            (else b))))
              (else
               (##bn< (if (bignum? a) a (fixnum->bignum a))
                      (if (bignum? b) b (fixnum->bignum b)))))))))


(define (> a b) (< b a))
(define (<= a b) (or (= a b) (< a b)))
(define (>= a b) (or (= a b) (< b a)))


;; Numerical properties predicates

(define (zero? a) (= a 0)) 
(define (positive? a) (< 0 a)) 
(define (negative? a) (< a 0)) 

(define (odd? a)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (not (##fx-even? a))
    (if-feature (and nums/bignum (not nums/flonum))
      (if (fixnum? a)
          (not (##fx-even? a))
          (not (##bn-even? a)))
      (if-feature nums/flonum
        (cond ((fixnum? a) (not (##fx-even? a)))
              ((flonum? a)
               (error "*** ERROR - INTEGER expected in procedure: odd?"))
               ;; (##fl-odd? a)) 
              (else (not (##bn-even? a))))))))

(define (even? a)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##fx-even? a)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (fixnum? a)
          (##fx-even? a)
          (##bn-even? a))
      (if-feature nums/flonum
        (cond ((fixnum? a) (##fx-even? a))
              ((flonum? a)
               (error "*** ERROR - INTEGER expected in procedure: even?"))
               ;; (##fl-even? a))
              (else (##bn-even? a)))))))


;;------------------------------------------------------------------------------

;; Max and min

(define (max a b) (if (< a b) b a))

(define (min a b) (if (< a b) a b))


;;------------------------------------------------------------------------------

;; Arithmetic operators: +, *, -, and /


;; Leaving this here just to show how easy it'll be to integrate the
;; optimization once it's done

;; (define (+ a b)
;;   (if-feature (and (not nums/bignum) (not nums/flonum))
;;     (##+ a b)
;;     (if-feature (and nums/bignum (not nums/flonum))
;;       (or (##+? a b)
;;        (bn-norm
;;         (##bn+ (if (bignum? a) a (fixnum->bignum a))
;;                (if (bignum? b) b (fixnum->bignum b))))))))


;; Other possible format:
;; (define (+ a b)
;;   (if-feature (and (not nums/bignum) (not nums/flonum))
;;     (##+ a b)
;;     (cond ((and (fixnum? a) (fixnum? b))
;;         (bn-norm (##+ a b)))
;;        (if-feature nums/flonum ;; <-- if-feature here instead
;;          ((or (flonum? a) (flonum? b))
;;           (##fl+ (cond ((fixnum? a) (num->flonum a))
;;                        ((bignum? a) (num->flonum a))
;;                        (else a))
;;                  (cond ((fixnum? b) (num->flonum b))
;;                        ((bignum? b) (num->flonum b))
;;                        (else b)))))
;;        (else
;;         (bn-norm (##bn+ (if (bignum? a) a (fixnum->bignum a))
;;                         (if (bignum? b) b (fixnum->bignum b))))))))))


(define (+ a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##+ a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (and (fixnum? a) (fixnum? b))
          (bn-norm (##+ a b))
          (bn-norm (##bn+ (if (bignum? a) a (fixnum->bignum a))
                          (if (bignum? b) b (fixnum->bignum b)))))
      (if-feature nums/flonum
        (cond ((and (fixnum? a) (fixnum? b))
               (bn-norm (##+ a b)))
              ((or (flonum? a) (flonum? b))
               (##fl+ (cond ((fixnum? a) (num->flonum a))
                            ((bignum? a) (num->flonum a))
                            (else a))
                      (cond ((fixnum? b) (num->flonum b))
                            ((bignum? b) (num->flonum b))
                            (else b))))
              (else
               (bn-norm (##bn+ (if (bignum? a) a (fixnum->bignum a))
                               (if (bignum? b) b (fixnum->bignum b))))))))))


(define (* a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##* a b)
    (if-feature (and nums/bignum (not nums/flonum))           
      (cond ((and (fixnum? a) (fixnum? b))
             (bn-norm (##* a b)))
            ((fixnum? b)
             (bn-norm (##bn-fx* a b)))
            ((fixnum? a)
             (bn-norm (##bn-fx* b a)))
            (else
             (bn-norm (##bn* a b))))
      (if-feature nums/flonum
        (cond ((and (fixnum? a) (fixnum? b))
               (bn-norm (##* a b)))
              ((or (flonum? a) (flonum? b))
               (##fl* (cond ((fixnum? a) (num->flonum a))
                            ((bignum? a) (num->flonum a))
                            (else a))
                      (cond ((fixnum? b) (num->flonum b))
                            ((bignum? b) (num->flonum b))
                            (else b))))
              ((fixnum? b)
               (bn-norm (##bn-fx* a b)))
              ((fixnum? a)
               (bn-norm (##bn-fx* b a)))
              (else
               (bn-norm (##bn* a b))))))))


(define (- a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##- a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (and (fixnum? a) (fixnum? b))
          (bn-norm (##- a b)) ;; could overflow: (- 0 -MAX_FIXNUM)
          (bn-norm (##bn- (if (bignum? a) a (fixnum->bignum a))
                          (if (bignum? b) b (fixnum->bignum b)))))
      (if-feature nums/flonum
        (cond ((and (fixnum? a) (fixnum? b))
               (bn-norm (##- a b)))
              ((or (flonum? a) (flonum? b))
               (##fl- (cond ((fixnum? a) (num->flonum a))
                            ((bignum? a) (num->flonum a))
                            (else a))
                      (cond ((fixnum? b) (num->flonum b))
                            ((bignum? b) (num->flonum b))
                            (else b))))
              (else
               (bn-norm (##bn- (if (bignum? a) a (fixnum->bignum a))
                               (if (bignum? b) b (fixnum->bignum b))))))))))


(define (/ a b)
  (if-feature (or (and (not nums/bignum) (not nums/flonum))
                  (and nums/bignum (not nums/flonum)))
    (quotient a b)
    (if-feature nums/flonum
     (if (or (flonum? a) (flonum? b))
         (##fl/ (cond ((fixnum? a) (num->flonum a))
                      ((bignum? a) (num->flonum a))
                      (else a))
                (cond ((fixnum? b) (num->flonum b))
                      ((bignum? b) (num->flonum b))
                      (else b)))
         (quotient a b)))))

    
;;------------------------------------------------------------------------------

;; Absolute value

(define (abs a) (if (< a 0) (- 0 a) a))


;;------------------------------------------------------------------------------

;; Quotient, remainder, and modulo

(define (quotient a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##quotient a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (cond ((and (fixnum? a) (fixnum? b))
             (##quotient a b)) ;; no need to normalize
            ((fixnum? b)
             (bn-norm (##bn-fx-quotient a b)))
            (else
             (bn-norm
              (##bn-quotient (if (bignum? a) a (fixnum->bignum a)) b))))
      (if-feature nums/flonum
        (cond ((and (fixnum? a) (fixnum? b))
               (##quotient a b)) ;; no need to normalize
              ((or (flonum? a) (flonum? b))
               (error "*** ERROR - INTEGER expected in procedure: quotient"))
               ;; (##fl-quotient (cond ((fixnum? a) (num->flonum a))
               ;;                           ((bignum? a) (num->flonum a))
               ;;                           (else a))
               ;;                     (cond ((fixnum? b) (num->flonum b))
               ;;                           ((bignum? b) (num->flonum b))
               ;;                           (else b))))
              ((fixnum? b)
               (bn-norm (##bn-fx-quotient a b)))
              (else
               (bn-norm
                (##bn-quotient (if (bignum? a) a (fixnum->bignum a)) b))))))))

        
(define (remainder a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##fx-remainder a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (cond ((and (fixnum? a) (fixnum? b))
             (##fx-remainder a b)) ;; no need to normalize
            ((fixnum? b)
             (bn-norm (##bn-fx-remainder a b)))
            (else
             (bn-norm
              (##bn-remainder (if (bignum? a) a (fixnum->bignum a)) b))))
      (if-feature nums/flonum
        (cond ((and (fixnum? a) (fixnum? b))
               (##fx-remainder a b)) ;; no need to normalize
              ((or (flonum? a) (flonum? b))
               (error "*** ERROR - INTEGER expected in procedure: remainder"))
               ;; (##fl-remainder (cond ((fixnum? a) (num->flonum a))
               ;;                            ((bignum? a) (num->flonum a))
               ;;                            (else a))
               ;;                      (cond ((fixnum? b) (num->flonum b))
               ;;                            ((bignum? b) (num->flonum b))
               ;;                            (else b))))
              ((fixnum? b)
               (bn-norm (##bn-fx-remainder a b)))
              (else
               (bn-norm
                (##bn-remainder (if (bignum? a) a (fixnum->bignum a)) b))))))))


(define (modulo a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##fx- modulo a b) 
    (if-feature (and nums/bignum (not nums/flonum))
      (if (and (fixnum? a) (fixnum? b))
          (##fx-modulo a b) ;; no need to normalize
          (bn-norm
           (##bn-modulo (if (bignum? a) a (fixnum->bignum a))
                        (if (bignum? b) b (fixnum->bignum b)))))
      (if-feature nums/flonum
        (cond ((and (fixnum? a) (fixnum? b))
               (##fx-modulo a b)) ;; no need to normalize
              ((or (flonum? a) (flonum? b))
               (error "*** ERROR - INTEGER expected in procedure: modulo"))
               ;; (##fl-remainder (cond ((fixnum? a) (num->flonum a))
               ;;                            ((bignum? a) (num->flonum a))
               ;;                            (else a))
               ;;                      (cond ((fixnum? b) (num->flonum b))
               ;;                            ((bignum? b) (num->flonum b))
               ;;                            (else b))))
              (else
               (bn-norm
                (##bn-modulo (if (bignum? a) a (fixnum->bignum a))
                             (if (bignum? b) b (fixnum->bignum b))))))))))


;;------------------------------------------------------------------------------

;; Gcd and lcm

(define (gcd x y)
  (let ((ax (abs x)))
    (let ((ay (abs y)))
      (if (< ax ay)
          (gcd-aux ax ay)
          (gcd-aux ay ax)))))

(define (gcd-aux x y)
  (if (eqv? x 0)
      y
      (gcd-aux (remainder y x) x)))

(define (lcm x y)
  (if (eqv? y 0)
      0
      (let ((ax (abs x)))
        (let ((ay (abs y)))
          (* (quotient ax (gcd ax ay)) ay)))))


;;------------------------------------------------------------------------------

;; Floor, ceiling, truncate, and round


(define (floor a)
  (if-feature (or (and (not nums/bignum) (not nums/flonum))
                  (and nums/bignum (not nums/flonum)))
    (##id a)
    (if-feature nums/flonum
      (if (flonum? a)
          (##fl-floor a)
          (##id a)))))
          
(define (ceiling a)
  (if-feature (or (and (not nums/bignum) (not nums/flonum))
                  (and nums/bignum (not nums/flonum)))
    (##id a)
    (if-feature nums/flonum
      (if (flonum? a)
          (##fl-ceiling a)
          (##id a)))))

(define (truncate a)
  (if-feature (or (and (not nums/bignum) (not nums/flonum))
                  (and nums/bignum (not nums/flonum)))
    (##id a)
    (if-feature nums/flonum
      (if (flonum? a)
          (##fl-truncate a)
          (##id a)))))

(define (round a)
  (if-feature (or (and (not nums/bignum) (not nums/flonum))
                  (and nums/bignum (not nums/flonum)))
    (##id a)
    (if-feature nums/flonum
      (if (flonum? a)
          (##fl-round a)
          (##id a)))))


;;==============================================================================

;; Numerical input and output

(define (string->number str)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##fx-string->number str)
    (if-feature (and nums/bignum (not nums/flonum))
      (##bn-string->number str)
      (if-feature nums/flonum 
        (##fl-string->number str)))))

(define (number->string a)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##fx-number->string a)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (fixnum? a)
          (##fx-number->string a)
          (##bn-number->string a))
      (if-feature nums/flonum
        (cond ((fixnum? a)
               (##fx-number->string a))
              ((bignum? a)
               (##bn-number->string a))
              ((flonum? a)
               (##fl-number->string a)))))))

;; used for tests

(define (test-display n)

  (define (display-str str)
    (let loop ((lst (##field0 str)))
      (if (##eqv? lst '())
          (##putchar 10)
          (begin
            (##putchar (##field0 lst))
            (loop (##field1 lst))))))
  
  (cond ((or (fixnum? n) (bignum? n))
         (display-str (number->string n)))
        ((##eqv? n #f)
         (##putchar 35) (##putchar 102) (##putchar 10))
        ((##eqv? n #t)
         (##putchar 35) (##putchar 116) (##putchar 10))
        ((##eqv? string-type (##field2 n))
         (display-str n))))
         
        



;;==============================================================================

;; Unit tests

;; Same tests as tests/nums

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


(define (test res expec)
  (if (not (equal? res expec))
      (begin
        (display "test failed...\n")
        (display "expected: ") (display expec) (newline)
        (display "result: ") (display res) (newline))
      ;;(display "test passed...\n")))
      ))


;;------------------------------------------------------------------------------

;; Fixnums tests

;; (display "addition: \n")

;; (display (##+? 32768 32768)) ;; no overflow
;; (newline)

;; (display (##+? 2147483647 0)) ;; no overflow: 2^31-1 + 0 = 2^31-1
;; (newline)

;; (display (##+? 2147483647 1)) ;; overflow: : 2^31-1 + 1 = 2^31
;; (newline)

;; (display (##+? 2147483647 2147483647)) ;; overflow: 2^31-1 + 2^31-1 = 2^32-2
;; (newline)

;; ;; 2147483648 + anything not possible since it would be converted to a bignum before

;; ;; (display (##+? 2147483648 0)) ;; overflow: -2^31 + 0 (because of tagging)
;; ;; (newline)

;; (display (##+? -32768 -32768)) ;; no overflow
;; (newline)

;; (display (##+? -2147483648 0)) ;; no overflow: -2^31 + 0
;; (newline)

;; ;; overflow if x and y have the same parity but x+y doesn't

;; (display (##+? -2147483648 -1)) ;; overflow: -2^31+1 + -1 (because of tagging)
;; (newline)

;; (display (##+? -1073741824 -1073741824)) ;; overflow: (-2^31+1) + (-2^31+1)
;; (newline)

;; (newline)


;; (display "multiplication: \n")

;; (display (##*? 32768 32768)) ;; no overflow: 2^3
;; (newline)

;; (display (##*? 2147483647 -1)) ;; -2^31 
;; (newline)

;; (display (##*? 32768 65536)) ;; overflow: 2147483648 = 2^31 + 1 (for Ribbit, not C)
;; (newline)

;; (display (##*? 65536 65536)) ;; overflow in C
;; (newline)

;; (display (##*? 65536 131072)) ;; overflow in C
;; (newline)

;; (newline)


;; (display "substraction: \n")

;; (display (- 2147483647 0)) ;; no overflow: 2^31 + (-0) = 2^31
;; (newline)

;; (display (- 2147483647 -1)) ;; overflow: : 2^31 + (-1)
;; (newline)

;; (display (- -2147483647 0)) ;; overflow: -2^31 + (-0) (because of tagging)
;; (newline)

;; (newline)


;; ;;------------------------------------------------------------------------------

;; ;; Flonums tests (single-precision)


;; ;; FIXME patches to get stuff running for now until the reader and writer are
;; ;; modified

;; (define $$ string->number)

;; (define _fl-min-subnormal ($$ "1.401298464324817e-45"))
;; (define _fl-min-subnormal-neg ($$ "-1.401298464324817e-45"))
;; (define _fl-max-subnormal ($$ "1.1754942106924411e-38"))
;; (define _fl-max-subnormal-neg ($$ "-1.1754942106924411e-38"))
;; (define _fl-min-normal ($$ "1.1754943508222875e-38")) ;; ieee32
;; (define _fl-min-normal-neg ($$ "-1.1754943508222875e-38")) ;; ieee32
;; (define _fl-max-normal ($$ "3.4028234663852886e+38")) ;; ieee32
;; (define _fl-max-normal-neg ($$ "-3.4028234663852886e+38")) ;; ieee32
;; (define _max-normal 340282346638528859811704183484516925440) ;; ieee32
;; (define _max-normal-neg -340282346638528859811704183484516925440) ;; ieee32

;; (define _test-num-pos 680564693277057719623408366969033850880) ;; ieee32
;; (define _test-num-neg -680564693277057719623408366969033850880) ;; ieee32

;; (define (display-fl n)
;;   (display (##fl-number->string n)))
      
;; (define (test-fl res expec)
;;   (if (and (not (equal? res expec))
;;            (and (not (nan? res)) (not (nan? expec))))
;;       (begin
;;         (display "test failed...\n")
;;         (display "expected: ") (display-fl expec) (newline)
;;         (display "result: ") (display-fl res) (newline))
;;       ;;(display "test passed...\n")))
;;       ))


;; ;; ;; number?
;; ;; (display "flonum's number? : \n")
;; ;; (test (number? ($$ "1.0")) #t)
;; ;; (test (number? ($$ "-0.1")) #t)
;; ;; (test (number? ($$ "+0.0")) #t)
;; ;; (test (number? ($$ "-0.0")) #t)
;; ;; (test (number? ($$ "+nan.0")) #t)
;; ;; (test (number? ($$ "+inf.0")) #t)
;; ;; (test (number? ($$ "-inf.0")) #t)
;; ;; (test (number? ($$ "1.0e+10")) #t)
;; ;; (test (number? ($$ "-1.0e-10")) #t)
;; ;; (newline)

;; ;; ;; complex?
;; ;; (display "flonum's complex? : \n")
;; ;; (test (complex? ($$ "1.0")) #t)
;; ;; (test (complex? ($$ "-0.1")) #t)
;; ;; (test (complex? ($$ "+0.0")) #t)
;; ;; (test (complex? ($$ "-0.0")) #t)
;; ;; (test (complex? ($$ "+nan.0")) #t)
;; ;; (test (complex? ($$ "+inf.0")) #t)
;; ;; (test (complex? ($$ "-inf.0")) #t)
;; ;; (test (complex? ($$ "1.0e+10")) #t)
;; ;; (test (complex? ($$ "-1.0e-10")) #t)
;; ;; (newline)

;; ;; ;; real?
;; ;; (display "flonum's real? : \n")
;; ;; (test (real? ($$ "1.0")) #t)
;; ;; (test (real? ($$ "-0.1")) #t)
;; ;; (test (real? ($$ "+0.0")) #t)
;; ;; (test (real? ($$ "-0.0")) #t)
;; ;; (test (real? ($$ "+nan.0")) #t)
;; ;; (test (real? ($$ "+inf.0")) #t)
;; ;; (test (real? ($$ "-inf.0")) #t)
;; ;; (test (real? ($$ "1.0e+10")) #t)
;; ;; (test (real? ($$ "-1.0e-10")) #t)
;; ;; (newline)

;; ;; ;; rational?
;; ;; (display "flonum's rational? : \n")
;; ;; (test (rational? ($$ "1.0")) #f)
;; ;; (test (rational? ($$ "-0.1")) #f)
;; ;; (test (rational? ($$ "+0.0")) #f)
;; ;; (test (rational? ($$ "-0.0")) #f)
;; ;; (test (rational? ($$ "+nan.0")) #f)
;; ;; (test (rational? ($$ "+inf.0")) #f)
;; ;; (test (rational? ($$ "-inf.0")) #f)
;; ;; (test (rational? ($$ "1.0e+10")) #f)
;; ;; (test (rational? ($$ "-1.0e-10")) #f)
;; ;; (newline)

;; ;; ;; integer?
;; ;; (display "flonum's integer? : \n")
;; ;; (test (integer? ($$ "1.0")) #f)
;; ;; (test (integer? ($$ "-0.1")) #f)
;; ;; (test (integer? ($$ "+0.0")) #f)
;; ;; (test (integer? ($$ "-0.0")) #f)
;; ;; (test (integer? ($$ "+nan.0")) #f)
;; ;; (test (integer? ($$ "+inf.0")) #f)
;; ;; (test (integer? ($$ "-inf.0")) #f)
;; ;; (test (integer? ($$ "1.0e+10")) #f)
;; ;; (test (integer? ($$ "-1.0e-10")) #f)
;; ;; (newline)

;; ;; ;; exact?
;; ;; (display "flonum's exact? : \n")
;; ;; (test (exact? ($$ "1.0")) #f)
;; ;; (test (exact? ($$ "-0.1")) #f)
;; ;; (test (exact? ($$ "+0.0")) #f)
;; ;; (test (exact? ($$ "-0.0")) #f)
;; ;; (test (exact? ($$ "+nan.0")) #f)
;; ;; (test (exact? ($$ "+inf.0")) #f)
;; ;; (test (exact? ($$ "-inf.0")) #f)
;; ;; (test (exact? ($$ "1.0e+10")) #f)
;; ;; (test (exact? ($$ "-1.0e-10")) #f)
;; ;; (newline)

;; ;; ;; inexact?
;; ;; (display "flonum's inexact? : \n")
;; ;; (test (inexact? ($$ "1.0")) #t)
;; ;; (test (inexact? ($$ "-0.1")) #t)
;; ;; (test (inexact? ($$ "+0.0")) #t)
;; ;; (test (inexact? ($$ "-0.0")) #t)
;; ;; (test (inexact? ($$ "+nan.0")) #t)
;; ;; (test (inexact? ($$ "+inf.0")) #t)
;; ;; (test (inexact? ($$ "-inf.0")) #t)
;; ;; (test (inexact? ($$ "1.0e+10")) #t)
;; ;; (test (inexact? ($$ "-1.0e-10")) #t)
;; ;; (newline)

;; ;; ;; zero?
;; ;; (display "flonum's zero? : \n")
;; ;; (test (zero? ($$ "1.0")) #f)
;; ;; (test (zero? ($$ "-0.1")) #f)
;; ;; (test (zero? ($$ "0.0")) #t)
;; ;; (test (zero? ($$ "+0.0")) #t) 
;; ;; (test (zero? ($$ "-0.0")) #t)
;; ;; (test (zero? ($$ "+nan.0")) #f)
;; ;; (test (zero? ($$ "+inf.0")) #f)
;; ;; (test (zero? ($$ "-inf.0")) #f)
;; ;; (test (zero? ($$ "1.0e+10")) #f)
;; ;; (test (zero? ($$ "-1.0e+10")) #f)
;; ;; (test (zero? ($$ "1.0e-350")) #t)
;; ;; (test (zero? ($$ "-1.0e-350")) #t)
;; ;; (newline)

;; ;; ;; positive?
;; ;; (display "flonum's positive? : \n")
;; ;; (test (positive? ($$ "1.0")) #t)
;; ;; (test (positive? ($$ "-0.1")) #f)
;; ;; (test (positive? ($$ "0.0")) #f)
;; ;; (test (positive? ($$ "+0.0")) #f) 
;; ;; (test (positive? ($$ "-0.0")) #f)
;; ;; (test (positive? ($$ "+nan.0")) #f)
;; ;; (test (positive? ($$ "+inf.0")) #t)
;; ;; (test (positive? ($$ "-inf.0")) #f)
;; ;; (test (positive? ($$ "1.0e+10")) #t)
;; ;; (test (positive? ($$ "-1.0e+10")) #f)
;; ;; (newline)

;; ;; ;; negative?
;; ;; (display "flonum's negative? : \n")
;; ;; (test (negative? ($$ "1.0")) #f)
;; ;; (test (negative? ($$ "-0.1")) #t)
;; ;; (test (negative? ($$ "0.0")) #f)
;; ;; (test (negative? ($$ "+0.0")) #f)
;; ;; (test (negative? ($$ "-0.0")) #f)
;; ;; (test (negative? ($$ "+nan.0")) #f)
;; ;; (test (negative? ($$ "+inf.0")) #f)
;; ;; (test (negative? ($$ "-inf.0")) #t)
;; ;; (test (negative? ($$ "1.0e+10")) #f)
;; ;; (test (negative? ($$ "-1.0e+10")) #t)
;; ;; (newline)

;; ;; ;; ;; odd?
;; ;; ;; (display "flonum's odd? : \n")
;; ;; ;; (test (odd? ($$ "1.0")) #t)
;; ;; ;; (test (odd? ($$ "1.5")) 'error) ;; FIXME
;; ;; ;; (test (odd? ($$ "-2.0")) #f)
;; ;; ;; (test (odd? ($$ "-2.5")) 'error) ;; FIXME
;; ;; ;; (test (odd? ($$ "+0.0")) #t)
;; ;; ;; (test (odd? ($$ "-0.0")) #t)
;; ;; ;; (test (odd? ($$ "+nan.0")) #t)
;; ;; ;; (test (odd? ($$ "+inf.0")) #t)
;; ;; ;; (test (odd? ($$ "-inf.0")) #t)
;; ;; ;; (test (odd? ($$ "1.0000000001e+10")) #t)
;; ;; ;; (test (odd? ($$ "-1.0e+10")) #f)
;; ;; ;; (newline)

;; ;; ;; ;; even?
;; ;; ;; (display "flonum's even? : \n")
;; ;; ;; (test (even? ($$ "1.0")) #t)
;; ;; ;; (test (even? ($$ "1.5")) 'error)
;; ;; ;; (test (even? ($$ "-2.0")) #f)
;; ;; ;; (test (even? ($$ "-2.5")) 'error)
;; ;; ;; (test (even? ($$ "+0.0")) #t)
;; ;; ;; (test (even? ($$ "-0.0")) #t)
;; ;; ;; (test (even? ($$ "+nan.0")) #t)
;; ;; ;; (test (even? ($$ "+inf.0")) #t)
;; ;; ;; (test (even? ($$ "-inf.0")) #t)
;; ;; ;; (test (even? ($$ "1.0000000001e+10")) #f)
;; ;; ;; (test (even? ($$ "-1.0e+10")) #t)
;; ;; ;; (test (even? ($$ "1.0e+350")) #f)
;; ;; ;; (test (even? ($$ "-1.0e+350")) #f)
;; ;; ;; (newline)

;; ;; ;; = TODO variadics
;; ;; (display "flonum's = : \n")
;; ;; (test (= ($$ "1.0") ($$ "1.0")) #t)
;; ;; (test (= ($$ "0.1") ($$ "-0.1")) #f)
;; ;; (test (= ($$ "+0.0") ($$ "-0.0")) #t) 
;; ;; (test (= ($$ "0.0") ($$ "1.0e-350")) #t)
;; ;; (test (= ($$ "0.0") _fl-min-subnormal) #f)
;; ;; (test (= ($$ "0.0") _fl-min-subnormal-neg) #f)
;; ;; (test (= ($$ "+nan.0") ($$ "+nan.0")) #f)
;; ;; (test (= ($$ "+nan.0") ($$ "1.0")) #f) 
;; ;; (test (= ($$ "+inf.0") ($$ "+inf.0")) #t) 
;; ;; (test (= ($$ "+inf.0") ($$ "1.0e+350")) #t) ;; conversion during parsing should work
;; ;; (test (= ($$ "-inf.0") ($$ "-inf.0")) #t) 
;; ;; (test (= ($$ "-inf.0") ($$ "-1.0e+350")) #t) ;; conversion during parsing should work
;; ;; (test (= ($$ "+inf.0") ($$ "-inf.0")) #f)
;; ;; (test (= ($$ "1e+10") ($$ "10000000000.0")) #t)
;; ;; (test (= ($$ "1e+10") ($$ "1.0e+10")) #t) ;; equivalent notation
;; ;; (test (= ($$ "1e+10") ($$ "1e10")) #t) ;; equivalent notation
;; ;; (test (= ($$ "102334150.0") ($$ "102334152.0")) #t)   ;; same rep ieee32
;; ;; (test (= ($$ "-102334152.0") ($$ "-102334155.0")) #t) ;; same rep ieee32
;; ;; (test (= ($$ "1.0") 1) #t)
;; ;; (test (= ($$ "-1.0") -1) #t)
;; ;; (test (= _fl-max-normal _max-normal) #t)
;; ;; (test (= _fl-max-normal-neg _max-normal-neg) #t)
;; ;; (test (= ($$ "+inf.0") _test-num-pos) #t) ;; proper conversion of fx/bn to +inf.0
;; ;; (test (= ($$ "+inf.0") _test-num-neg) #f) ;; proper conversion of fx/bn to inf
;; ;; (test (= ($$ "-inf.0") _test-num-neg) #t) ;; proper conversion of fx/bn to -inf.0
;; ;; (test (= ($$ "-inf.0") _test-num-pos) #f) ;; proper conversion of fx/bn to inf
;; ;; ;; (test (= 0 ($$ "0.0") ($$ "+0.0") ($$ "-0.0") ($$ "1.0e-350") ($$ "-1.0e-350")))
;; ;; ;; (test (= ($$ "102334150.0") ($$ "102334152.0") ($$ "102334155.0")) #t)
;; ;; (newline)

;; ;; ;; < TODO variadics
;; ;; (display "flonum's < : \n")
;; ;; (test (< ($$ "1.0") ($$ "2.0")) #t)
;; ;; (test (< ($$ "-1.0") ($$ "-2.0")) #f)
;; ;; (test (< ($$ "-0.0") ($$ "+0.0")) #f) 
;; ;; (test (< ($$ "-0.0") ($$ "1.0e-350")) #f)
;; ;; (test (< ($$ "0.0") _fl-min-subnormal) #t)
;; ;; (test (< ($$ "0.0") _fl-min-subnormal-neg) #f)
;; ;; (test (< ($$ "+nan.0") ($$ "+nan.0")) #f)
;; ;; (test (< ($$ "+nan.0") ($$ "1.0")) #f)
;; ;; (test (< ($$ "+nan.0") ($$ "+inf.0")) #f)
;; (test (< ($$ "+inf.0") ($$ "+inf.0")) #f)
;; ;; (test (< ($$ "+inf.0") ($$ "1.0e+350")) #f)
;; ;; (test (< ($$ "-inf.0") ($$ "-inf.0")) #f)
;; ;; (test (< ($$ "-inf.0") ($$ "-1.0e+350")) #f) 
;; ;; (test (< ($$ "-inf.0") ($$ "+inf.0")) #t)
;; ;; (test (< ($$ "-inf.0") ($$ "1.0e+350")) #t)
;; ;; (test (< ($$ "+inf.0") ($$ "-inf.0")) #f)
;; ;; (test (< ($$ "+inf.0") ($$ "-1.0e+350")) #f)
;; ;; (test (< ($$ "102334150.0") ($$ "102334152.0")) #f) 
;; ;; (test (< ($$ "-102334155.0") ($$ "-102334152.0")) #f) 
;; ;; (test (< ($$ "0.99999994") ($$ "1.0")) #t)
;; ;; (test (< ($$ "1.0") ($$ "1.0000001")) #t) 
;; ;; (test (< ($$ "1.0000001") ($$ "1.0000002")) #t) 
;; ;; (test (< ($$ "-0.99999994") ($$ "-1.0")) #f)
;; ;; (test (< ($$ "-1.0") ($$ "-1.0000001")) #f) 
;; ;; (test (< ($$ "-1.0000001") ($$ "-1.0000002")) #f) 
;; ;; (test (< ($$ "1.0") 2) #t)
;; ;; (test (< ($$ "-1.0") -2) #f)
;; ;; (test (< ($$ "-inf.0") 0) #t)
;; ;; (test (< ($$ "+inf.0") 0) #f)
;; ;; (test (< _fl-max-normal _test-num-pos) #t) 
;; ;; (test (< ($$ "+inf.0") _test-num-pos) #f) 
;; ;; (test (< _fl-max-normal-neg _test-num-neg) #f) 
;; ;; (test (< ($$ "-inf.0") _test-num-neg) #f) 
;; ;; ;; ;; (test (< ($$ "0.99999994") ($$ "1.0") ($$ "1.0000001") ($$ "1.0000002")))
;; ;; ;; ;; (test (< ($$ "-0.99999994") ($$ "-1.0") ($$ "-1.0000001") ($$ "-1.0000002")))
;; ;; (newline)

;; ;; ;; > TODO variadics
;; ;; (display "flonum's > : \n")
;; ;; (test (> ($$ "1.0") ($$ "2.0")) #f)
;; ;; (test (> ($$ "-1.0") ($$ "-2.0")) #t)
;; ;; (test (> ($$ "-0.0") ($$ "+0.0")) #f) 
;; ;; (test (> ($$ "+0.0") ($$ "1.0e-350")) #f) 
;; ;; (test (> ($$ "0.0") _fl-min-subnormal) #f)
;; ;; (test (> ($$ "0.0") _fl-min-subnormal-neg) #t)
;; ;; (test (> ($$ "+nan.0") ($$ "+nan.0")) #f) 
;; ;; (test (> ($$ "+nan.0") ($$ "1.0")) #f)
;; ;; (test (> ($$ "+nan.0") ($$ "-inf.0")) #f)
;; ;; (test (> ($$ "+inf.0") ($$ "+inf.0")) #f) 
;; ;; (test (> ($$ "+inf.0") ($$ "1.0e+350")) #f) 
;; ;; (test (> ($$ "-inf.0") ($$ "-inf.0")) #f) 
;; ;; (test (> ($$ "-inf.0") ($$ "-1.0e+350")) #f) 
;; ;; (test (> ($$ "-inf.0") ($$ "+inf.0")) #f)
;; ;; (test (> ($$ "-inf.0") ($$ "1.0e+350")) #f)
;; ;; (test (> ($$ "+inf.0") ($$ "-inf.0")) #t) 
;; ;; (test (> ($$ "+inf.0") ($$ "-1.0e+350")) #t)
;; ;; (test (> ($$ "102334150.0") ($$ "102334152.0")) #f) 
;; ;; (test (> ($$ "-102334155.0") ($$ "-102334152.0")) #f) 
;; ;; (test (> ($$ "0.99999994") ($$ "1.0")) #f)
;; ;; (test (> ($$ "1.0") ($$ "1.0000001")) #f) 
;; ;; (test (> ($$ "1.0000001") ($$ "1.0000002")) #f) 
;; ;; (test (> ($$ "-0.99999994") ($$ "-1.0")) #t)
;; ;; (test (> ($$ "-1.0") ($$ "-1.0000001")) #t) 
;; ;; (test (> ($$ "-1.0000001") ($$ "-1.0000002")) #t) 
;; ;; (test (> ($$ "1.0") 2) #f)
;; ;; (test (> ($$ "-1.0") -2) #t)
;; ;; (test (> ($$ "-inf.0") 0) #f)
;; ;; (test (> ($$ "+inf.0") 0) #t)
;; ;; (test (> _fl-max-normal _test-num-pos) #f) 
;; ;; (test (> ($$ "+inf.0") _test-num-pos) #f) 
;; ;; (test (> _fl-max-normal-neg _test-num-neg) #t)
;; ;; (test (> ($$ "-inf.0") _test-num-neg) #f) 
;; ;; ;; ;; (test (> ($$ "0.99999994") ($$ "1.0") ($$ "1.0000001") ($$ "1.0000002")))
;; ;; ;; ;; (test (> ($$ "-0.99999994") ($$ "-1.0") ($$ "-1.0000001") ($$ "-1.0000002")))
;; ;; (newline)

;; ;; <=

;; ;; >=

;; ;; max
;; (display "flonum's max: \n")
;; (test-fl (max ($$ "1.0") ($$ "1.1")) ($$ "1.1"))
;; (test-fl (max ($$ "0.1") ($$ "-0.1")) ($$ "0.1"))
;; (test-fl (max ($$ "-0.1") ($$ "-1.0")) ($$ "-0.1"))
;; ;; special symbols
;; (test-fl (max ($$ "+0.0") ($$ "-0.0")) ($$ "0.0"))
;; (test-fl (max ($$ "0.0") ($$ "+nan.0")) ($$ "+nan.0"))
;; (test-fl (max ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
;; (test-fl (max ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0"))
;; (test-fl (max ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
;; (test-fl (max ($$ "+inf.0") ($$ "-inf.0")) ($$ "+inf.0"))
;; ;; fixnums
;; (display "worked\n")
;; (test-fl (max ($$ "0.9") 1) ($$ "1.0"))
;; (display "worked too\n")
;; (test-fl (max ($$ "-1.1e5") -100000) ($$ "-100000.0"))
;; ;; bignums
;; (test-fl (max ($$ "1.0e+35") 1000000000000000000000000000000000000) ($$ "1.0e+36"))
;; (test-fl (max ($$ "-1.0e+35") -1000000000000000000000000000000000000) ($$ "-1.0e+35"))
;; ;; variadics
;; (newline)

;; ;; min
;; (display "flonum's min: \n")
;; (test-fl (min ($$ "1.0") ($$ "1.1")) ($$ "1.0"))
;; (test-fl (min ($$ "0.1") ($$ "-0.1")) ($$ "-0.1"))
;; (test-fl (min ($$ "-0.1") ($$ "-1.0")) ($$ "-1.0"))
;; ;; special symbols
;; (test-fl (min ($$ "+0.0") ($$ "-0.0")) ($$ "-0.0"))
;; (test-fl (min ($$ "0.0") ($$ "+nan.0")) ($$ "+nan.0"))
;; (test-fl (min ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
;; (test-fl (min ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0"))
;; (test-fl (min ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
;; (test-fl (min ($$ "+inf.0") ($$ "-inf.0")) ($$ "-inf.0"))
;; ;; fixnums
;; (test-fl (min ($$ "0.9") 1) ($$ "0.9"))
;; (test-fl (min ($$ "-1.1e5") -100000) ($$ "-110000.0"))
;; ;; bignums
;; (test-fl (min ($$ "1.0e+35") 1000000000000000000000000000000000000) ($$ "1.0e+35"))
;; (test-fl (min ($$ "-1.0e+35") -1000000000000000000000000000000000000) ($$ "-1.0e+36"))
;; ;; variadics
;; (newline)


;; ;; addition
;; (display "flonum's addition: \n")
;; (test-fl (+ ($$ "1.0") ($$ "1.0")) ($$ "2.0"))
;; (test-fl (+ ($$ "-0.1") ($$ "-0.1")) ($$ "-0.2"))
;; (test-fl (+ ($$ "10.0e+4") ($$ "100000.0")) ($$ "2.0e+5"))
;; ;; special symbols
;; (test-fl (+ ($$ "+0.0") ($$ "-0.0")) ($$ "0.0"))
;; (test-fl (+ ($$ "-0.0") ($$ "-0.0")) ($$ "-0.0"))
;; (test-fl (+ ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
;; (test-fl (+ ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0"))
;; (test-fl (+ ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
;; (test-fl (+ ($$ "+inf.0") ($$ "-inf.0")) ($$ "+nan.0")) 
;; (test-fl (+ ($$ "-inf.0") ($$ "+inf.0")) ($$ "+nan.0")) 
;; (test-fl (+ ($$ "+inf.0") ($$ "+inf.0")) ($$ "+inf.0"))
;; (test-fl (+ ($$ "+inf.0") ($$ "-1.0")) ($$ "+inf.0"))
;; (test-fl (+ ($$ "-inf.0") ($$ "-inf.0")) ($$ "-inf.0"))
;; (test-fl (+ ($$ "-inf.0") ($$ "1.0")) ($$ "-inf.0"))
;; ;; extremal values
;; (test-fl (+ _fl-min-subnormal _fl-min-subnormal-neg) ($$ "0.0"))
;; (test-fl (+ _fl-max-normal _fl-max-normal) ($$ "+inf.0"))
;; (test-fl (+ _fl-max-normal-neg _fl-max-normal-neg) ($$ "-inf.0"))
;; ;; fixnums
;; (test-fl (+ ($$ "1.0") 1) ($$ "2.0"))
;; (test-fl (+ ($$ "-1.0") -1) ($$ "-2.0"))
;; (test-fl (+ ($$ "+nan.0") 1) ($$ "+nan.0"))
;; (test-fl (+ ($$ "+inf.0") -1) ($$ "+inf.0"))
;; (test-fl (+ ($$ "-inf.0") 1) ($$ "-inf.0"))
;; (test-fl (+ _fl-min-subnormal 1) ($$ "1.0"))
;; (test-fl (+ _fl-max-subnormal -1) ($$ "-1.0"))
;; (test-fl (+ _fl-min-subnormal-neg -1) ($$ "-1.0"))
;; (test-fl (+ _fl-max-subnormal-neg 1) ($$ "1.0"))
;; (test-fl (+ _fl-min-normal -1) ($$ "-1.0"))
;; (test-fl (+ _fl-min-normal-neg 1) ($$ "1.0"))
;; (test-fl (+ _fl-max-normal -1) _fl-max-normal)
;; (test-fl (+ _fl-max-normal-neg 1) _fl-max-normal-neg)
;; ;; ...
;; ;; bignums
;; (test-fl (+ ($$ "1.0e+35") 100000000000000000000000000000000000) ($$ "2.0e+35"))
;; (test-fl (+ ($$ "-1.0e+35") -100000000000000000000000000000000000) ($$ "-2.0e+35"))
;; (test-fl (+ ($$ "6.78e+30") 123450000000000000000000000000000000) ($$ "1.2345678e+35"))
;; (test-fl (+ _fl-max-normal _max-normal) ($$ "+inf.0"))
;; (test-fl (+ _fl-max-normal-neg _max-normal-neg) ($$ "-inf.0"))
;; (test-fl (+ ($$ "+nan.0") _test-num-pos) ($$ "+nan.0"))
;; (test-fl (+ ($$ "+inf.0") _test-num-neg) ($$ "+nan.0")) 
;; (test-fl (+ ($$ "-inf.0") _test-num-pos) ($$ "+nan.0"))
;; ;; ...
;; ;; variadics
;; (newline)


;; ;; multiplication
;; (display "flonum's multiplication : \n")
;; (test-fl (* ($$ "2.0") ($$ "2.0")) ($$ "4.0"))
;; (test-fl (* ($$ "-0.2") ($$ "-0.2")) ($$ "0.040000002"))
;; (test-fl (* ($$ "1.0e+5") ($$ "-100000.0")) ($$ "-1.0e+10"))
;; (test-fl (* ($$ "1.0e+5") ($$ "-100000")) ($$ "-1.0e+10")) ;; FIXME problem with string->number?
;; ;; special symbols
;; (test-fl (* ($$ "-0.0") ($$ "+0.0")) ($$ "-0.0"))
;; (test-fl (* ($$ "-0.0") ($$ "1.0")) ($$ "-0.0"))
;; (test-fl (* ($$ "-0.0") ($$ "-0.0")) ($$ "0.0"))
;; (test-fl (* ($$ "+0.0") ($$ "+nan.0")) ($$ "+nan.0"))
;; (test-fl (* ($$ "+0.0") ($$ "-inf.0")) ($$ "+nan.0")) 
;; (test-fl (* ($$ "-0.0") ($$ "+inf.0")) ($$ "+nan.0")) 
;; (test-fl (* ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
;; (test-fl (* ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0"))
;; (test-fl (* ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
;; (test-fl (* ($$ "+inf.0") ($$ "+inf.0")) ($$ "+inf.0"))
;; (test-fl (* ($$ "+inf.0") ($$ "-inf.0")) ($$ "-inf.0"))
;; (test-fl (* ($$ "-inf.0") ($$ "-inf.0")) ($$ "+inf.0")) 
;; (test-fl (* ($$ "+inf.0") ($$ "-1.0")) ($$ "-inf.0"))
;; (test-fl (* ($$ "-inf.0") ($$ "-1.0")) ($$ "+inf.0"))
;; ;; extremal values
;; (test-fl (* _fl-min-subnormal _fl-min-subnormal) ($$ "0.0"))
;; (test-fl (* _fl-max-subnormal _fl-max-subnormal-neg) ($$ "-0.0"))
;; (test-fl (* _fl-max-normal _fl-max-normal) ($$ "+inf.0"))
;; (test-fl (* _fl-max-normal-neg _fl-max-normal-neg) ($$ "+inf.0"))
;; ;; fixnums
;; (test-fl (* ($$ "2.0") 2) ($$ "4.0"))
;; (test-fl (* ($$ "1.0e+5") -100000) ($$ "-1.0e+10"))
;; (test-fl (* ($$ "-0.0") 0) ($$ "-0.0")) ;; gambit evaluates to 0... FIXME?
;; (test-fl (* ($$ "+nan.0") 0) ($$ "+nan.0")) ;; gambit evaluates to 0... FIXME? 
;; (test-fl (* ($$ "+nan.0") 1) ($$ "+nan.0")) 
;; (test-fl (* ($$ "+inf.0") 0) ($$ "+nan.0")) ;; gambit evaluates to 0... FIXME?
;; (test-fl (* ($$ "+inf.0") -1) ($$ "-inf.0"))
;; (test-fl (* ($$ "-inf.0") 0) ($$ "+nan.0")) ;; gambit evaluates to 0... FIXME?
;; (test-fl (* ($$ "-inf.0") -1) ($$ "+inf.0"))
;; ;; ...
;; ;; bignums
;; (test-fl (* ($$ "1.0e+10") 10000000000000000000000000) ($$ "1.0e+35"))
;; (test-fl (* ($$ "1.0e+10") -10000000000000000000000000) ($$ "-1.0e+35"))
;; (test-fl (* ($$ "1.23e+5") 4560000000000000000000000000000) ($$ "5.6087998e+35"))
;; (test-fl (* _fl-max-normal _max-normal) ($$ "+inf.0"))
;; (test-fl (* _fl-max-normal _max-normal-neg) ($$ "-inf.0"))
;; (test-fl (* ($$ "+nan.0") _test-num-pos) ($$ "+nan.0"))
;; (test-fl (* ($$ "+inf.0") _test-num-pos) ($$ "+inf.0")) 
;; (test-fl (* ($$ "+inf.0") _test-num-neg) ($$ "-inf.0")) 
;; (test-fl (* ($$ "-inf.0") _test-num-pos) ($$ "-inf.0")) 
;; (test-fl (* ($$ "-inf.0") _test-num-neg) ($$ "+inf.0")) 
;; ;; ...
;; ;; variadics
;; (newline)


;; ;; substraction
;; (display "flonum's substraction: \n")
;; (test-fl (- ($$ "1.0") ($$ "-1.0")) ($$ "2.0"))
;; (test-fl (- ($$ "-0.1") ($$ "0.1")) ($$ "-0.2"))
;; (test-fl (- ($$ "-10.0e+4") ($$ "-100000.0")) ($$ "0.0"))
;; ;; special symbols
;; (test-fl (- ($$ "+0.0") ($$ "-0.0")) ($$ "0.0")) 
;; (test-fl (- ($$ "-0.0") ($$ "-0.0")) ($$ "0.0"))
;; (test-fl (- ($$ "-0.0") ($$ "+0.0")) ($$ "-0.0"))
;; (test-fl (- ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
;; (test-fl (- ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0")) 
;; (test-fl (- ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
;; (test-fl (- ($$ "+inf.0") ($$ "-inf.0")) ($$ "+inf.0")) 
;; (test-fl (- ($$ "-inf.0") ($$ "+inf.0")) ($$ "-inf.0")) 
;; (test-fl (- ($$ "+inf.0") ($$ "+inf.0")) ($$ "+nan.0"))
;; (test-fl (- ($$ "+inf.0") ($$ "-1.0")) ($$ "+inf.0"))
;; (test-fl (- ($$ "-inf.0") ($$ "-inf.0")) ($$ "+nan.0"))
;; (test-fl (- ($$ "-inf.0") ($$ "1.0")) ($$ "-inf.0"))
;; ;; extremal values
;; (test-fl (- _fl-min-subnormal _fl-min-subnormal) ($$ "0.0"))
;; (test-fl (- _fl-min-subnormal-neg _fl-min-subnormal-neg) ($$ "0.0"))
;; (test-fl (- _fl-max-normal _fl-max-normal-neg) ($$ "+inf.0"))
;; (test-fl (- _fl-max-normal-neg _fl-max-normal) ($$ "-inf.0"))
;; ;; ...
;; ;; fixnums
;; (test-fl (- ($$ "1.0") -1) ($$ "2.0"))
;; (test-fl (- ($$ "-1.0") 1) ($$ "-2.0"))
;; (test-fl (- ($$ "+nan.0") 1) ($$ "+nan.0"))
;; (test-fl (- ($$ "+inf.0") 1) ($$ "+inf.0"))
;; (test-fl (- ($$ "-inf.0") -1) ($$ "-inf.0"))
;; (test-fl (- _fl-min-subnormal 1) ($$ "-1.0"))
;; (test-fl (- _fl-max-subnormal -1) ($$ "1.0"))
;; (test-fl (- _fl-min-subnormal-neg -1) ($$ "1.0"))
;; (test-fl (- _fl-max-subnormal-neg 1) ($$ "-1.0"))
;; (test-fl (- _fl-min-normal -1) ($$ "1.0"))
;; (test-fl (- _fl-min-normal-neg 1) ($$ "-1.0"))
;; (test-fl (- _fl-max-normal 1) _fl-max-normal)
;; (test-fl (- _fl-max-normal-neg -1) _fl-max-normal-neg)
;; ;; ...
;; ;; bignums
;; (test-fl (- ($$ "1.0e+35") -100000000000000000000000000000000000) ($$ "2.0e+35"))
;; (test-fl (- ($$ "-1.0e+35") 100000000000000000000000000000000000) ($$ "-2.0e+35"))
;; (test-fl (- ($$ "6.78e+30") -123450000000000000000000000000000000) ($$ "1.2345678e+35"))
;; (test-fl (- _fl-max-normal _max-normal-neg) ($$ "+inf.0"))
;; (test-fl (- _fl-max-normal-neg _max-normal) ($$ "-inf.0"))
;; (test-fl (- ($$ "+nan.0") _test-num-pos) ($$ "+nan.0"))
;; (test-fl (- ($$ "+inf.0") _test-num-neg) ($$ "+nan.0")) 
;; (test-fl (- ($$ "-inf.0") _test-num-pos) ($$ "+nan.0"))
;; ;; ...
;; (newline)


;; ;; division
;; (display "flonum's division: \n")
;; (test-fl (/ ($$ "4.0") ($$ "2.0")) ($$ "2.0"))
;; (test-fl (/ ($$ "-0.4") ($$ "0.2")) ($$ "-2.0"))
;; (test-fl (/ ($$ "1.0e+15") ($$ "-100000.0")) ($$ "-1.0e+10"))
;; ;; special symbols
;; (test-fl (/ ($$ "+0.0") ($$ "-0.0")) ($$ "+nan.0"))
;; (test-fl (/ ($$ "-0.0") ($$ "1.0")) ($$ "-0.0"))
;; (test-fl (/ ($$ "+0.0") ($$ "+nan.0")) ($$ "+nan.0"))
;; (test-fl (/ ($$ "+0.0") ($$ "+inf.0")) ($$ "0.0"))
;; (test-fl (/ ($$ "+0.0") ($$ "-inf.0")) ($$ "-0.0"))
;; (test-fl (/ ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
;; (test-fl (/ ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0"))
;; (test-fl (/ ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
;; (test-fl (/ ($$ "+inf.0") ($$ "+inf.0")) ($$ "+nan.0"))
;; (test-fl (/ ($$ "+inf.0") ($$ "-inf.0")) ($$ "+nan.0"))
;; (test-fl (/ ($$ "+inf.0") ($$ "-1.0")) ($$ "-inf.0"))
;; (test-fl (/ ($$ "-inf.0") ($$ "-1.0")) ($$ "+inf.0"))
;; (test-fl (/ ($$ "1.0") ($$ "0.0")) ($$ "+inf.0"))
;; (test-fl (/ ($$ "1.0") ($$ "+0.0")) ($$ "+inf.0"))
;; (test-fl (/ ($$ "1.0") ($$ "-0.0")) ($$ "-inf.0"))
;; (test-fl (/ ($$ "1.0") ($$ "+inf.0")) ($$ "0.0"))
;; (test-fl (/ ($$ "1.0") ($$ "-inf.0")) ($$ "-0.0"))
;; ;; extremal values
;; (test-fl (/ _fl-min-subnormal _fl-min-subnormal) ($$ "1.0"))
;; (test-fl (/ _fl-max-subnormal _fl-max-subnormal-neg) ($$ "-1.0"))
;; ;; fixnums
;; (test-fl (/ ($$ "4.0") 2) ($$ "2.0"))
;; (test-fl (/ 4 ($$ "-2.0")) ($$ "-2.0"))
;; ;; ...
;; ;; bignums
;; (test-fl (/ ($$ "2.0e+35") 100000000000000000000000000000000000) ($$ "2.0"))
;; (test-fl (/ ($$ "2.0e+35") -100000000000000000000000000000000000) ($$ "-2.0"))
;; (test-fl (/ _fl-max-normal _max-normal) ($$ "1.0"))
;; (test-fl (/ _fl-max-normal _max-normal-neg) ($$ "-1.0"))
;; ;; ...
;; ;; variadics
;; (newline)

;; ;; abs
;; (display "flonum's abs : \n")
;; (test-fl (abs ($$ "1.0")) ($$ "1.0"))
;; (test-fl (abs ($$ "-1.0")) ($$ "1.0"))
;; (test-fl (abs ($$ "0.0")) ($$ "0.0"))
;; (test-fl (abs ($$ "+0.0")) ($$ "0.0"))
;; (test-fl (abs ($$ "-0.0")) ($$ "0.0")) ;; FIXME could be solved just with a logical and
;; (test-fl (abs ($$ "+nan.0")) ($$ "+nan.0")) 
;; (test-fl (abs ($$ "+inf.0")) ($$ "+inf.0"))
;; (test-fl (abs ($$ "-inf.0")) ($$ "+inf.0"))
;; (test-fl (abs ($$ "-1.0e+10")) ($$ "1.0e+10"))
;; (test-fl (abs ($$ "-10000000000.0")) ($$ "1.0e+10"))
;; (test-fl (abs ($$ "-1.0e-10")) ($$ "1.0e-10"))
;; (test-fl (abs ($$ "-0.0000000001")) ($$ "1.0e-10"))
;; (newline)

;; ;; quotient

;; ;; remainder

;; ;; modulo

;; ;; gcd

;; ;; lcm

;; ;; floor
;; (display "flonum's floor : \n")
;; (test-fl (floor ($$ "1.0")) ($$ "1.0"))
;; (test-fl (floor ($$ "1.5")) ($$ "1.0"))
;; (test-fl (floor ($$ "1.99")) ($$ "1.0"))
;; (test-fl (floor ($$ "-1.0")) ($$ "-1.0"))
;; (test-fl (floor ($$ "-1.01")) ($$ "-2.0"))
;; (test-fl (floor ($$ "-1.5")) ($$ "-2.0"))
;; (test-fl (floor ($$ "0.0")) ($$ "0.0"))
;; (test-fl (floor ($$ "+0.0")) ($$ "0.0")) 
;; (test-fl (floor ($$ "-0.0")) ($$ "-0.0")) ;; FIXME
;; (test-fl (floor ($$ "+nan.0")) ($$ "+nan.0")) 
;; ;; (test-fl (floor ($$ "+inf.0")) #f) ;; FIXME not sure what to do here
;; ;; (test-fl (floor ($$ "-inf.0")) #f) ;; FIXME not sure what to do here
;; (test-fl (floor _fl-min-subnormal) ($$ "0.0"))
;; (test-fl (floor _fl-min-subnormal-neg) ($$ "-1.0"))
;; (test-fl (floor _fl-max-subnormal) ($$ "0.0"))
;; (test-fl (floor _fl-max-subnormal-neg) ($$ "-1.0"))
;; (test-fl (floor _fl-min-normal) ($$ "0.0"))
;; (test-fl (floor _fl-min-normal-neg) ($$ "-1.0"))
;; (newline)

;; ;; ceiling
;; (display "flonum's ceiling : \n")
;; (test-fl (ceiling ($$ "1.0")) ($$ "1.0"))
;; (test-fl (ceiling ($$ "1.5")) ($$ "2.0"))
;; (test-fl (ceiling ($$ "1.01")) ($$ "2.0"))
;; (test-fl (ceiling ($$ "-1.0")) ($$ "-1.0"))
;; (test-fl (ceiling ($$ "-1.5")) ($$ "-1.0"))
;; (test-fl (ceiling ($$ "-1.99")) ($$ "-1.0"))
;; (test-fl (ceiling ($$ "0.0")) ($$ "0.0"))
;; (test-fl (ceiling ($$ "+0.0")) ($$ "0.0")) 
;; (test-fl (ceiling ($$ "-0.0")) ($$ "-0.0")) ;; FIXME
;; (test-fl (ceiling ($$ "+nan.0")) ($$ "+nan.0")) 
;; ;; (test-fl (ceiling ($$ "+inf.0")) #f) ;; FIXME not sure what to do here
;; ;; (test-fl (ceiling ($$ "-inf.0")) #f) ;; FIXME not sure what to do here
;; (test-fl (ceiling _fl-min-subnormal) ($$ "1.0"))
;; (test-fl (ceiling _fl-min-subnormal-neg) ($$ "-0.0"))
;; (test-fl (ceiling _fl-max-subnormal) ($$ "1.0"))
;; (test-fl (ceiling _fl-max-subnormal-neg) ($$ "-0.0"))
;; (test-fl (ceiling _fl-min-normal) ($$ "1.0"))
;; (test-fl (ceiling _fl-min-normal-neg) ($$ "-0.0"))
;; (newline)

;; ;; truncate
;; (display "flonum's truncate : \n")
;; (test-fl (truncate ($$ "1.0")) ($$ "1.0"))
;; (test-fl (truncate ($$ "1.5")) ($$ "1.0"))
;; (test-fl (truncate ($$ "1.99")) ($$ "1.0"))
;; (test-fl (truncate ($$ "-1.0")) ($$ "-1.0"))
;; (test-fl (truncate ($$ "-1.5")) ($$ "-1.0"))
;; (test-fl (truncate ($$ "-1.99")) ($$ "-1.0"))
;; (test-fl (truncate ($$ "+0.0")) ($$ "0.0"))
;; (test-fl (truncate ($$ "-0.0")) ($$ "-0.0")) 
;; (test-fl (truncate ($$ "+nan.0")) ($$ "+nan.0")) ;; FIXME? gambit returns an error
;; (test-fl (truncate ($$ "+inf.0")) ($$ "+inf.0")) ;; FIXME? gambit returns an error
;; (test-fl (truncate ($$ "-inf.0")) ($$ "-inf.0")) ;; FIXME? gambit returns an error
;; (test-fl (truncate _fl-min-subnormal) ($$ "0.0"))
;; (test-fl (truncate _fl-min-subnormal-neg) ($$ "-0.0"))
;; (newline)

;; ;; round
;; (display "flonum's round : \n")
;; (test-fl (round ($$ "1.0")) ($$ "1.0"))
;; (test-fl (round ($$ "1.49")) ($$ "1.0"))
;; (test-fl (round ($$ "1.5")) ($$ "2.0"))
;; (test-fl (round ($$ "-1.0")) ($$ "-1.0"))
;; (test-fl (round ($$ "-1.49")) ($$ "-1.0"))
;; (test-fl (round ($$ "-1.5")) ($$ "-2.0"))
;; (test-fl (round ($$ "+0.0")) ($$ "0.0"))
;; (test-fl (round ($$ "-0.0")) ($$ "-0.0")) 
;; (test-fl (round ($$ "+nan.0")) ($$ "+nan.0")) ;; FIXME? gambit returns an error
;; (test-fl (round ($$ "+inf.0")) ($$ "+inf.0")) ;; FIXME? gambit returns an error
;; (test-fl (round ($$ "-inf.0")) ($$ "-inf.0")) ;; FIXME? gambit returns an error
;; (test-fl (round _fl-min-subnormal) ($$ "0.0"))
;; (test-fl (round _fl-min-subnormal-neg) ($$ "-0.0"))
;; (newline)

