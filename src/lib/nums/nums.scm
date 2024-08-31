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
;;    - conversions between numeric types 
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

(define (number? n) (complex? n))
(define (complex? n) (real? n)) 
(define (real? n) (or (rational? n) (flonum? n)))

(define (rational? n)
  (if-feature nums/flonum
    (or (integer? n) (and (flonum? n) (finite? n)))
    (integer? n)))
    
(define (integer? n) 
  (if-feature nums/flonum
   (or (inexact-integer? n) (or (fixnum? n) (bignum? n)))
   (or (fixnum? n) (bignum? n))))


;; Exactness predicates

(define (exact? n) (not (inexact? n)))

(define (inexact? n)
  (if-feature nums/flonum (flonum? n) #f))

(define (exact-integer? n)
  (if-feature nums/flonum
    (and (integer? n) (not (flonum? n)))
    (integer? n)))


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
               (##fl= (if (exact-integer? a)
                          (##exact-integer->inexact-integer a)
                          a)
                      (if (exact-integer? b)
                          (##exact-integer->inexact-integer b)
                          b)))
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
               (##fl< (if (exact-integer? a)
                          (##exact-integer->inexact-integer a)
                          a)
                      (if (exact-integer? b)
                          (##exact-integer->inexact-integer b)
                          b)))
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
               (if (integer? a)
                   (##fx-odd? (##inexact-integer->exact-integer a))
                   (error "*** ERROR - INTEGER expected in procedure: odd?")))
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
               (if (integer? a)
                   (##fx-even? (##inexact-integer->exact-integer a))
                   (error "*** ERROR - INTEGER expected in procedure: even?")))
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
;;           (##fl+ (cond ((fixnum? a) (##exact-integer->inexact-integer a))
;;                        ((bignum? a) (##exact-integer->inexact-integer a))
;;                        (else a))
;;                  (cond ((fixnum? b) (##exact-integer->inexact-integer b))
;;                        ((bignum? b) (##exact-integer->inexact-integer b))
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
               (##fl+ (if (exact-integer? a)
                          (##exact-integer->inexact-integer a)
                          a)
                      (if (exact-integer? b)
                          (##exact-integer->inexact-integer b)
                          b)))
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
               (##fl* (if (exact-integer? a)
                          (##exact-integer->inexact-integer a)
                          a)
                      (if (exact-integer? b)
                          (##exact-integer->inexact-integer b)
                          b)))
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
               (##fl- (if (exact-integer? a)
                          (##exact-integer->inexact-integer a)
                          a)
                      (if (exact-integer? b)
                          (##exact-integer->inexact-integer b)
                          b)))
              (else
               (bn-norm (##bn- (if (bignum? a) a (fixnum->bignum a))
                               (if (bignum? b) b (fixnum->bignum b))))))))))


(define (/ a b)
  (if-feature nums/flonum
    (if (or (flonum? a) (flonum? b))
        (##fl/ (if (exact-integer? a) (##exact-integer->inexact-integer a) a)
               (if (exact-integer? b) (##exact-integer->inexact-integer b) b))
        (quotient a b))
    (quotient a b)))

    
;;------------------------------------------------------------------------------

;; Absolute value

(define (abs a) (if (< a 0) (- 0 a) a))

;; (define (abs a)
;;   (if-feature (and (not nums/bignum) (not nums/flonum))
;;     (##fx-abs a b)
;;     (if-feature (and nums/bignum (not nums/flonum))
;;       (if (fixnum? a)
;;           (bn-norm (##fx-abs))
;;           (bn-norm (##bn-abs a)))
;;       (if-feature nums/flonum
;;         (cond ((fixnum? a) (##fx-abs a))
;;               ((flonum? a) (##fl-abs a))
;;               (else (##bn-abs a)))))))


;;------------------------------------------------------------------------------

;; Quotient, remainder, and modulo

;; FIXME more compact logic for these

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
               (if (and (integer? a) (integer? b))
                   ;; Not sure for this one, makes sense if the host supports
                   ;; returns a floating-point number result and if the host
                   ;; allows for division between fixnums and flonums
                   (##exact-integer->inexact-integer
                    (##fx-remainder
                     (if (flonum? a) (##inexact-integer->exact-integer a) a)
                     (if (flonum? b) (##inexact-integer->exact-integer b) b)))
                   (error
                    "*** ERROR - INTEGER expected in procedure: quotient")))
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
               (if (and (integer? a) (integer? b))
                   (##exact-integer->inexact-integer
                    (##fx-remainder
                     (if (flonum? a) (##inexact-integer->exact-integer a) a)
                     (if (flonum? b) (##inexact-integer->exact-integer b) b)))
                   (error
                    "*** ERROR - INTEGER expected in procedure: remainder")))
              ((fixnum? b)
               (bn-norm (##bn-fx-remainder a b)))
              (else
               (bn-norm
                (##bn-remainder (if (bignum? a) a (fixnum->bignum a)) b))))))))


;; (if-feature compact
  
;;   (define (modulo x y)
;;     (let ((q (quotient x y)))
;;       (let ((r (- x (* y q))))
;;      (if (eqv? r 0)
;;             0
;;             (if (eqv? (< x 0) (< y 0))
;;              r
;;              (+ r y))))))

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
               (if (and (integer? a) (integer? b))
                   (##exact-integer->inexact-integer
                    (##fx-modulo
                     (if (flonum? a) (##inexact-integer->exact-integer a) a)
                     (if (flonum? b) (##inexact-integer->exact-integer b) b)))
                   (error
                    "*** ERROR - INTEGER expected in procedure: modulo")))
              (else
               (bn-norm
                (##bn-modulo (if (bignum? a) a (fixnum->bignum a))
                             (if (bignum? b) b (fixnum->bignum b))))))))))


;;------------------------------------------------------------------------------

;; Gcd and lcm

(define (gcd x y)

  (define (##gcd x y)
    (let* ((ax (abs x))
           (ay (abs y)))
        (if (< ax ay) (##gcd-aux ax ay) (##gcd-aux ay ax))))

  (define (##gcd-aux x y)
    (if (eqv? x 0) y (##gcd-aux (remainder y x) x)))

  (if-feature nums/flonum
    (if (and (integer? x) (integer? y))
        (##exact-integer->inexact-integer
         (##gcd (if (flonum? x) (##inexact-integer->exact-integer x) x)
                (if (flonum? y) (##inexact-integer->exact-integer y) y)))
        (error "*** ERROR - INTEGER expected in procedure: gcd"))
    
    (##gcd x y)))


(define (lcm x y)
  
  (define (##lcm x y)
    (if (eqv? y 0)
        0
        (let* ((ax (abs x))
               (ay (abs y)))
          (* (quotient ax (gcd ax ay)) ay))))

  (if-feature nums/flonum
    (if (and (integer? x) (integer? y))
        (##exact-integer->inexact-integer
         (##lcm (if (flonum? x) (##inexact-integer->exact-integer x) x)
                (if (flonum? y) (##inexact-integer->exact-integer y) y)))
        (error "*** ERROR - INTEGER expected in procedure: gcd"))
    
    (##lcm x y)))


;;------------------------------------------------------------------------------

;; Floor, ceiling, truncate, and round

(define (floor a)
  (if-feature nums/flonum
    (if (flonum? a) (##fl-floor a) (##id a))
    (##id a)))
          
(define (ceiling a)
  (if-feature nums/flonum
    (if (flonum? a) (##fl-ceiling a) (##id a))
    (##id a)))

(define (truncate a)
  (if-feature nums/flonum
    (if (flonum? a) (##fl-truncate a) (##id a))
    (##id a)))

(define (round a)
  (if-feature nums/flonum
    (if (flonum? a) (##fl-round a) (##id a))
    (##id a)))


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
