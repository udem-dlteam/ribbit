;;; File: nums.scm

;;; Author: Frédéric Lahaie-Bertrand

;;==============================================================================

(##include-once (ribbit "define-macro"))

(if-feature (or nums/fixnum nums/bignum nums/flonum)
  (begin (##include-once "./lib/nums/fixnum.scm"))) 

(if-feature (or nums/bignum nums/flonum) 
  (begin (##include-once "./lib/nums/bignum.scm")))

(if-feature nums/flonum
  (begin (##include-once "./lib/nums/flonum.scm")))
                
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
(define (complex? n) (real? n)) ;; (or (real? n) (cpxnum? n)))
(define (real? n) (or (rational? n) (flonum? n)))
(define (rational? n) (integer? n)) ;; (or (integer? n) (ratnum? n)))
(define (integer? n) (or (fixnum? n) (bignum? n)))


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

(define (max a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##fx-max a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (and (fixnum? a) (fixnum? b))
          (##fx-max a b)
          (bn-norm (##bn-max (if (bignum? a) a (fixnum->bignum a))
                             (if (bignum? b) b (fixnum->bignum b)))))
      (if-feature nums/flonum
        (cond ((and (fixnum? a) (fixnum? b))
               (##fx-max a b))
              ((or (flonum? a) (flonum? b))
               (##fl-max (cond ((fixnum? a) (num->flonum a))
                               ((bignum? a) (num->flonum a))
                               (else a))
                         (cond ((fixnum? b) (num->flonum b))
                               ((bignum? b) (num->flonum b))
                               (else b))))
              (else
               (bn-norm
                (##bn-max (if (bignum? a) a (fixnum->bignum a))
                          (if (bignum? b) b (fixnum->bignum b))))))))))


(define (min a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##fx-min a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (and (fixnum? a) (fixnum? b))
          (##fx-min a b)
          (bn-norm (##bn-min (if (bignum? a) a (fixnum->bignum a))
                             (if (bignum? b) b (fixnum->bignum b)))))
      (if-feature nums/flonum
        (cond ((and (fixnum? a) (fixnum? b))
               (##fx-min a b))
              ((or (flonum? a) (flonum? b))
               (##fl-min (cond ((fixnum? a) (num->flonum a))
                               ((bignum? a) (num->flonum a))
                               (else a))
                         (cond ((fixnum? b) (num->flonum b))
                               ((bignum? b) (num->flonum b))
                               (else b))))
              (else
               (bn-norm
                (##bn-min (if (bignum? a) a (fixnum->bignum a))
                          (if (bignum? b) b (fixnum->bignum b))))))))))


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

(define (abs a)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##fx-abs a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (fixnum? a)
          (bn-norm (##fx-abs))
          (bn-norm (##bn-abs a)))
      (if-feature nums/flonum
        (cond ((fixnum? a) (##fx-abs a))
              ((flonum? a) (##fl-abs a))
              (else (##bn-abs a)))))))


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


(define (gcd a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##fx-gcd a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (and (fixnum? a) (fixnum? b))
          (##fx-gcd a b)
          (bn-norm
           (##bn-gcd (if (bignum? a) a (fixnum->bignum a))
                     (if (bignum? b) b (fixnum->bignum b))))))))

(define (lcm a b)
  (if-feature (and (not nums/bignum) (not nums/flonum))
    (##fx-lcm a b)
    (if-feature (and nums/bignum (not nums/flonum))
      (if (and (fixnum? a) (fixnum? b))
          (##fx-lcm a b)
          (bn-norm
           (##bn-lcm (if (bignum? a) a (fixnum->bignum a))
                     (if (bignum? b) b (fixnum->bignum b))))))))


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



;;------------------------------------------------------------------------------

;; Bignums tests

;; number?
(display "bignum's number?: \n")
(test (number? 845100400152152934331135470251) #t)
(test (number? -56713727820156410577229101238628035243) #t)
(newline)

;; complex?
(display "bignum's complex?: \n")
(test (complex? 2305843009213693951) #t)
(test (complex? -618970019642690137449562111) #t)
(newline)

;; real?
(display "bignum's real?: \n")
(test (real? 162259276829213363391578010288127) #t)
(test (real? -170141183460469231731687303715884105727) #t)
(newline)

;; rational?
(display "bignum's rational?: \n")
(test (rational? 99194853094755497) #t)
(test (rational? -1066340417491710595814572169) #t)
(newline)

;; integer?
(display "bignum's integer?: \n")
(test (integer? 19134702400093278081449423917) #t)
(test (integer? -23768741896345550770650537601358309) #t)
(newline)

;; exact?
(display "bignum's exact?: \n")
(test (exact? 10888869450418352160768000001) #t)
(test (exact? -265252859812191058636308479999999) #t)
(newline)

;; inexact?
(display "bignum's inexact?: \n")
(test (inexact? 263130836933693530167218012159999999) #f)
(test (inexact? -8683317618811886495518194401279999999) #f)
(newline)

;; =
(display "bignum's =: \n")
(test (= 0 5230817703675159067350235072835405670403867435136222247715891504953098) #f)
(test (= -4448933309634087807693259939780541934144737744184263129860809988868741 0) #f)
(test (= 570 58635662018558100729360659876486117910453348850346) #f)
(test (= 113 -65768675324944166803962657978771855608455296407854) #f)
(test (= 26993908145466464588079727082668306343285878569830 -733) #f)
(test (= -58089330657574067954571637752542021149557615814002 -523) #f)
(test (= 3260472156951623965864573021631598193195 3260472156951623965864573021631598193195) #t)
(test (= 5012622859413021647155097925923099079654 -5012622859413021647155097925923099079654) #f)
(test (= -7376125517656751357517829666454779174501 7376125517656751357517829666454779174501) #f)
(test (= -1673538129741677294786724229246543668009 -1673538129741677294786724229246543668009) #t)
(test (= 1299614890304639947132962107340437518957 1299614890304639947132962107340437518956) #f)
(test (= -1337575114959501566049631862947265473642 -1337575114959501566049631862947265473641) #f)
(newline)

;; <
(display "bignum's <: \n")
(test (< 0 5230817703675159067350235072835405670403867435136222247715891504953098) #t)
(test (< -4448933309634087807693259939780541934144737744184263129860809988868741 0) #t)
(test (< 570 58635662018558100729360659876486117910453348850346) #t)
(test (< 113 -65768675324944166803962657978771855608455296407854) #f)
(test (< 26993908145466464588079727082668306343285878569830 -733) #f)
(test (< -58089330657574067954571637752542021149557615814002 -523) #t)
(test (< 3260472156951623965864573021631598193195 3260472156951623965864573021631598193195) #f)
(test (< 5012622859413021647155097925923099079654 -5012622859413021647155097925923099079654) #f)
(test (< -7376125517656751357517829666454779174501 7376125517656751357517829666454779174501) #t)
(test (< -1673538129741677294786724229246543668009 -1673538129741677294786724229246543668009) #f)
(test (< 1299614890304639947132962107340437518957 1299614890304639947132962107340437518956) #f)
(test (< -1337575114959501566049631862947265473642 -1337575114959501566049631862947265473641) #t)
(newline)

;; >
(display "bignum's >: \n")
(test (> 0 5230817703675159067350235072835405670403867435136222247715891504953098) #f)
(test (> -4448933309634087807693259939780541934144737744184263129860809988868741 0) #f)
(test (> 570 58635662018558100729360659876486117910453348850346) #f)
(test (> 113 -65768675324944166803962657978771855608455296407854) #t)
(test (> 26993908145466464588079727082668306343285878569830 -733) #t)
(test (> -58089330657574067954571637752542021149557615814002 -523) #f)
(test (> 3260472156951623965864573021631598193195 3260472156951623965864573021631598193195) #f)
(test (> 5012622859413021647155097925923099079654 -5012622859413021647155097925923099079654) #t)
(test (> -7376125517656751357517829666454779174501 7376125517656751357517829666454779174501) #f)
(test (> -1673538129741677294786724229246543668009 -1673538129741677294786724229246543668009) #f)
(test (> 1299614890304639947132962107340437518957 1299614890304639947132962107340437518956) #t)
(test (> -1337575114959501566049631862947265473642 -1337575114959501566049631862947265473641) #f)
(newline)

;; <=
(display "bignum's <=: \n")
(test (<= 0 5230817703675159067350235072835405670403867435136222247715891504953098) #t)
(test (<= -4448933309634087807693259939780541934144737744184263129860809988868741 0) #t)
(test (<= 570 58635662018558100729360659876486117910453348850346) #t)
(test (<= 113 -65768675324944166803962657978771855608455296407854) #f)
(test (<= 26993908145466464588079727082668306343285878569830 -733) #f)
(test (<= -58089330657574067954571637752542021149557615814002 -523) #t)
(test (<= 3260472156951623965864573021631598193195 3260472156951623965864573021631598193195) #t)
(test (<= 5012622859413021647155097925923099079654 -5012622859413021647155097925923099079654) #f)
(test (<= -7376125517656751357517829666454779174501 7376125517656751357517829666454779174501) #t)
(test (<= -1673538129741677294786724229246543668009 -1673538129741677294786724229246543668009) #t)
(test (<= 1299614890304639947132962107340437518957 1299614890304639947132962107340437518956) #f)
(test (<= -1337575114959501566049631862947265473642 -1337575114959501566049631862947265473641) #t)
(newline)

;; >=
(display "bignum's >=: \n")
(test (>= 0 5230817703675159067350235072835405670403867435136222247715891504953098) #f)
(test (>= -4448933309634087807693259939780541934144737744184263129860809988868741 0) #f)
(test (>= 570 58635662018558100729360659876486117910453348850346) #f)
(test (>= 113 -65768675324944166803962657978771855608455296407854) #t)
(test (>= 26993908145466464588079727082668306343285878569830 -733) #t)
(test (>= -58089330657574067954571637752542021149557615814002 -523) #f)
(test (>= 3260472156951623965864573021631598193195 3260472156951623965864573021631598193195) #t)
(test (>= 5012622859413021647155097925923099079654 -5012622859413021647155097925923099079654) #t)
(test (>= -7376125517656751357517829666454779174501 7376125517656751357517829666454779174501) #f)
(test (>= -1673538129741677294786724229246543668009 -1673538129741677294786724229246543668009) #t)
(test (>= 1299614890304639947132962107340437518957 1299614890304639947132962107340437518956) #t)
(test (>= -1337575114959501566049631862947265473642 -1337575114959501566049631862947265473641) #f)
(newline)

;; zero?
(display "bignum's zero?: \n")
(test (zero? 31415926535897932384626433832795028841971693993751) #f)
(test (zero? -31415926535897932384626433832795028841971693993751) #f)
(newline)

;; positive?
(display "bignum's positive?: \n")
(test (positive? 31415926535897932384626433832795028841971693993751) #t)
(test (positive? -31415926535897932384626433832795028841971693993751) #f)
(newline)

;; negative?
(display "bignum's negative?: \n")
(test (negative? 31415926535897932384626433832795028841971693993751) #f)
(test (negative? -31415926535897932384626433832795028841971693993751) #t)
(newline)

;; odd?
(display "bignum's odd?: \n")
(test (odd? 21516084244485963766983895228684783123552658213144) #f)
(test (odd? 95768572624334418930396864262434107732269780280731) #t)
(test (odd? -89154411010446823252716201052652272111660396665573) #t)
(test (odd? -92547110557853763466820653109896526918620564769312) #f)
(newline)

;; even?
(display "bignum's even?: \n")
(test (even? 21516084244485963766983895228684783123552658213144) #t)
(test (even? 95768572624334418930396864262434107732269780280731) #f)
(test (even? -89154411010446823252716201052652272111660396665573) #f)
(test (even? -92547110557853763466820653109896526918620564769312) #t)
(newline)

;; max
(display "bignum's max: \n")
(test (max 1133053054882046652138414 6951941511609433057270365) 6951941511609433057270365)
(test (max 7595919530921861173819326 -1179310511854807446237996) 7595919530921861173819326)
(test (max -2749567351885752724891227 9381830119491298336733624) 9381830119491298336733624)
(test (max -4065664308602139494639522 -4737190702179860943702770) -4065664308602139494639522)
(test (max 53921717629317675238467481846766940513200056812714 0)
         53921717629317675238467481846766940513200056812714)
(test (max -1 -52635608277857713427577896091736371787214684409012) -1)
(newline)

;; min
(display "bignum's min: \n")
(test (min 2495343014654958537105079 2279689258923542019956112) 2279689258923542019956112)
(test (min 1290219608640344181598136 -2977477130996051870721134) -2977477130996051870721134)
(test (min -9999998372978049951059731 7328160963185950244594553) -9999998372978049951059731)
(test (min -4690830264252230825334468 -5035261931188171010003137) -5035261931188171010003137)
(test (min 83875288658753320838142061717766914730359825349042 0) 0)
(test (min -1 -87554687311595628638823537875937519577818577805321)
         -87554687311595628638823537875937519577818577805321)
(newline)

;; addition
(display "bignum's addition: \n")
(test (+ 0 -5093029553211653449872027) -5093029553211653449872027)
(test (+ 5596023648066549911988183 1) 5596023648066549911988184)
(test (+ 80676928238280689964004824354037014163149658979409 24323789690706977942236250822168895738379862300159)
         105000717928987667906241075176205909901529521279568)
(test (+ 37764716512289357860158816175578297352334460428151 -26272037343146531977774160319906655418763979293344)
         11492679169142825882384655855671641933570481134807)
(test (+ -19521541341899485444734567383162499341913181480927 77710386387734317720754565453220777092120190516609)
         58188845045834832276019998070058277750207009035682)
(test (+ -62804909263601975988281613323166636528619326686336 -62735676303544776280350450777235547105859548702790)
         -125540585567146752268632064100402183634478875389126)
(newline)

;; multiplication
(display "bignum's multiplication: \n")
(test (* 204208320856611906254543372131 535958450687724602901618766795)
         109447175263851475898050147902487213179665409408592991190145)
(test (* 240616342522577195429162991930 645537799140373404328752628889)
         155326944189230725681962101155865142950816677893856191865770)
(test (* 639958794757291746426357455254 790914513571113694109119393251)
         506152698861019586175178635212961921714585921347356862090754)
(test (* 910760208252026187985318877058 429725916778131496990090192116)
         391377265456143916734134781243890615978829116741876604874728)
(test (* 359614589019389713111790429782856475032031986915140287080859 904)
         325091588473528300653058548523702253428956916171286819521096536)
(test (* 947647772622414254854540332157185306142288137585043063321751 -1)
         -947647772622414254854540332157185306142288137585043063321751)
(test (* 1 -4949450114654062843366393790039769265672146385306736096571209)
         -4949450114654062843366393790039769265672146385306736096571209)
(test (* -1807 -8880078692560290228472104031721186082041900042296617119637792)
         16046302197456444442849091985320183250249713376429987135185490144)
(test (* 31290 29547) 924525630)
(test (* 76446248246857926039535277348030480290058760758251047470916439613626760449256274 0) 0)
(newline)

;; substraction
(display "bignum's substraction: \n")
(test (- 775356636980742 65425278625518184175) -65424503268881203433)
(test (- 746728909777727 -93800081647060016145) 93800828375969793872)
(test (- -249192173217214 77235014144197356854) -77235263336370574068)
(test (- -816136115735255 -21334757418494684385) 21333941282378949130)
(test (- 4328 8983569485562099219222184) -8983569485562099219217856)
(test (- 2332390739414333454776241 -6863) 2332390739414333454783104)
(test (- 8143562401451718062464362679456127531813 407833033625423278394497538243720583531147711992606381334677)
         -407833033625423278386353975842268865468683349313150253802864)
(test (- 6879695970309833913077109870408591337464 -144282277263465947047458784778720192771528073176790770715721)
         144282277263465947054338480749030026684605183047199362053185)
(test (- 344473060570073349243693113835049316312840425121925651798069 -4113528013147013047816437885185290928545)
         344473060570073349247806641848196329360656863007110942726614)
(test (- -201165839341965621349143415956258658655705526904965209858033 -8507224264829397285847831630577775606888)
         -201165839341965621340636191691429261369857695274387434251145)
(newline)

;; division
(display "bignum's division: \n")
(test (/ 5820974944592307816406286208998628034825 34211706798214808651) 170145704186148656447) 
(newline)

;; abs
(display "bingum's abs: \n")
(test (abs 2187312514712053292819182618612586732157919841484882916447060957527069)
         2187312514712053292819182618612586732157919841484882916447060957527069)
(test (abs -5722091756711672291098169091528017350671274858322287183520935396572512)
         5722091756711672291098169091528017350671274858322287183520935396572512)
(newline)

;; quotient
(display "bignum's quotient: \n")
(test (quotient 3883786360950680064225125 205117392984896) 18934456529)
(test (quotient 8412848862694560424196528 -502221066118630) -16751286296)
(test (quotient -6744278622039194945047123 713786960956364) -9448587591)
(test (quotient -3719172874677646575739624 -138908658326459) 26774233654)
(test (quotient 958133904780275900994657640789 5126) 186916485520927799647806796)
(test (quotient 946839835259570982582262052248 -9407) -100652687919588708683136180)
(test (quotient -726719478268482601476990902640 1363) -533176433065651211648562657)
(test (quotient -944374553050682034962524517493 -9965) 94769147320690620668592525)
(test (quotient 1431429809190659250937221696461515709858 1) 1431429809190659250937221696461515709858)
(test (quotient 3874105978859597729754989301617539284681 -1) -3874105978859597729754989301617539284681)
(test (quotient 3826868386 -894277415599185592524595395943) 0)
(test (quotient 85225499546667278239864565961163548862305774564980 355936345681743241125150760694)
         239440283580566865841)
(test (quotient 85225499546667278239864565961163548862305774564980 -355936345681743241125150760694)
         -239440283580566865841)
(test (quotient -79451096596094025228879710893145669136867228748940 560101503308617928680920874760)
         -141851246830730585008)
(test (quotient -91782493858900971490967598526136554978189312978482 -168299894872265880485756401427)
         545350868629840110843)
(newline)

;; remainder
(display "bignum's remainder: \n")
(test (remainder 31415926535897932384 626433832795028) 269821227278184)
(test (remainder 84197169399375105820 -974944592307816) 954407672116060)
(test (remainder -40628620899862803482 534211706798214) -217962738234140)
(test (remainder -808651328230664709384 -460955058223172) -174365045185644)
(test (remainder 10499725246808459872 7364) 4452)
(test (remainder 46958486538367362226 -2609) 1090)
(test (remainder -91246080512438843904 5124) -1536)
(test (remainder -41365497627807977156 -9143) -1592)
(test (remainder 53594081284811174502 -53594081284811174502) 0)
(test (remainder 47755513237964145152374623436454285844479526586782 105114135473573952311342716610)
         5975323008975327512344064272)
(test (remainder 21359695362314429524849371871101457654035902799344 -037420073105785390621983874478)
         14577110881247298898268851848)
(test (remainder -84784896833214457138687519435064302184531910484810 537061468067491927819119793995)
         -371670919999668461747445868975)
(test (remainder -20614196634287544406437451237181921799983910159195 -618146751426912397489409071864)
         -241207608238831296814278237947)
(newline)

;; modulo
(display "bignum's modulo: \n")
(test (modulo 841027019385211055596 446229489549303) 5022568170073)
(test (modulo 819644288109756659334 -461284756482337) -140995282932867)
(test (modulo -867831652712019091456 485669234603486) 17877788765252)
(test (modulo -104543266482133936072 -602491412737245) -161526792658162)
(test (modulo 59977001296160894416 9486) 778)
(test (modulo 85558484063534220722 -2582) -1902)
(test (modulo -84886481584560285060 1684) 1404)
(test (modulo -27394522674676788952 -5213) -4897)
(test (modulo -870066063155881748815 -209209628292540) -39981787375855)
(test (modulo -917153643678925903600 -917153643678925903600) 0)
(test (modulo 94231961567945208095146550225231603881930142093762 137855956638937787083039069792)
         90585486260603582098162526050)
(test (modulo 77346722182562599661501421503068038447734549202605 414665925201497442850732518666)
         215368454539423767664906648493)
(test (modulo 21324340881907104863317346496514539057962685610055 810665879699816357473638405257)
         559803199168492902195474634561)
(test (modulo 14591028970641401109712062804390397595156771577004 203378699360072305587631763594)
         180913516359088603385344241858)
(newline)

;; ;; gcd
;; (display "bignum's gcd: \n")
;; (test (gcd 71226806613001927876 0))
;; (test (gcd 0 -61119590921642019893))
;; (test (gcd 80952572010654858632 78865936153381827968))
;; (test (gcd 23030195203530185296 89957736225994138912))
;; (test (gcd 49721775283479131515 57485724245415069595))
;; (test (gcd 82953311686172785588 90750983817546374649))
;; (test (gcd 39319255060400927701 39319255060400927701))
;; (test (gcd 67113900984882401285 5))
;; (test (gcd -1 83616035637076601047))
;; (test (gcd 5306143444318586769751456614000000000000 7765913440120562238994561314071120000000))
;; (test (gcd -7589852437441702913276561809377344403070 -3794926218720851456638280904688672201535))
;; (newline)

;; ;; lcm
;; (display "bignum's lcm: \n")
;; (test (lcm 101819429555961 0))
;; (test (lcm 0 -989467678374494))
;; (test (lcm 482553797747268 471040475346462))
;; (test (lcm 804668425906949 -129331367702898))
;; (test (lcm -915210475216205 696602405803815))
;; (test (lcm -193511253382430 -355876402474960))
;; (test (lcm 473263914199272 473263914199272))
;; (test (lcm 604269922796782 3547))
;; (test (lcm -8163 600934172199245))
;; (test (lcm 8631 5030))
;; (test (lcm 1083579151369882091444210067510334671103 1412671113699086585163983150197016515116))
;; (test (lcm -8517143765761835155650884909989859982387 -3455283316355076479185358932261854896321))

;; floor
(display "bignum's floor: \n")
(test (floor 8517143765761835155650884909989859982387) 8517143765761835155650884909989859982387)
(test (floor -3455283316355076479185358932261854896321) -3455283316355076479185358932261854896321)
(newline)

;; ceiling
(display "bignum's ceiling: \n")
(test (ceiling 3293308985706420467525907091548141654985) 3293308985706420467525907091548141654985)
(test (ceiling -9461637180270981994309924488957571282890) -9461637180270981994309924488957571282890)
(newline)

;; truncate
(display "bignum's truncate: \n")
(test (truncate 5923233260972997120844335732654893823911) 5923233260972997120844335732654893823911)
(test (truncate -9325974636673058360414281388303203824903) -9325974636673058360414281388303203824903)
(newline)

;; round
(display "bignum's round: \n")
(test (round 7589852437441702913276561809377344403070) 7589852437441702913276561809377344403070)
(test (round -7469211201913020330380197621101100449293) -7469211201913020330380197621101100449293)
(newline)

;; ;; string->number
;; (display "bignum's string->number \n")
;; (test (string->number "314159265358979323846264338327950288419716939937510582097494459230781640628")
;;       314159265358979323846264338327950288419716939937510582097494459230781640628)
;; (test (string->number "-314159265358979323846264338327950288419716939937510582097494459230781640628")
;;       -314159265358979323846264338327950288419716939937510582097494459230781640628)
;; (newline)

;; number->string
(display "bignum's number->string: \n")
(test (number->string 314159265358979323846264338327950288419716939937510582097494459230781640628)
         "314159265358979323846264338327950288419716939937510582097494459230781640628")
(test (number->string -314159265358979323846264338327950288419716939937510582097494459230781640628)
         "-314159265358979323846264338327950288419716939937510582097494459230781640628")
(newline)


;;------------------------------------------------------------------------------

;; Flonums tests (single-precision)


;; FIXME patches to get stuff running for now until the reader and writer are
;; modified

(define $$ string->number)

(define _fl-min-subnormal ($$ "1.401298464324817e-45"))
(define _fl-min-subnormal-neg ($$ "-1.401298464324817e-45"))
(define _fl-max-subnormal ($$ "1.1754942106924411e-38"))
(define _fl-max-subnormal-neg ($$ "-1.1754942106924411e-38"))
(define _fl-min-normal ($$ "1.1754943508222875e-38")) ;; ieee32
(define _fl-min-normal-neg ($$ "-1.1754943508222875e-38")) ;; ieee32
(define _fl-max-normal ($$ "3.4028234663852886e+38")) ;; ieee32
(define _fl-max-normal-neg ($$ "-3.4028234663852886e+38")) ;; ieee32
(define _max-normal 340282346638528859811704183484516925440) ;; ieee32
(define _max-normal-neg -340282346638528859811704183484516925440) ;; ieee32

(define _test-num-pos 680564693277057719623408366969033850880) ;; ieee32
(define _test-num-neg -680564693277057719623408366969033850880) ;; ieee32

(define (display-fl n)
  (display (##fl-number->string n)))
      
(define (test-fl res expec)
  (if (and (not (equal? res expec))
           (and (not (nan? res)) (not (nan? expec))))
      (begin
        (display "test failed...\n")
        (display "expected: ") (display-fl expec) (newline)
        (display "result: ") (display-fl res) (newline))
      ;;(display "test passed...\n")))
      ))


;; number?
(display "flonum's number? : \n")
(test (number? ($$ "1.0")) #t)
(test (number? ($$ "-0.1")) #t)
(test (number? ($$ "+0.0")) #t)
(test (number? ($$ "-0.0")) #t)
(test (number? ($$ "+nan.0")) #t)
(test (number? ($$ "+inf.0")) #t)
(test (number? ($$ "-inf.0")) #t)
(test (number? ($$ "1.0e+10")) #t)
(test (number? ($$ "-1.0e-10")) #t)
(newline)

;; complex?
(display "flonum's complex? : \n")
(test (complex? ($$ "1.0")) #t)
(test (complex? ($$ "-0.1")) #t)
(test (complex? ($$ "+0.0")) #t)
(test (complex? ($$ "-0.0")) #t)
(test (complex? ($$ "+nan.0")) #t)
(test (complex? ($$ "+inf.0")) #t)
(test (complex? ($$ "-inf.0")) #t)
(test (complex? ($$ "1.0e+10")) #t)
(test (complex? ($$ "-1.0e-10")) #t)
(newline)

;; real?
(display "flonum's real? : \n")
(test (real? ($$ "1.0")) #t)
(test (real? ($$ "-0.1")) #t)
(test (real? ($$ "+0.0")) #t)
(test (real? ($$ "-0.0")) #t)
(test (real? ($$ "+nan.0")) #t)
(test (real? ($$ "+inf.0")) #t)
(test (real? ($$ "-inf.0")) #t)
(test (real? ($$ "1.0e+10")) #t)
(test (real? ($$ "-1.0e-10")) #t)
(newline)

;; rational?
(display "flonum's rational? : \n")
(test (rational? ($$ "1.0")) #f)
(test (rational? ($$ "-0.1")) #f)
(test (rational? ($$ "+0.0")) #f)
(test (rational? ($$ "-0.0")) #f)
(test (rational? ($$ "+nan.0")) #f)
(test (rational? ($$ "+inf.0")) #f)
(test (rational? ($$ "-inf.0")) #f)
(test (rational? ($$ "1.0e+10")) #f)
(test (rational? ($$ "-1.0e-10")) #f)
(newline)

;; integer?
(display "flonum's integer? : \n")
(test (integer? ($$ "1.0")) #f)
(test (integer? ($$ "-0.1")) #f)
(test (integer? ($$ "+0.0")) #f)
(test (integer? ($$ "-0.0")) #f)
(test (integer? ($$ "+nan.0")) #f)
(test (integer? ($$ "+inf.0")) #f)
(test (integer? ($$ "-inf.0")) #f)
(test (integer? ($$ "1.0e+10")) #f)
(test (integer? ($$ "-1.0e-10")) #f)
(newline)

;; exact?
(display "flonum's exact? : \n")
(test (exact? ($$ "1.0")) #f)
(test (exact? ($$ "-0.1")) #f)
(test (exact? ($$ "+0.0")) #f)
(test (exact? ($$ "-0.0")) #f)
(test (exact? ($$ "+nan.0")) #f)
(test (exact? ($$ "+inf.0")) #f)
(test (exact? ($$ "-inf.0")) #f)
(test (exact? ($$ "1.0e+10")) #f)
(test (exact? ($$ "-1.0e-10")) #f)
(newline)

;; inexact?
(display "flonum's inexact? : \n")
(test (inexact? ($$ "1.0")) #t)
(test (inexact? ($$ "-0.1")) #t)
(test (inexact? ($$ "+0.0")) #t)
(test (inexact? ($$ "-0.0")) #t)
(test (inexact? ($$ "+nan.0")) #t)
(test (inexact? ($$ "+inf.0")) #t)
(test (inexact? ($$ "-inf.0")) #t)
(test (inexact? ($$ "1.0e+10")) #t)
(test (inexact? ($$ "-1.0e-10")) #t)
(newline)

;; zero?
(display "flonum's zero? : \n")
(test (zero? ($$ "1.0")) #f)
(test (zero? ($$ "-0.1")) #f)
(test (zero? ($$ "0.0")) #t)
(test (zero? ($$ "+0.0")) #t) 
(test (zero? ($$ "-0.0")) #t)
(test (zero? ($$ "+nan.0")) #f)
(test (zero? ($$ "+inf.0")) #f)
(test (zero? ($$ "-inf.0")) #f)
(test (zero? ($$ "1.0e+10")) #f)
(test (zero? ($$ "-1.0e+10")) #f)
(test (zero? ($$ "1.0e-350")) #t)
(test (zero? ($$ "-1.0e-350")) #t)
(newline)

;; positive?
(display "flonum's positive? : \n")
(test (positive? ($$ "1.0")) #t)
(test (positive? ($$ "-0.1")) #f)
(test (positive? ($$ "0.0")) #f)
(test (positive? ($$ "+0.0")) #f) 
(test (positive? ($$ "-0.0")) #f)
(test (positive? ($$ "+nan.0")) #f)
(test (positive? ($$ "+inf.0")) #t)
(test (positive? ($$ "-inf.0")) #f)
(test (positive? ($$ "1.0e+10")) #t)
(test (positive? ($$ "-1.0e+10")) #f)
(newline)

;; negative?
(display "flonum's negative? : \n")
(test (negative? ($$ "1.0")) #f)
(test (negative? ($$ "-0.1")) #t)
(test (negative? ($$ "0.0")) #f)
(test (negative? ($$ "+0.0")) #f)
(test (negative? ($$ "-0.0")) #f)
(test (negative? ($$ "+nan.0")) #f)
(test (negative? ($$ "+inf.0")) #f)
(test (negative? ($$ "-inf.0")) #t)
(test (negative? ($$ "1.0e+10")) #f)
(test (negative? ($$ "-1.0e+10")) #t)
(newline)

;; ;; odd?
;; (display "flonum's odd? : \n")
;; (test (odd? ($$ "1.0")) #t)
;; (test (odd? ($$ "1.5")) 'error) ;; FIXME
;; (test (odd? ($$ "-2.0")) #f)
;; (test (odd? ($$ "-2.5")) 'error) ;; FIXME
;; (test (odd? ($$ "+0.0")) #t)
;; (test (odd? ($$ "-0.0")) #t)
;; (test (odd? ($$ "+nan.0")) #t)
;; (test (odd? ($$ "+inf.0")) #t)
;; (test (odd? ($$ "-inf.0")) #t)
;; (test (odd? ($$ "1.0000000001e+10")) #t)
;; (test (odd? ($$ "-1.0e+10")) #f)
;; (newline)

;; ;; even?
;; (display "flonum's even? : \n")
;; (test (even? ($$ "1.0")) #t)
;; (test (even? ($$ "1.5")) 'error)
;; (test (even? ($$ "-2.0")) #f)
;; (test (even? ($$ "-2.5")) 'error)
;; (test (even? ($$ "+0.0")) #t)
;; (test (even? ($$ "-0.0")) #t)
;; (test (even? ($$ "+nan.0")) #t)
;; (test (even? ($$ "+inf.0")) #t)
;; (test (even? ($$ "-inf.0")) #t)
;; (test (even? ($$ "1.0000000001e+10")) #f)
;; (test (even? ($$ "-1.0e+10")) #t)
;; (test (even? ($$ "1.0e+350")) #f)
;; (test (even? ($$ "-1.0e+350")) #f)
;; (newline)

;; = TODO variadics
(display "flonum's = : \n")
(test (= ($$ "1.0") ($$ "1.0")) #t)
(test (= ($$ "0.1") ($$ "-0.1")) #f)
(test (= ($$ "+0.0") ($$ "-0.0")) #t) 
(test (= ($$ "0.0") ($$ "1.0e-350")) #t)
(test (= ($$ "0.0") _fl-min-subnormal) #f)
(test (= ($$ "0.0") _fl-min-subnormal-neg) #f)
(test (= ($$ "+nan.0") ($$ "+nan.0")) #f)
(test (= ($$ "+nan.0") ($$ "1.0")) #f) 
(test (= ($$ "+inf.0") ($$ "+inf.0")) #t) 
(test (= ($$ "+inf.0") ($$ "1.0e+350")) #t) ;; conversion during parsing should work
(test (= ($$ "-inf.0") ($$ "-inf.0")) #t) 
(test (= ($$ "-inf.0") ($$ "-1.0e+350")) #t) ;; conversion during parsing should work
(test (= ($$ "+inf.0") ($$ "-inf.0")) #f)
(test (= ($$ "1e+10") ($$ "10000000000.0")) #t)
(test (= ($$ "1e+10") ($$ "1.0e+10")) #t) ;; equivalent notation
(test (= ($$ "1e+10") ($$ "1e10")) #t) ;; equivalent notation
(test (= ($$ "102334150.0") ($$ "102334152.0")) #t)   ;; same rep ieee32
(test (= ($$ "-102334152.0") ($$ "-102334155.0")) #t) ;; same rep ieee32
(test (= ($$ "1.0") 1) #t)
(test (= ($$ "-1.0") -1) #t)
(test (= _fl-max-normal _max-normal) #t)
(test (= _fl-max-normal-neg _max-normal-neg) #t)
(test (= ($$ "+inf.0") _test-num-pos) #t) ;; proper conversion of fx/bn to +inf.0
(test (= ($$ "+inf.0") _test-num-neg) #f) ;; proper conversion of fx/bn to inf
(test (= ($$ "-inf.0") _test-num-neg) #t) ;; proper conversion of fx/bn to -inf.0
(test (= ($$ "-inf.0") _test-num-pos) #f) ;; proper conversion of fx/bn to inf
;; (test (= 0 ($$ "0.0") ($$ "+0.0") ($$ "-0.0") ($$ "1.0e-350") ($$ "-1.0e-350")))
;; (test (= ($$ "102334150.0") ($$ "102334152.0") ($$ "102334155.0")) #t)
(newline)

;; < TODO variadics
(display "flonum's < : \n")
(test (< ($$ "1.0") ($$ "2.0")) #t)
(test (< ($$ "-1.0") ($$ "-2.0")) #f)
(test (< ($$ "-0.0") ($$ "+0.0")) #f) 
(test (< ($$ "-0.0") ($$ "1.0e-350")) #f)
(test (< ($$ "0.0") _fl-min-subnormal) #t)
(test (< ($$ "0.0") _fl-min-subnormal-neg) #f)
(test (< ($$ "+nan.0") ($$ "+nan.0")) #f)
(test (< ($$ "+nan.0") ($$ "1.0")) #f)
(test (< ($$ "+nan.0") ($$ "+inf.0")) #f)
(test (< ($$ "+inf.0") ($$ "+inf.0")) #f)
(test (< ($$ "+inf.0") ($$ "1.0e+350")) #f)
(test (< ($$ "-inf.0") ($$ "-inf.0")) #f)
(test (< ($$ "-inf.0") ($$ "-1.0e+350")) #f) 
(test (< ($$ "-inf.0") ($$ "+inf.0")) #t)
(test (< ($$ "-inf.0") ($$ "1.0e+350")) #t)
(test (< ($$ "+inf.0") ($$ "-inf.0")) #f)
(test (< ($$ "+inf.0") ($$ "-1.0e+350")) #f)
(test (< ($$ "102334150.0") ($$ "102334152.0")) #f) 
(test (< ($$ "-102334155.0") ($$ "-102334152.0")) #f) 
(test (< ($$ "0.99999994") ($$ "1.0")) #t)
(test (< ($$ "1.0") ($$ "1.0000001")) #t) 
(test (< ($$ "1.0000001") ($$ "1.0000002")) #t) 
(test (< ($$ "-0.99999994") ($$ "-1.0")) #f)
(test (< ($$ "-1.0") ($$ "-1.0000001")) #f) 
(test (< ($$ "-1.0000001") ($$ "-1.0000002")) #f) 
(test (< ($$ "1.0") 2) #t)
(test (< ($$ "-1.0") -2) #f)
(test (< ($$ "-inf.0") 0) #t)
(test (< ($$ "+inf.0") 0) #f)
(test (< _fl-max-normal _test-num-pos) #t) 
(test (< ($$ "+inf.0") _test-num-pos) #f) 
(test (< _fl-max-normal-neg _test-num-neg) #f) 
(test (< ($$ "-inf.0") _test-num-neg) #f) 
;; ;; (test (< ($$ "0.99999994") ($$ "1.0") ($$ "1.0000001") ($$ "1.0000002")))
;; ;; (test (< ($$ "-0.99999994") ($$ "-1.0") ($$ "-1.0000001") ($$ "-1.0000002")))
(newline)

;; > TODO variadics
(display "flonum's > : \n")
(test (> ($$ "1.0") ($$ "2.0")) #f)
(test (> ($$ "-1.0") ($$ "-2.0")) #t)
(test (> ($$ "-0.0") ($$ "+0.0")) #f) 
(test (> ($$ "+0.0") ($$ "1.0e-350")) #f) 
(test (> ($$ "0.0") _fl-min-subnormal) #f)
(test (> ($$ "0.0") _fl-min-subnormal-neg) #t)
(test (> ($$ "+nan.0") ($$ "+nan.0")) #f) 
(test (> ($$ "+nan.0") ($$ "1.0")) #f)
(test (> ($$ "+nan.0") ($$ "-inf.0")) #f)
(test (> ($$ "+inf.0") ($$ "+inf.0")) #f) 
(test (> ($$ "+inf.0") ($$ "1.0e+350")) #f) 
(test (> ($$ "-inf.0") ($$ "-inf.0")) #f) 
(test (> ($$ "-inf.0") ($$ "-1.0e+350")) #f) 
(test (> ($$ "-inf.0") ($$ "+inf.0")) #f)
(test (> ($$ "-inf.0") ($$ "1.0e+350")) #f)
(test (> ($$ "+inf.0") ($$ "-inf.0")) #t) 
(test (> ($$ "+inf.0") ($$ "-1.0e+350")) #t)
(test (> ($$ "102334150.0") ($$ "102334152.0")) #f) 
(test (> ($$ "-102334155.0") ($$ "-102334152.0")) #f) 
(test (> ($$ "0.99999994") ($$ "1.0")) #f)
(test (> ($$ "1.0") ($$ "1.0000001")) #f) 
(test (> ($$ "1.0000001") ($$ "1.0000002")) #f) 
(test (> ($$ "-0.99999994") ($$ "-1.0")) #t)
(test (> ($$ "-1.0") ($$ "-1.0000001")) #t) 
(test (> ($$ "-1.0000001") ($$ "-1.0000002")) #t) 
(test (> ($$ "1.0") 2) #f)
(test (> ($$ "-1.0") -2) #t)
(test (> ($$ "-inf.0") 0) #f)
(test (> ($$ "+inf.0") 0) #t)
(test (> _fl-max-normal _test-num-pos) #f) 
(test (> ($$ "+inf.0") _test-num-pos) #f) 
(test (> _fl-max-normal-neg _test-num-neg) #t)
(test (> ($$ "-inf.0") _test-num-neg) #f) 
;; ;; (test (> ($$ "0.99999994") ($$ "1.0") ($$ "1.0000001") ($$ "1.0000002")))
;; ;; (test (> ($$ "-0.99999994") ($$ "-1.0") ($$ "-1.0000001") ($$ "-1.0000002")))
(newline)

;; <=

;; >=

;; max
(display "flonum's max: \n")
(test-fl (max ($$ "1.0") ($$ "1.1")) ($$ "1.1"))
(test-fl (max ($$ "0.1") ($$ "-0.1")) ($$ "0.1"))
(test-fl (max ($$ "-0.1") ($$ "-1.0")) ($$ "-0.1"))
;; special symbols
(test-fl (max ($$ "+0.0") ($$ "-0.0")) ($$ "0.0"))
(test-fl (max ($$ "0.0") ($$ "+nan.0")) ($$ "+nan.0"))
(test-fl (max ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
(test-fl (max ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0"))
(test-fl (max ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
(test-fl (max ($$ "+inf.0") ($$ "-inf.0")) ($$ "+inf.0"))
;; fixnums
(test-fl (max ($$ "0.9") 1) ($$ "1.0"))
(test-fl (max ($$ "-1.1e5") -100000) ($$ "-100000.0"))
;; bignums
(test-fl (max ($$ "1.0e+35") 1000000000000000000000000000000000000) ($$ "1.0e+36"))
(test-fl (max ($$ "-1.0e+35") -1000000000000000000000000000000000000) ($$ "-1.0e+35"))
;; variadics
(newline)

;; min
(display "flonum's min: \n")
(test-fl (min ($$ "1.0") ($$ "1.1")) ($$ "1.0"))
(test-fl (min ($$ "0.1") ($$ "-0.1")) ($$ "-0.1"))
(test-fl (min ($$ "-0.1") ($$ "-1.0")) ($$ "-1.0"))
;; special symbols
(test-fl (min ($$ "+0.0") ($$ "-0.0")) ($$ "-0.0"))
(test-fl (min ($$ "0.0") ($$ "+nan.0")) ($$ "+nan.0"))
(test-fl (min ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
(test-fl (min ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0"))
(test-fl (min ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
(test-fl (min ($$ "+inf.0") ($$ "-inf.0")) ($$ "-inf.0"))
;; fixnums
(test-fl (min ($$ "0.9") 1) ($$ "0.9"))
(test-fl (min ($$ "-1.1e5") -100000) ($$ "-110000.0"))
;; bignums
(test-fl (min ($$ "1.0e+35") 1000000000000000000000000000000000000) ($$ "1.0e+35"))
(test-fl (min ($$ "-1.0e+35") -1000000000000000000000000000000000000) ($$ "-1.0e+36"))
;; variadics
(newline)


;; addition
(display "flonum's addition: \n")
(test-fl (+ ($$ "1.0") ($$ "1.0")) ($$ "2.0"))
(test-fl (+ ($$ "-0.1") ($$ "-0.1")) ($$ "-0.2"))
(test-fl (+ ($$ "10.0e+4") ($$ "100000.0")) ($$ "2.0e+5"))
;; special symbols
(test-fl (+ ($$ "+0.0") ($$ "-0.0")) ($$ "0.0"))
(test-fl (+ ($$ "-0.0") ($$ "-0.0")) ($$ "-0.0"))
(test-fl (+ ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
(test-fl (+ ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0"))
(test-fl (+ ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
(test-fl (+ ($$ "+inf.0") ($$ "-inf.0")) ($$ "+nan.0")) 
(test-fl (+ ($$ "-inf.0") ($$ "+inf.0")) ($$ "+nan.0")) 
(test-fl (+ ($$ "+inf.0") ($$ "+inf.0")) ($$ "+inf.0"))
(test-fl (+ ($$ "+inf.0") ($$ "-1.0")) ($$ "+inf.0"))
(test-fl (+ ($$ "-inf.0") ($$ "-inf.0")) ($$ "-inf.0"))
(test-fl (+ ($$ "-inf.0") ($$ "1.0")) ($$ "-inf.0"))
;; extremal values
(test-fl (+ _fl-min-subnormal _fl-min-subnormal-neg) ($$ "0.0"))
(test-fl (+ _fl-max-normal _fl-max-normal) ($$ "+inf.0"))
(test-fl (+ _fl-max-normal-neg _fl-max-normal-neg) ($$ "-inf.0"))
;; fixnums
(test-fl (+ ($$ "1.0") 1) ($$ "2.0"))
(test-fl (+ ($$ "-1.0") -1) ($$ "-2.0"))
(test-fl (+ ($$ "+nan.0") 1) ($$ "+nan.0"))
(test-fl (+ ($$ "+inf.0") -1) ($$ "+inf.0"))
(test-fl (+ ($$ "-inf.0") 1) ($$ "-inf.0"))
(test-fl (+ _fl-min-subnormal 1) ($$ "1.0"))
(test-fl (+ _fl-max-subnormal -1) ($$ "-1.0"))
(test-fl (+ _fl-min-subnormal-neg -1) ($$ "-1.0"))
(test-fl (+ _fl-max-subnormal-neg 1) ($$ "1.0"))
(test-fl (+ _fl-min-normal -1) ($$ "-1.0"))
(test-fl (+ _fl-min-normal-neg 1) ($$ "1.0"))
(test-fl (+ _fl-max-normal -1) _fl-max-normal)
(test-fl (+ _fl-max-normal-neg 1) _fl-max-normal-neg)
;; ...
;; bignums
(test-fl (+ ($$ "1.0e+35") 100000000000000000000000000000000000) ($$ "2.0e+35"))
(test-fl (+ ($$ "-1.0e+35") -100000000000000000000000000000000000) ($$ "-2.0e+35"))
(test-fl (+ ($$ "6.78e+30") 123450000000000000000000000000000000) ($$ "1.2345678e+35"))
(test-fl (+ _fl-max-normal _max-normal) ($$ "+inf.0"))
(test-fl (+ _fl-max-normal-neg _max-normal-neg) ($$ "-inf.0"))
(test-fl (+ ($$ "+nan.0") _test-num-pos) ($$ "+nan.0"))
(test-fl (+ ($$ "+inf.0") _test-num-neg) ($$ "+nan.0")) 
(test-fl (+ ($$ "-inf.0") _test-num-pos) ($$ "+nan.0"))
;; ...
;; variadics
(newline)


;; multiplication
(display "flonum's multiplication : \n")
(test-fl (* ($$ "2.0") ($$ "2.0")) ($$ "4.0"))
(test-fl (* ($$ "-0.2") ($$ "-0.2")) ($$ "0.040000002"))
(test-fl (* ($$ "1.0e+5") ($$ "-100000.0")) ($$ "-1.0e+10"))
(test-fl (* ($$ "1.0e+5") ($$ "-100000")) ($$ "-1.0e+10")) ;; FIXME problem with string->number?
;; special symbols
(test-fl (* ($$ "-0.0") ($$ "+0.0")) ($$ "-0.0"))
(test-fl (* ($$ "-0.0") ($$ "1.0")) ($$ "-0.0"))
(test-fl (* ($$ "-0.0") ($$ "-0.0")) ($$ "0.0"))
(test-fl (* ($$ "+0.0") ($$ "+nan.0")) ($$ "+nan.0"))
(test-fl (* ($$ "+0.0") ($$ "-inf.0")) ($$ "+nan.0")) 
(test-fl (* ($$ "-0.0") ($$ "+inf.0")) ($$ "+nan.0")) 
(test-fl (* ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
(test-fl (* ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0"))
(test-fl (* ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
(test-fl (* ($$ "+inf.0") ($$ "+inf.0")) ($$ "+inf.0"))
(test-fl (* ($$ "+inf.0") ($$ "-inf.0")) ($$ "-inf.0"))
(test-fl (* ($$ "-inf.0") ($$ "-inf.0")) ($$ "+inf.0")) 
(test-fl (* ($$ "+inf.0") ($$ "-1.0")) ($$ "-inf.0"))
(test-fl (* ($$ "-inf.0") ($$ "-1.0")) ($$ "+inf.0"))
;; extremal values
(test-fl (* _fl-min-subnormal _fl-min-subnormal) ($$ "0.0"))
(test-fl (* _fl-max-subnormal _fl-max-subnormal-neg) ($$ "-0.0"))
(test-fl (* _fl-max-normal _fl-max-normal) ($$ "+inf.0"))
(test-fl (* _fl-max-normal-neg _fl-max-normal-neg) ($$ "+inf.0"))
;; fixnums
(test-fl (* ($$ "2.0") 2) ($$ "4.0"))
(test-fl (* ($$ "1.0e+5") -100000) ($$ "-1.0e+10"))
(test-fl (* ($$ "-0.0") 0) ($$ "-0.0")) ;; gambit evaluates to 0... FIXME?
(test-fl (* ($$ "+nan.0") 0) ($$ "+nan.0")) ;; gambit evaluates to 0... FIXME? 
(test-fl (* ($$ "+nan.0") 1) ($$ "+nan.0")) 
(test-fl (* ($$ "+inf.0") 0) ($$ "+nan.0")) ;; gambit evaluates to 0... FIXME?
(test-fl (* ($$ "+inf.0") -1) ($$ "-inf.0"))
(test-fl (* ($$ "-inf.0") 0) ($$ "+nan.0")) ;; gambit evaluates to 0... FIXME?
(test-fl (* ($$ "-inf.0") -1) ($$ "+inf.0"))
;; ...
;; bignums
(test-fl (* ($$ "1.0e+10") 10000000000000000000000000) ($$ "1.0e+35"))
(test-fl (* ($$ "1.0e+10") -10000000000000000000000000) ($$ "-1.0e+35"))
(test-fl (* ($$ "1.23e+5") 4560000000000000000000000000000) ($$ "5.6087998e+35"))
(test-fl (* _fl-max-normal _max-normal) ($$ "+inf.0"))
(test-fl (* _fl-max-normal _max-normal-neg) ($$ "-inf.0"))
(test-fl (* ($$ "+nan.0") _test-num-pos) ($$ "+nan.0"))
(test-fl (* ($$ "+inf.0") _test-num-pos) ($$ "+inf.0")) 
(test-fl (* ($$ "+inf.0") _test-num-neg) ($$ "-inf.0")) 
(test-fl (* ($$ "-inf.0") _test-num-pos) ($$ "-inf.0")) 
(test-fl (* ($$ "-inf.0") _test-num-neg) ($$ "+inf.0")) 
;; ...
;; variadics
(newline)


;; substraction
(display "flonum's substraction: \n")
(test-fl (- ($$ "1.0") ($$ "-1.0")) ($$ "2.0"))
(test-fl (- ($$ "-0.1") ($$ "0.1")) ($$ "-0.2"))
(test-fl (- ($$ "-10.0e+4") ($$ "-100000.0")) ($$ "0.0"))
;; special symbols
(test-fl (- ($$ "+0.0") ($$ "-0.0")) ($$ "0.0")) 
(test-fl (- ($$ "-0.0") ($$ "-0.0")) ($$ "0.0"))
(test-fl (- ($$ "-0.0") ($$ "+0.0")) ($$ "-0.0"))
(test-fl (- ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
(test-fl (- ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0")) 
(test-fl (- ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
(test-fl (- ($$ "+inf.0") ($$ "-inf.0")) ($$ "+inf.0")) 
(test-fl (- ($$ "-inf.0") ($$ "+inf.0")) ($$ "-inf.0")) 
(test-fl (- ($$ "+inf.0") ($$ "+inf.0")) ($$ "+nan.0"))
(test-fl (- ($$ "+inf.0") ($$ "-1.0")) ($$ "+inf.0"))
(test-fl (- ($$ "-inf.0") ($$ "-inf.0")) ($$ "+nan.0"))
(test-fl (- ($$ "-inf.0") ($$ "1.0")) ($$ "-inf.0"))
;; extremal values
(test-fl (- _fl-min-subnormal _fl-min-subnormal) ($$ "0.0"))
(test-fl (- _fl-min-subnormal-neg _fl-min-subnormal-neg) ($$ "0.0"))
(test-fl (- _fl-max-normal _fl-max-normal-neg) ($$ "+inf.0"))
(test-fl (- _fl-max-normal-neg _fl-max-normal) ($$ "-inf.0"))
;; ...
;; fixnums
(test-fl (- ($$ "1.0") -1) ($$ "2.0"))
(test-fl (- ($$ "-1.0") 1) ($$ "-2.0"))
(test-fl (- ($$ "+nan.0") 1) ($$ "+nan.0"))
(test-fl (- ($$ "+inf.0") 1) ($$ "+inf.0"))
(test-fl (- ($$ "-inf.0") -1) ($$ "-inf.0"))
(test-fl (- _fl-min-subnormal 1) ($$ "-1.0"))
(test-fl (- _fl-max-subnormal -1) ($$ "1.0"))
(test-fl (- _fl-min-subnormal-neg -1) ($$ "1.0"))
(test-fl (- _fl-max-subnormal-neg 1) ($$ "-1.0"))
(test-fl (- _fl-min-normal -1) ($$ "1.0"))
(test-fl (- _fl-min-normal-neg 1) ($$ "-1.0"))
(test-fl (- _fl-max-normal 1) _fl-max-normal)
(test-fl (- _fl-max-normal-neg -1) _fl-max-normal-neg)
;; ...
;; bignums
(test-fl (- ($$ "1.0e+35") -100000000000000000000000000000000000) ($$ "2.0e+35"))
(test-fl (- ($$ "-1.0e+35") 100000000000000000000000000000000000) ($$ "-2.0e+35"))
(test-fl (- ($$ "6.78e+30") -123450000000000000000000000000000000) ($$ "1.2345678e+35"))
(test-fl (- _fl-max-normal _max-normal-neg) ($$ "+inf.0"))
(test-fl (- _fl-max-normal-neg _max-normal) ($$ "-inf.0"))
(test-fl (- ($$ "+nan.0") _test-num-pos) ($$ "+nan.0"))
(test-fl (- ($$ "+inf.0") _test-num-neg) ($$ "+nan.0")) 
(test-fl (- ($$ "-inf.0") _test-num-pos) ($$ "+nan.0"))
;; ...
(newline)


;; division
(display "flonum's division: \n")
(test-fl (/ ($$ "4.0") ($$ "2.0")) ($$ "2.0"))
(test-fl (/ ($$ "-0.4") ($$ "0.2")) ($$ "-2.0"))
(test-fl (/ ($$ "1.0e+15") ($$ "-100000.0")) ($$ "-1.0e+10"))
;; special symbols
(test-fl (/ ($$ "+0.0") ($$ "-0.0")) ($$ "+nan.0"))
(test-fl (/ ($$ "-0.0") ($$ "1.0")) ($$ "-0.0"))
(test-fl (/ ($$ "+0.0") ($$ "+nan.0")) ($$ "+nan.0"))
(test-fl (/ ($$ "+0.0") ($$ "+inf.0")) ($$ "0.0"))
(test-fl (/ ($$ "+0.0") ($$ "-inf.0")) ($$ "-0.0"))
(test-fl (/ ($$ "+nan.0") ($$ "+nan.0")) ($$ "+nan.0"))
(test-fl (/ ($$ "+nan.0") ($$ "+inf.0")) ($$ "+nan.0"))
(test-fl (/ ($$ "+nan.0") ($$ "-inf.0")) ($$ "+nan.0"))
(test-fl (/ ($$ "+inf.0") ($$ "+inf.0")) ($$ "+nan.0"))
(test-fl (/ ($$ "+inf.0") ($$ "-inf.0")) ($$ "+nan.0"))
(test-fl (/ ($$ "+inf.0") ($$ "-1.0")) ($$ "-inf.0"))
(test-fl (/ ($$ "-inf.0") ($$ "-1.0")) ($$ "+inf.0"))
(test-fl (/ ($$ "1.0") ($$ "0.0")) ($$ "+inf.0"))
(test-fl (/ ($$ "1.0") ($$ "+0.0")) ($$ "+inf.0"))
(test-fl (/ ($$ "1.0") ($$ "-0.0")) ($$ "-inf.0"))
(test-fl (/ ($$ "1.0") ($$ "+inf.0")) ($$ "0.0"))
(test-fl (/ ($$ "1.0") ($$ "-inf.0")) ($$ "-0.0"))
;; extremal values
(test-fl (/ _fl-min-subnormal _fl-min-subnormal) ($$ "1.0"))
(test-fl (/ _fl-max-subnormal _fl-max-subnormal-neg) ($$ "-1.0"))
;; fixnums
(test-fl (/ ($$ "4.0") 2) ($$ "2.0"))
(test-fl (/ 4 ($$ "-2.0")) ($$ "-2.0"))
;; ...
;; bignums
(test-fl (/ ($$ "2.0e+35") 100000000000000000000000000000000000) ($$ "2.0"))
(test-fl (/ ($$ "2.0e+35") -100000000000000000000000000000000000) ($$ "-2.0"))
(test-fl (/ _fl-max-normal _max-normal) ($$ "1.0"))
(test-fl (/ _fl-max-normal _max-normal-neg) ($$ "-1.0"))
;; ...
;; variadics
(newline)

;; abs
(display "flonum's abs : \n")
(test-fl (abs ($$ "1.0")) ($$ "1.0"))
(test-fl (abs ($$ "-1.0")) ($$ "1.0"))
(test-fl (abs ($$ "0.0")) ($$ "0.0"))
(test-fl (abs ($$ "+0.0")) ($$ "0.0"))
(test-fl (abs ($$ "-0.0")) ($$ "0.0")) ;; FIXME could be solved just with a logical and
(test-fl (abs ($$ "+nan.0")) ($$ "+nan.0")) 
(test-fl (abs ($$ "+inf.0")) ($$ "+inf.0"))
(test-fl (abs ($$ "-inf.0")) ($$ "+inf.0"))
(test-fl (abs ($$ "-1.0e+10")) ($$ "1.0e+10"))
(test-fl (abs ($$ "-10000000000.0")) ($$ "1.0e+10"))
(test-fl (abs ($$ "-1.0e-10")) ($$ "1.0e-10"))
(test-fl (abs ($$ "-0.0000000001")) ($$ "1.0e-10"))
(newline)

;; quotient

;; remainder

;; modulo

;; gcd

;; lcm

;; floor
(display "flonum's floor : \n")
(test-fl (floor ($$ "1.0")) ($$ "1.0"))
(test-fl (floor ($$ "1.5")) ($$ "1.0"))
(test-fl (floor ($$ "1.99")) ($$ "1.0"))
(test-fl (floor ($$ "-1.0")) ($$ "-1.0"))
(test-fl (floor ($$ "-1.01")) ($$ "-2.0"))
(test-fl (floor ($$ "-1.5")) ($$ "-2.0"))
(test-fl (floor ($$ "0.0")) ($$ "0.0"))
(test-fl (floor ($$ "+0.0")) ($$ "0.0")) 
(test-fl (floor ($$ "-0.0")) ($$ "-0.0")) ;; FIXME
(test-fl (floor ($$ "+nan.0")) ($$ "+nan.0")) 
;; (test-fl (floor ($$ "+inf.0")) #f) ;; FIXME not sure what to do here
;; (test-fl (floor ($$ "-inf.0")) #f) ;; FIXME not sure what to do here
(test-fl (floor _fl-min-subnormal) ($$ "0.0"))
(test-fl (floor _fl-min-subnormal-neg) ($$ "-1.0"))
(test-fl (floor _fl-max-subnormal) ($$ "0.0"))
(test-fl (floor _fl-max-subnormal-neg) ($$ "-1.0"))
(test-fl (floor _fl-min-normal) ($$ "0.0"))
(test-fl (floor _fl-min-normal-neg) ($$ "-1.0"))
(newline)

;; ceiling
(display "flonum's ceiling : \n")
(test-fl (ceiling ($$ "1.0")) ($$ "1.0"))
(test-fl (ceiling ($$ "1.5")) ($$ "2.0"))
(test-fl (ceiling ($$ "1.01")) ($$ "2.0"))
(test-fl (ceiling ($$ "-1.0")) ($$ "-1.0"))
(test-fl (ceiling ($$ "-1.5")) ($$ "-1.0"))
(test-fl (ceiling ($$ "-1.99")) ($$ "-1.0"))
(test-fl (ceiling ($$ "0.0")) ($$ "0.0"))
(test-fl (ceiling ($$ "+0.0")) ($$ "0.0")) 
(test-fl (ceiling ($$ "-0.0")) ($$ "-0.0")) ;; FIXME
(test-fl (ceiling ($$ "+nan.0")) ($$ "+nan.0")) 
;; (test-fl (ceiling ($$ "+inf.0")) #f) ;; FIXME not sure what to do here
;; (test-fl (ceiling ($$ "-inf.0")) #f) ;; FIXME not sure what to do here
(test-fl (ceiling _fl-min-subnormal) ($$ "1.0"))
(test-fl (ceiling _fl-min-subnormal-neg) ($$ "-0.0"))
(test-fl (ceiling _fl-max-subnormal) ($$ "1.0"))
(test-fl (ceiling _fl-max-subnormal-neg) ($$ "-0.0"))
(test-fl (ceiling _fl-min-normal) ($$ "1.0"))
(test-fl (ceiling _fl-min-normal-neg) ($$ "-0.0"))
(newline)

;; truncate
(display "flonum's truncate : \n")
(newline)

;; round
(display "flonum's round : \n")
(newline)

