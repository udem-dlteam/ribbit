;; Bignum's predicates


;; Numerical types predicates

;; number?
(test-display "bignum's number?:")
(test-display (number? 845100400152152934331135470251))
(test-display (number? -56713727820156410577229101238628035243))

;; complex?
(test-display "bignum's complex?:")
(test-display (complex? 2305843009213693951))
(test-display (complex? -618970019642690137449562111))

;; real?
(test-display "bignum's real?:")
(test-display (real? 162259276829213363391578010288127))
(test-display (real? -170141183460469231731687303715884105727))

;; rational?
(test-display "bignum's rational?:")
(test-display (rational? 99194853094755497))
(test-display (rational? -1066340417491710595814572169))

;; integer?
(test-display "bignum's integer?:")
(test-display (integer? 19134702400093278081449423917))
(test-display (integer? -23768741896345550770650537601358309))


;; Exactness predicates

;; exact?
(test-display "bignum's exact?:")
(test-display (exact? 10888869450418352160768000001))
(test-display (exact? -265252859812191058636308479999999))

;; inexact?
(test-display "bignum's inexact?:")
(test-display (inexact? 263130836933693530167218012159999999))
(test-display (inexact? -8683317618811886495518194401279999999))


;; Numerical properties predicates

;; zero?
(test-display "bignum's zero?:")
(test-display (zero? 31415926535897932384626433832795028841971693993751))
(test-display (zero? -31415926535897932384626433832795028841971693993751))

;; positive?
(test-display "bignum's positive?:")
(test-display (positive? 31415926535897932384626433832795028841971693993751))
(test-display (positive? -31415926535897932384626433832795028841971693993751))

;; negative?
(test-display "bignum's negative?:")
(test-display (negative? 31415926535897932384626433832795028841971693993751))
(test-display (negative? -31415926535897932384626433832795028841971693993751))

;; odd?
(test-display "bignum's odd?:")
(test-display (odd? 21516084244485963766983895228684783123552658213144))
(test-display (odd? 95768572624334418930396864262434107732269780280731))
(test-display (odd? -89154411010446823252716201052652272111660396665573))
(test-display (odd? -92547110557853763466820653109896526918620564769312))

;; even?
(test-display "bignum's even?:")
(test-display (even? 21516084244485963766983895228684783123552658213144))
(test-display (even? 95768572624334418930396864262434107732269780280731))
(test-display (even? -89154411010446823252716201052652272111660396665573))
(test-display (even? -92547110557853763466820653109896526918620564769312))


;;;run: -l nums/nums -f+ nums/bignum -l r4rs -f+ scheme-bignum
;;;expected:
;;;bignum's number?:
;;;#t
;;;#t
;;;bignum's complex?:
;;;#t
;;;#t
;;;bignum's real?:
;;;#t
;;;#t
;;;bignum's rational?:
;;;#t
;;;#t
;;;bignum's integer?:
;;;#t
;;;#t
;;;bignum's exact?:
;;;#t
;;;#t
;;;bignum's inexact?:
;;;#f
;;;#f
;;;bignum's zero?:
;;;#f
;;;#f
;;;bignum's positive?:
;;;#t
;;;#f
;;;bignum's negative?:
;;;#f
;;;#t
;;;bignum's odd?:
;;;#f
;;;#t
;;;#t
;;;#f
;;;bignum's even?:
;;;#t
;;;#f
;;;#f
;;;#t
