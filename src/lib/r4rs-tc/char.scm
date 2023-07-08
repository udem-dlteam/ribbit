(##include-once "./types.scm")
(##include-once "./pair-list.scm")

;; Characters (R4RS section 6.6).

(define (char=? ch1 ch2) (eqv? ch1 ch2))
(define (char<? ch1 ch2) (< (char->integer ch1) (char->integer ch2)))
(define (char>? ch1 ch2) (> (char->integer ch1) (char->integer ch2)))
(define (char<=? ch1 ch2) (not (char>? ch1 ch2)))
(define (char>=? ch1 ch2) (not (char<? ch1 ch2)))

(define (char-ci=? ch1 ch2) (char=? (char-upcase ch1) (char-upcase ch2)))
(define (char-ci<? ch1 ch2) (char<? (char-upcase ch1) (char-upcase ch2)))
(define (char-ci>? ch1 ch2) (char>? (char-upcase ch1) (char-upcase ch2)))
(define (char-ci<=? ch1 ch2) (not (char-ci>? ch1 ch2)))
(define (char-ci>=? ch1 ch2) (not (char-ci<? ch1 ch2)))

(define-signatures
  (char=? char<? char>? char<=? char>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?)
  ((ch1
     guard: (char? ch1)
     expected: "CHARACTER")
   (ch2
     guard: (char? ch2)
     expected: "CHARACTER")))


(define (char-alphabetic? ch)
  (or (char-lower-case? ch)
      (char-upper-case? ch)))

(define (char-between start end) 
  (lambda (ch) (and (char>=? ch start) (char<=? ch end))))

(define char-numeric? (char-between #\0 #\9))
(define char-lower-case? (char-between #\a #\z))
(define char-upper-case? (char-between #\A #\Z))

(define (char-whitespace? ch) (< (char->integer ch) 33))

(define (char-upcase ch)
  (if (char-lower-case? ch)
      (integer->char (- (char->integer ch) 32))
      ch))

(define (char-downcase ch)
  (if (char-upper-case? ch)
      (integer->char (+ (char->integer ch) 32))
      ch))


(define-signatures
  (char-upcase char-downcase char-alphabetic? char-numeric? char-lower-case? char-upper-case? char-whitespace?)
  ((ch
     guard: (char? ch)
     expected:"CHARACTER")))
