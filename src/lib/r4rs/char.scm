(##include-once "./types.scm")
(##include-once "./bool.scm")

;; Characters (R4RS section 6.6).

(define (char=? ch1 ch2) (eqv? (char-cmp ch1 ch2) 0))
(define (char<? ch1 ch2) (< (char-cmp ch1 ch2) 0))
(define (char>? ch1 ch2) (< 0 (char-cmp ch1 ch2)))
(define (char<=? ch1 ch2) (not (char>? ch1 ch2)))
(define (char>=? ch1 ch2) (not (char<? ch1 ch2)))

(define (char-ci=? ch1 ch2) (char=? (char-upcase ch1) (char-upcase ch2)))
(define (char-ci<? ch1 ch2) (char<? (char-upcase ch1) (char-upcase ch2)))
(define (char-ci>? ch1 ch2) (char>? (char-upcase ch1) (char-upcase ch2)))
(define (char-ci<=? ch) (not (char-ci>? ch1 ch2)))
(define (char-ci>=? ch) (not (char-ci<? ch1 ch2)))

(define (char-cmp ch1 ch2)
  (cond 
    ((< (field0 ch1) (field0 ch2)) -1)
    ((< (field0 ch2) (field0 ch1)) 1)
    (else 0)))

(define (char-alphabetic? ch)
  (or (char-lower-case? ch)
      (char-upper-case? ch)))

(define (char-numeric? ch)
  (and (char>=? ch #\0)
       (char<=? ch #\9)))

(define (char-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\newline)
      (char=? ch #\vtab)
      (char=? ch #\return)))

(define (char-lower-case? ch)
  (and (char<=? #\a ch)
       (char<=? ch #\z)))

(define (char-upper-case? ch)
  (and (char<=? #\A ch)
       (char<=? ch #\Z)))

(define (char-upcase ch)
  (if (char-lower-case? ch)
      (integer->char (- (field0 ch) 32)) ; (- (field0 #\A) (field0 #\a))
      ch))

(define (char-downcase ch)
  (if (char-upper-case? ch)
      (integer->char (+ (field0 ch) 32)) ; (- (field0 #\a) (field0 #\A))
      ch))



