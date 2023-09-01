(##include-once "./pair-list.scm")
(##include-once "./types.scm")

;; Characters (R4RS section 6.6).

(define (char=? ch1 ch2) (eqv? ch1 ch2))
(define (char<? ch1 ch2) (##< (##field0 ch1) (##field0 ch2)))
(define (char>? ch1 ch2) (##< (##field0 ch2) (##field0 ch1)))
(define (char<=? ch1 ch2) (not (char>? ch1 ch2)))
(define (char>=? ch1 ch2) (not (char<? ch1 ch2)))

(define (char-ci=? ch1 ch2) (char=? (char-upcase ch1) (char-upcase ch2)))
(define (char-ci<? ch1 ch2) (char<? (char-upcase ch1) (char-upcase ch2)))
(define (char-ci>? ch1 ch2) (char>? (char-upcase ch1) (char-upcase ch2)))
(define (char-ci<=? ch1 ch2) (not (char-ci>? ch1 ch2)))
(define (char-ci>=? ch1 ch2) (not (char-ci<? ch1 ch2)))

(define (char-alphabetic? ch)
  (or (char-lower-case? ch)
      (char-upper-case? ch)))

(define (char-numeric? ch)
  (and (##< 47 (##field0 ch))   ;; #\0
       (##< (##field0 ch) 58))) ;; #\9

(define (char-whitespace? ch)
  (pair? (memq (##field0 ch) '(32 9 10 11 13))))  ;; (#\space #\tab #\newline #\vtab #\return)

(define (char-lower-case? ch)
  (and (##< 96 (##field0 ch))    ;; #\a
       (##< (##field0 ch) 123))) ;; #\z

(define (char-upper-case? ch)
  (and (##< 64 (##field0 ch))   ;; #\A
       (##< (##field0 ch) 91))) ;; #\Z

(define (char-upcase ch)
  (if (char-lower-case? ch)
      (integer->char (##- (##field0 ch) 32)) ; (- (##field0 #\A) (##field0 #\a))
      ch))

(define (char-downcase ch)
  (if (char-upper-case? ch)
      (integer->char (##+ (##field0 ch) 32)) ; (- (##field0 #\a) (##field0 #\A))
      ch))



