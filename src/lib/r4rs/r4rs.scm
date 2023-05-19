;; R4RS as a library for ribbit.

(##include "./types.scm")
(##include "./bool.scm")
(##include "./io.scm")
(##include "./pair-list.scm")
(##include "./number.scm")
(##include "./math.scm")
(##include "./string.scm")
(##include "./vector.scm")
(##include "./control.scm")

;;;----------------------------------------------------------------------------

;; Symbols (R4RS section 6.4).

(define global-var-ref field0)
(define global-var-set! field0-set!)

;;;----------------------------------------------------------------------------


;; Characters (R4RS section 6.6).

(define char? integer?)

(define char=? eqv?)
(define char<? <)
(define char>? >)
(define char<=? <=)
(define char>=? >=)

;;(define char-ci=? eqv?)
;;(define char-ci<? <)
;;(define char-ci>? >)
;;(define char-ci<=? <=)
;;(define char-ci>=? >=)

;;(define (char-alphabetic? c) ...)
;;(define (char-numeric? c) ...)
;;(define (char-whitespace? c) ...)
;;(define (char-upper-case? c) ...)
;;(define (char-lower-case? c) ...)

(define char->integer id)
(define integer->char id)

;;(define (char-upcase c) ...)
;;(define (char-downcase c) ...)
