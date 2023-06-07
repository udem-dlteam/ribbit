;; R4RS as a library for ribbit.

(##include-once "./types.scm")
(##include-once "./bool.scm")
(##include-once "./io.scm")
(##include-once "./pair-list.scm")
(##include-once "./number.scm")
(##include-once "./math.scm")
(##include-once "./string.scm")
(##include-once "./vector.scm")
(##include-once "./control.scm")
(##include-once "./compiler.scm")

;;;----------------------------------------------------------------------------

;; Symbols (R4RS section 6.4).

(define global-var-ref field0)
(define global-var-set! field0-set!)

;;;----------------------------------------------------------------------------
