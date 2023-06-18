(##include-once "./types.scm")
(##include-once "./bool.scm")
(##include-once "./pair-list.scm")
(##include-once "./char.scm")
(##include-once "./control.scm")

;; Strings (R4RS section 6.7).

(define (string-length x) (##field1 x))

(define (string-ref str i) (integer->char (list-ref (##field0 str) i)))
(define (string-set! str i ch) (list-set! (##field0 str) i (##field0 ch)))

(define (make-string k (ch #\space)) (list->string (make-list k ch)))

(define (string . args) (list->string args))

(define (string=? str1 str2) (##eqv? (string-cmp str1 str2) 0))
(define (string<? str1 str2) (##< (string-cmp str1 str2) 0))
(define (string>? str1 str2) (##< 0 (string-cmp str1 str2)))
(define (string<=? str1 str2) (not (string>? str1 str2)))
(define (string>=? str1 str2) (not (string<? str1 str2)))

(define (string-ci=? str1 str2) (##eqv? (string-cmp-ci str1 str2) 0))
(define (string-ci<? str1 str2) (##< (string-cmp-ci str1 str2) 0))
(define (string-ci>? str1 str2) (##< 0 (string-cmp-ci str1 str2)))
(define (string-ci<=? str1 str2) (not (string-ci>? str1 str2)))
(define (string-ci>=? str1 str2) (not (string-ci<? str1 str2)))

(define (string-cmp str1 str2)
  (define (string-cmp-aux lst1 lst2)
    (if (pair? lst1)
      (if (pair? lst2)
        (let ((c1 (##field0 lst1)) (c2 (##field0 lst2)))
          (if (##< c1 c2)
            -1
            (if (##< c2 c1)
              1
              (string-cmp-aux (##field1 lst1) (##field1 lst2)))))
        1)
      (if (pair? lst2)
        -1
        0)))
  (string-cmp-aux (##field0 str1) (##field0 str2)))

(define (string-cmp-ci str1 str2)
  (define (string-cmp-ci-aux lst1 lst2)
      (if (pair? lst1) 
        (if (pair? lst2)
          (let ((c1 (##field0 lst1)) (c2 (##field0 lst2)))
            (if (char-ci=? c1 c2)
              (string-cmp-ci-aux (##field1 lst1) (##field1 lst2))
              (if (char-ci<? c1 c2)
                -1
                1)))
        1)
      (if (pair? lst2)
        -1
        0)))

  (string-cmp-ci-aux (string->list str1) (string->list str2)))


(define (substring str start end)

  (define (substring-aux str start end tail)
    (if (##< start end)
      (let ((i (##- end 1)))
        (substring-aux str start i (cons (list-ref (##field0 str) i) tail)))
      (##list->string tail)))

  (substring-aux str start end '()))

(define (string-append . args)
  (list->string (apply append (map string->list args))))
