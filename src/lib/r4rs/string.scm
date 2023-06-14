(##include-once "./types.scm")
(##include-once "./bool.scm")
(##include-once "./pair-list.scm")
(##include-once "./control.scm")
(##include-once "./char.scm")

;; Strings (R4RS section 6.7).

(define (string-length x) (##field1 x))

(define (string-ref str i) (integer->char (list-ref (##field0 str) i)))
(define (string-set! str i ch) (list-set! str i (char->integer ch)))

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
        (let ((c1 (car lst1)))
          (let ((c2 (car lst2)))
            (if (##< c1 c2)
              -1
              (if (##< c2 c1)
                1
                (string-cmp-aux (cdr lst1) (cdr lst2))))))
        1)
      (if (pair? lst2)
        -1
        0)))
  (string-cmp-aux (##string->list str1) (##string->list str2)))

(define (string-cmp-ci str1 str2)
  (define (string-cmp-ci-aux lst1 lst2)
    (cond 
      ((and (pair? lst1) (pair? lst2))
        (if (char-ci=? (car lst1) (car lst2))
          (string-cmp-ci-aux (cdr lst1) (cdr lst2))
          (if (char-ci<? (car lst1) (car lst2))
            -1
            1)))
      ((pair? lst1) 1)
      ((pair? lst2) -1)
      (else 0)))

  (string-cmp-ci-aux (string->list str1) (string->list str2)))


(define (substring str start end)

  (define (substring-aux str start end tail)
    (if (##< start end)
      (let ((i (##- end 1)))
        (substring-aux str start i (cons (list-ref (##field0 str) i) tail)))
      (##list->string tail)))

  (substring-aux str start end '()))


(define (string-append . args)
  (##list->string (apply append (map ##string->list args))))
