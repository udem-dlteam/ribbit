;;;============================================================================

;;; File: "shims.scm"

;;; Copyright (c) 1994-2022 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; This module contains various shims.

;;;============================================================================

(define (assert test msg info)
  (if (not test)
      (compiler-internal-error msg info)))

(define (compiler-internal-error msg info)
  (error msg info))

(define (list0) '())
(define (list1 a) (cons a (list0)))
(define (list2 b a) (cons b (list1 a)))
(define (list3 c b a) (cons c (list2 b a)))
(define (list4 d c b a) (cons d (list3 c b a)))
(define (list5 e d c b a) (cons e (list4 d c b a)))

(define (vector0)
  (let ((v (make-vector 0)))
    v))

(define (vector1 a)
  (let ((v (make-vector 1)))
    (vector-set! v 0 a)
    v))

(define (vector2 a b)
  (let ((v (make-vector 2)))
    (vector-set! v 0 a)
    (vector-set! v 1 b)
    v))

(define (vector3 a b c)
  (let ((v (make-vector 3)))
    (vector-set! v 0 a)
    (vector-set! v 1 b)
    (vector-set! v 2 c)
    v))

(define (vector4 a b c d)
  (let ((v (make-vector 4)))
    (vector-set! v 0 a)
    (vector-set! v 1 b)
    (vector-set! v 2 c)
    (vector-set! v 3 d)
    v))

(define (vector5 a b c d e)
  (let ((v (make-vector 5)))
    (vector-set! v 0 a)
    (vector-set! v 1 b)
    (vector-set! v 2 c)
    (vector-set! v 3 d)
    (vector-set! v 4 e)
    v))

(cond-expand

  (ribbit

   (define make-u8vector   make-vector)
   (define list->u8vector  list->vector)
   (define u8vector->list  vector->list)
   (define u8vector-length vector-length)
   (define u8vector-ref    vector-ref)
   (define u8vector-set!   vector-set!)

   (define fixnum? integer?)

   (define fx* *)
   (define fx+ +)
   (define fx- -)
   (define fx< <)
   (define fx<= <=)
   (define fx= =)
   (define fx> >)
   (define fx>= >=)
   (define fxmodulo modulo)
   (define fxquotient quotient)

   (define (fxmin x y)
     (if (< x y) x y))

   (define (fxand x y)
     (cond ((= x -1) y)
           ((= y -1) x)
           ((= x 0)  0)
           ((= y 0)  0)
           (else     (+ (if (and (odd? x) (odd? y)) 1 0)
                        (* 2 (fxand (fxarithmetic-shift-right x 1)
                                    (fxarithmetic-shift-right y 1)))))))

   (define (fxarithmetic-shift-left x y)
     (if (= y 0)
         x
         (fxarithmetic-shift-left (* x 2) (- y 1))))

   (define (fxarithmetic-shift-right x y)
     (if (= y 0)
         x
         (fxarithmetic-shift-right (quotient (if (< x 0) (- x 1) x) 2)
                                   (- y 1))))

   )

  (else))

(cond-expand

  (gambit

   (define (symbol->str symbol)
     (symbol->string symbol))

   (define (str->uninterned-symbol string)
     (string->uninterned-symbol string)))

  (else

   (define (make-table)
     (cons '() '()))

   (define (table-ref table key default)
     (let ((x (assoc key (car table))))
       (if x
           (cdr x)
           default)))

   (define (table-set! table key value)
     (let ((x (assoc key (car table))))
       (if x
           (set-cdr! x value)
           (set-car! table
                     (cons (cons key value) (car table))))))

   (define (table-length table)
     (length (car table)))

   (define (table->list table)
     (car table))

   (define uninterned-symbols (make-table))

   (define (str->uninterned-symbol string)
     (let* ((name
             (string-append "@@@" ;; use a "unique" prefix
                            (number->string
                             (table-length uninterned-symbols))))
            (sym
             (string->symbol name)))
       (table-set! uninterned-symbols sym string) ;; remember "real" name
       sym))

   (define (symbol->str symbol)
     (table-ref uninterned-symbols symbol (symbol->string symbol)))

   (define (pp obj)
     (write obj)
     (newline))))

(cond-expand

  ((and gambit (or enable-bignum disable-bignum))) ;; recent Gambit?

  (else

   (define (list-sort! compare list)

     ;; Stable mergesort algorithm

     (define (sort list len)
       (if (= len 1)
           (begin
             (set-cdr! list '())
             list)
           (let ((len1 (quotient len 2)))
             (let loop ((n len1) (tail list))
               (if (> n 0)
                   (loop (- n 1) (cdr tail))
                   (let ((x (sort tail (- len len1))))
                     (merge (sort list len1) x)))))))

     (define (merge list1 list2)
       (if (pair? list1)
           (if (pair? list2)
               (let ((x1 (car list1))
                     (x2 (car list2)))
                 (if (compare x2 x1)
                     (merge-loop list2 list2 list1 (cdr list2))
                     (merge-loop list1 list1 (cdr list1) list2)))
               list1)
           list2))

     (define (merge-loop result prev list1 list2)
       (if (pair? list1)
           (if (pair? list2)
               (let ((x1 (car list1))
                     (x2 (car list2)))
                 (if (compare x2 x1)
                     (begin
                       (set-cdr! prev list2)
                       (merge-loop result list2 list1 (cdr list2)))
                     (begin
                       (set-cdr! prev list1)
                       (merge-loop result list1 (cdr list1) list2))))
               (begin
                 (set-cdr! prev list1)
                 result))
           (begin
             (set-cdr! prev list2)
             result)))

     (let ((len (length list)))
       (if (= 0 len)
           '()
           (sort list len))))

   (define (list-sort compare list)
     (list-sort! compare (append list '())))))

;;;============================================================================
