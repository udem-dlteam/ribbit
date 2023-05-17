(##include "./types.scm")

;; Pairs and lists (R4RS section 6.3).

(define (cons car cdr) (rib car cdr pair-type))
(define car field0)
(define cdr field1)
(define set-car! field0-set!)
(define set-cdr! field1-set!)

(define (cadr pair) (field0 (field1 pair)))
(define (cddr pair) (field1 (field1 pair)))
(define (caddr pair) (cadr (field1 pair)))
(define (cadddr pair) (caddr (field1 pair)))

(define (caar pair) (field0 (field0 pair)))
(define (cdar pair) (field1 (field0 pair)))

(define (caaar pair) (caar (field0 pair)))
(define (caadr pair) (caar (field1 pair)))
(define (cadar pair) (cadr (field0 pair)))
(define (cdaar pair) (cdar (field0 pair)))
(define (cdadr pair) (cdar (field1 pair)))
(define (cddar pair) (cddr (field0 pair)))
(define (cdddr pair) (cddr (field1 pair)))

(define (caaaar pair) (caaar (field0 pair)))
(define (caaadr pair) (caaar (field1 pair)))
(define (caadar pair) (caadr (field0 pair)))
(define (caaddr pair) (caadr (field1 pair)))
(define (cadaar pair) (cadar (field0 pair)))
(define (cadadr pair) (cadar (field1 pair)))
(define (caddar pair) (caddr (field0 pair)))
(define (cdaaar pair) (cdaar (field0 pair)))
(define (cdaadr pair) (cdaar (field1 pair)))
(define (cdadar pair) (cdadr (field0 pair)))
(define (cdaddr pair) (cdadr (field1 pair)))
(define (cddaar pair) (cddar (field0 pair)))
(define (cddadr pair) (cddar (field1 pair)))
(define (cdddar pair) (cdddr (field0 pair)))
(define (cddddr pair) (cdddr (field1 pair)))


(define (list . args) args)

(define (length lst)
  (if (pair? lst)
      (+ 1 (length (cdr lst)))
      0))

(define (append lst . lsts)
  (define (append2 lst1 lst2)
    (if (pair? lst1)
      (cons (car lst1) (append2 (cdr lst1) lst2))
      lst2))

  (define (append-aux lsts)
    (if (pair? lsts)
      (append2 (car lsts) (append-aux (cdr lsts)))
      lsts))

  (append-aux (cons lst lsts)))

(define (reverse lst)
  (reverse-aux lst '()))

(define (reverse-aux lst result)
  (if (pair? lst)
      (reverse-aux (cdr lst) (cons (car lst) result))
      result))

(define (list-ref lst i)
  (car (list-tail lst i)))

(define (list-set! lst i x)
  (set-car! (list-tail lst i) x))

(define (list-tail lst i)
  (if (< 0 i)
      (list-tail (cdr lst) (- i 1))
      lst))

(define (memv x lst)
  (if (pair? lst)
      (if (eqv? x (car lst))
          lst
          (memv x (cdr lst)))
      #f))

(define memq memv)

(define (member x lst)
  (if (pair? lst)
      (if (equal? x (car lst))
          lst
          (member x (cdr lst)))
      #f))

(define (assv x lst)
  (if (pair? lst)
      (let ((couple (car lst)))
        (if (eqv? x (car couple))
            couple
            (assv x (cdr lst))))
      #f))

(define assq assv)

(define (assoc x lst)
  (if (pair? lst)
      (let ((couple (car lst)))
        (if (equal? x (car couple))
            couple
            (assoc x (cdr lst))))
      #f))

(define (make-list k fill)
  (make-list-aux k fill '()))

(define (make-list-aux k fill lst)
  (if (< 0 k)
      (make-list-aux (- k 1) fill (cons fill lst))
      lst))

