(##include-once "./types.scm")

;; Pairs and lists (R4RS section 6.3).

(define (cons car cdr) (##rib car cdr pair-type))
(define (car x) (##field0 x))
(define (cdr x) (##field1 x))
(define (set-car! x) (##field0-set! x))
(define (set-cdr! x) (##field1-set! x))

(define (cadr pair) (##field0 (##field1 pair)))
(define (cddr pair) (##field1 (##field1 pair)))
(define (caddr pair) (cadr (##field1 pair)))
(define (cadddr pair) (caddr (##field1 pair)))

(define (caar pair) (##field0 (##field0 pair)))
(define (cdar pair) (##field1 (##field0 pair)))

(define (caaar pair) (caar (##field0 pair)))
(define (caadr pair) (caar (##field1 pair)))
(define (cadar pair) (cadr (##field0 pair)))
(define (cdaar pair) (cdar (##field0 pair)))
(define (cdadr pair) (cdar (##field1 pair)))
(define (cddar pair) (cddr (##field0 pair)))
(define (cdddr pair) (cddr (##field1 pair)))

(define (caaaar pair) (caaar (##field0 pair)))
(define (caaadr pair) (caaar (##field1 pair)))
(define (caadar pair) (caadr (##field0 pair)))
(define (caaddr pair) (caadr (##field1 pair)))
(define (cadaar pair) (cadar (##field0 pair)))
(define (cadadr pair) (cadar (##field1 pair)))
(define (caddar pair) (caddr (##field0 pair)))
(define (cdaaar pair) (cdaar (##field0 pair)))
(define (cdaadr pair) (cdaar (##field1 pair)))
(define (cdadar pair) (cdadr (##field0 pair)))
(define (cdaddr pair) (cdadr (##field1 pair)))
(define (cddaar pair) (cddar (##field0 pair)))
(define (cddadr pair) (cddar (##field1 pair)))
(define (cdddar pair) (cdddr (##field0 pair)))
(define (cddddr pair) (cdddr (##field1 pair)))


(define (list . args) args)

(define (length lst)
  (if (pair? lst)
      (##+ 1 (length (##field1 lst)))
      0))

(define (append . lsts)
  (define (append-aux lsts)
    (if (pair? lsts)
        (let ((lst (##field0 lsts)))
          (if (pair? lst)
              (cons (##field0 lst) (append-aux (cons (##field1 lst) (##field1 lsts))))
              (if (null? (##field1 lsts))
                  (##field0 lsts)
                  (append-aux (##field1 lsts)))))
        '()))
  (append-aux lsts))

(define (reverse lst)
  (reverse-aux lst '()))

(define (reverse-aux lst result)
  (if (pair? lst)
      (reverse-aux (##field1 lst) (cons (##field0 lst) result))
      result))

(define (list-ref lst i)
  (##field0 (list-tail lst i)))

(define (list-set! lst i x)
  (##field0-set! (list-tail lst i) x))

(define (list-tail lst i)
  (if (##< 0 i)
      (list-tail (##field1 lst) (##- i 1))
      lst))

(define (memv x lst)
  (if (pair? lst)
      (if (eqv? x (##field0 lst))
          lst
          (memv x (##field1 lst)))
      #f))

(define memq memv)

(define (member x lst)
  (if (pair? lst)
      (if (equal? x (##field0 lst))
          lst
          (member x (##field1 lst)))
      #f))

(define (assv x lst)
  (if (pair? lst)
      (let ((couple (##field0 lst)))
        (if (eqv? x (##field0 couple))
            couple
            (assv x (##field1 lst))))
      #f))

(define assq assv)

(define (assoc x lst)
  (if (pair? lst)
      (let ((couple (##field0 lst)))
        (if (equal? x (##field0 couple))
            couple
            (assoc x (##field1 lst))))
      #f))

(define (make-list k fill)
  (make-list-aux k fill '()))

(define (make-list-aux k fill lst)
  (if (##< 0 k)
      (make-list-aux (##- k 1) fill (cons fill lst))
      lst))
