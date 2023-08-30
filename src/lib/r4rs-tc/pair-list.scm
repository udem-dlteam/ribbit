(##include-once "./types.scm")

;; Pairs and lists (R4RS section 6.3).

(define (cons car cdr) (##rib car cdr pair-type))
(define (car x) (##field0 x))
(define (cdr x) (##field1 x))

(define-signatures
  (car cdr)
  ((x
     guard: (pair? x)
     expected: "PAIR")))


(define (set-car! x car) (##field0-set! x car))
(define (set-cdr! x cdr) (##field1-set! x cdr))

(define-signatures
  (set-car! set-cdr!)
  ((x
     guard: (pair? x)
     expected: "PAIR")
   (value)))



(define (cadr pair) (car (cdr pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caddr pair) (cadr (cdr pair)))
(define (cadddr pair) (caddr (cdr pair)))

(define (caar pair) (car (car pair)))
(define (cdar pair) (cdr (car pair)))

(define (caaar pair) (caar (car pair)))
(define (caadr pair) (caar (cdr pair)))
(define (cadar pair) (cadr (car pair)))
(define (cdaar pair) (cdar (car pair)))
(define (cdadr pair) (cdar (cdr pair)))
(define (cddar pair) (cddr (car pair)))
(define (cdddr pair) (cddr (cdr pair)))

(define (caaaar pair) (caaar (car pair)))
(define (caaadr pair) (caaar (cdr pair)))
(define (caadar pair) (caadr (car pair)))
(define (caaddr pair) (caadr (cdr pair)))
(define (cadaar pair) (cadar (car pair)))
(define (cadadr pair) (cadar (cdr pair)))
(define (caddar pair) (caddr (car pair)))
(define (cdaaar pair) (cdaar (car pair)))
(define (cdaadr pair) (cdaar (cdr pair)))
(define (cdadar pair) (cdadr (car pair)))
(define (cdaddr pair) (cdadr (cdr pair)))
(define (cddaar pair) (cddar (car pair)))
(define (cddadr pair) (cddar (cdr pair)))
(define (cdddar pair) (cdddr (car pair)))
(define (cddddr pair) (cdddr (cdr pair)))


(define (list . args) args)

(define (length lst)
  (if (pair? lst)
      (##+ 1 (length (##field1 lst)))
      0))

(define (reverse lst)
  (define (reverse-aux lst result)
    (if (pair? lst)
      (reverse-aux (##field1 lst) (cons (##field0 lst) result))
      result))
  (reverse-aux lst '()))


(define-signatures
  (length reverse)
  ((lst 
     guard: (list? lst)
     expected: "LIST")))



(define (append . lsts)
  (define (append-aux lsts)
    (if (pair? lsts)
        (let ((lst (##field0 lsts)))
          (if (pair? lst)
              (cons (##field0 lst) (append-aux (cons (##field1 lst) (##field1 lsts))))
              (if (null? (##field1 lsts))
                  lst
                  (append-aux (##field1 lsts)))))
        '()))
  (append-aux lsts))

(define-signature 
  append
  ((lst 
     rest-param:
     guard: (or (null? lst) (all list? (cdr (reverse lst))))
     expected: "All LISTs except the last arg")))


(define (list-ref lst i)
  (##field0 (##ntc-list-tail lst i)))

(define (list-set! lst i x)
  (##field0-set! (list-tail lst i) x))

(define (list-tail lst i)
  (if (##< 0 i)
      (##ntc-list-tail (##field1 lst) (##- i 1))
      lst))

(define-signatures
  (list-ref list-tail)
  ((lst 
     guard: (list? lst)
     expected: "LIST")
   (i
     guard: (and (integer? i) (< -1 i (length lst)))
     expected: (string-append "INTEGER between 0 and " (number->string (length lst))))))



(define (memv x lst)
  (if (pair? lst)
      (if (eqv? x (##field0 lst))
          lst
          (memv x (##field1 lst)))
      #f))

(define (memq x lst) (memv x lst))

(define ##case-memv memv)

(define (member x lst)
  (if (pair? lst)
      (if (equal? x (##field0 lst))
          lst
          (member x (##field1 lst)))
      #f))

(define-signatures
  (member memv memq)
  ((x)
   (lst 
     guard: (list? lst)
     expected: "LIST")))



(define (assv x lst)
  (if (pair? lst)
      (let ((couple (##field0 lst)))
        (if (eqv? x (##field0 couple))
            couple
            (assv x (##field1 lst))))
      #f))

(define (assq x lst) (assv x lst))

(define (assoc x lst)
  (if (pair? lst)
      (let ((couple (##field0 lst)))
        (if (equal? x (##field0 couple))
            couple
            (assoc x (##field1 lst))))
      #f))

(define-signatures
  (assoc assq assv)
  ((x)
   (lst 
     guard: (and (list? lst) (all pair? lst))
     expected: "LIST of PAIRs")))



(define (make-list k fill)
  (make-list-aux k fill '()))

(define (make-list-aux k fill lst)
  (if (##< 0 k)
      (make-list-aux (##- k 1) fill (cons fill lst))
      lst))


;; -------------------- QUASIQUOTES -------------------- ;;

(define ##qq-list list)
(define (##qq-list->vector l) (list->vector l))
(define ##qq-cons cons)
(define ##qq-append append)

