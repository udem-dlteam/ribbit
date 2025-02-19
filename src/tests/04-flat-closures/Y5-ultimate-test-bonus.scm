(cond-expand
  (gambit
   (define putchar (lambda (x) (write-char (integer->char x))))))

(define car ##field0)
(define cdr ##field1)
(define (cons a b) (##rib a b 0))

(define (make-adder x)
  (cons (lambda (y)
          (##+ x y))
        (lambda (z)
          (set! x (##+ x z)))))

(define m (make-adder 40))
(define m.eval (car m))
(define m.add (cdr m))

(##putchar (m.eval 2)) ;; should be 42

(m.add 2) ;; internal value is now 42

(##putchar (m.eval 0));; should be 42 again

(m.add -40) ;; internal value is now 2

(##putchar (m.eval 40)) ;; should be 42 again

(##putchar 10)

;;;flat-closures-run: -l max
;;;flat-closures-run:
;;;expected:
;;;***
