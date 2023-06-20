(write '(a . (b . (c . (d . (e . ()))))))
(newline)

(define x (list 'a 'b 'c))
(define y x)
(write (list? y))
(newline)

(set-cdr! x 4)
(write x)
(newline)

(write (eqv? x y))
(newline)

(write '(a . (b . (c . d))))
(newline)

(write (list? y))
(newline)

(let ((x (list 'a))) (set-cdr! x x) (write (list? x)))
(newline)

(write (pair? '(a . b)))
(newline)

(write (pair? '(a . 1)))
(newline)

(write (pair? '(a b c)))
(newline)

(write (pair? '()))
(newline)

(write (pair? '#(a b)))
(newline)

(write (cons 'a '()))
(newline)

(write (cons '(a) '(b c d)))
(newline)

(write (cons "a" '(b c)))
(newline)

(write (cons 'a 3))
(newline)

(write (cons '(a b) 'c))
(newline)

(write (car '(a b c)))
(newline)

(write (car '((a) b c d)))
(newline)

(write (car '(1 . 2)))
(newline)

(write (cdr '((a) b c d)))
(newline)

(write (cdr '(1 . 2)))
(newline)

(write (list 'a (+ 3 4) 'c))
(newline)

(write (list))
(newline)

(write (length '(a b c)))
(newline)

(write (length '(a (b) (c d e))))
(newline)

(write (length '()))
(newline)

(write (append '(x) '(y)))
(newline)

(write (append '(a) '(b c d)))
(newline)

(write (append '(a (b)) '((c))))
(newline)

(write (append))
(newline)

(write (append '(a b) '(c . d)))
(newline)

(write (append '() 'a))
(newline)

(write (reverse '(a b c)))
(newline)

(write (reverse '(a (b c) d (e (f)))))
(newline)

(write (list-ref '(a b c d) 2))
(newline)

(write (memq 'a '(a b c)))
(newline)

(write (memq 'b '(a b c)))
(newline)

(write (memq 'a '(b c d)))
(newline)

(write (memq (list 'a) '(b (a) c)))
(newline)

(write (member (list 'a) '(b (a) c)))
(newline)

(write (memv 101 '(100 101 102)))
(newline)

(define e '((a 1) (b 2) (c 3)))
(write (assq 'a e))
(newline)

(write (assq 'b e))
(newline)

(write (assq 'd e))
(newline)

(write (assq (list 'a) '(((a)) ((b)) ((c)))))
(newline)

(write (assoc (list 'a) '(((a)) ((b)) ((c)))))
(newline)

(write (assv 5 '((2 3) (5 7) (11 13))))
(newline)

;;;options: -l r4rs
;;;expected:
;;;(a b c d e)
;;;#t
;;;(a . 4)
;;;#t
;;;(a b c . d)
;;;#f
;;;#f
;;;#t
;;;#t
;;;#t
;;;#f
;;;#f
;;;(a)
;;;((a) b c d)
;;;("a" b c)
;;;(a . 3)
;;;((a b) . c)
;;;a
;;;(a)
;;;1
;;;(b c d)
;;;2
;;;(a 7 c)
;;;()
;;;3
;;;3
;;;0
;;;(x y)
;;;(a b c d)
;;;(a (b) (c))
;;;()
;;;(a b c . d)
;;;a
;;;(c b a)
;;;((e (f)) d (b c) a)
;;;c
;;;(a b c)
;;;(b c)
;;;#f
;;;#f
;;;((a) c)
;;;(101 102)
;;;(a 1)
;;;(b 2)
;;;#f
;;;#f
;;;((a))
;;;(5 7)
