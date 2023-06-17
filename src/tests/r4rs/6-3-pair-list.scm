(display '(a . (b . (c . (d . (e . ()))))))
(newline)

(define x (list 'a 'b 'c))
(define y x)
(display (list? y))
(newline)

(set-cdr! x 4)
(display x)
(newline)

(display (eqv? x y))
(newline)

(display '(a . (b . (c . d))))
(newline)

(display (list? y))
(newline)

(let ((x (list 'a))) (set-cdr! x x) (display (list? x)))
(newline)

(display (pair? '(a . b)))
(newline)

(display (pair? '(a . 1)))
(newline)

(display (pair? '(a b c)))
(newline)

(display (pair? '()))
(newline)

(display (pair? '#(a b)))
(newline)

(display (cons 'a '()))
(newline)

(display (cons '(a) '(b c d)))
(newline)

(display (cons "a" '(b c)))
(newline)

(display (cons 'a 3))
(newline)

(display (cons '(a b) 'c))
(newline)

(display (car '(a b c)))
(newline)

(display (car '((a) b c d)))
(newline)

(display (car '(1 . 2)))
(newline)

(display (cdr '((a) b c d)))
(newline)

(display (cdr '(1 . 2)))
(newline)

(display (list 'a (+ 3 4) 'c))
(newline)

(display (list))
(newline)

(display (length '(a b c)))
(newline)

(display (length '(a (b) (c d e))))
(newline)

(display (length '()))
(newline)

(display (append '(x) '(y)))
(newline)

(display (append '(a) '(b c d)))
(newline)

(display (append '(a (b)) '((c))))
(newline)

(display (append))
(newline)

(display (append '(a b) '(c . d)))
(newline)

(display (append '() 'a))
(newline)

(display (reverse '(a b c)))
(newline)

(display (reverse '(a (b c) d (e (f)))))
(newline)

(display (list-ref '(a b c d) 2))
(newline)

(display (memq 'a '(a b c)))
(newline)

(display (memq 'b '(a b c)))
(newline)

(display (memq 'a '(b c d)))
(newline)

(display (memq (list 'a) '(b (a) c)))
(newline)

(display (member (list 'a) '(b (a) c)))
(newline)

(display (memv 101 '(100 101 102)))
(newline)

(define e '((a 1) (b 2) (c 3)))
(display (assq 'a e))
(newline)

(display (assq 'b e))
(newline)

(display (assq 'd e))
(newline)

(display (assq (list 'a) '(((a)) ((b)) ((c)))))
(newline)

(display (assoc (list 'a) '(((a)) ((b)) ((c)))))
(newline)

(display (assv 5 '((2 3) (5 7) (11 13))))
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
