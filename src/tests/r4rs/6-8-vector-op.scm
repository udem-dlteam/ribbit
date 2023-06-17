(display (vector? '#(0 (2 2 2 2) "Anna")))
(newline)

(display (vector? '#()))
(newline)

(display (vector 'a 'b 'c))
(newline)

(display (vector))
(newline)

(display (vector-length '#(0 (2 2 2 2) "Anna")))
(newline)

(display (vector-length '#()))
(newline)

(display (vector-ref '#(1 1 2 3 5 8 13 21) 5))
(newline)


(display (let ((vec (vector 0 '(2 2 2 2) "Anna")))
           (vector-set! vec 1 '("Sue" "Sue"))
           vec))
(newline)

(display (make-vector 2))
(newline)

(display (make-vector 0))
(newline)

(display (make-vector 0))
(newline)

;;;options: -l r4rs
;;;expected:
;;;#t
;;;#t
;;;#(a b c)
;;;#()
;;;3
;;;0
;;;8
;;;#(0 ("Sue" "Sue") "Anna")
;;;#(0 0)
;;;#()
;;;#()
