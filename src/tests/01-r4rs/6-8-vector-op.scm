(write (vector? '#(0 (2 2 2 2) "Anna")))
(newline)

(write (vector? '#()))
(newline)

(write (vector 'a 'b 'c))
(newline)

(write (vector))
(newline)

(write (vector-length '#(0 (2 2 2 2) "Anna")))
(newline)

(write (vector-length '#()))
(newline)

(write (vector-ref '#(1 1 2 3 5 8 13 21) 5))
(newline)


(write (let ((vec (vector 0 '(2 2 2 2) "Anna")))
           (vector-set! vec 1 '("Sue" "Sue"))
           vec))
(newline)

(write (make-vector 2))
(newline)

(write (make-vector 0))
(newline)

(write (make-vector 0))
(newline)

;;;r4rs-run: -l r4rs
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
