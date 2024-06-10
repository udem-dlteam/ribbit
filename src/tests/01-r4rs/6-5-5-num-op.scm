(display (+ 1 2))
(newline)

(display (+ 5))
(newline)

(display (+ 1 2 3))
(newline)

(display (+))
(newline)

(newline)

(display (* 1 2))
(newline)

(display (* 5))
(newline)

(display (* 1 2 3))
(newline)

(display (*))
(newline)

(newline)

(display (- 1 2))
(newline)

(display (- 5))
(newline)

(display (- -5))
(newline)

(newline)

(display (/ 9 2))
(newline)

(display (/ 5))
(newline)

(display (/ 42 -3))
(newline)

(display (/ -5))
(newline)

(display (+ 3 4))
(newline)

(display (+ 3))
(newline)

(display (+))
(newline)

(display (* 4))
(newline)

(display (*))
(newline)


(display (- 3 4))
(newline)

(display (- 3))
(newline)

(display (abs -7))
(newline)

(display (abs 7))
(newline)

(display (abs 0))
(newline)


(display (quotient 35 7))
(newline)

(display (quotient -35 7))
(newline)

(display (quotient 35 -7))
(newline)

(display (quotient -35 -7))
(newline)

(display (modulo 13 4))
(newline)

(display (remainder 13 4))
(newline)

(display (modulo -13 4))
(newline)

(display (remainder -13 4))
(newline)

(display (modulo 13 -4))
(newline)

(display (remainder 13 -4))
(newline)

(display (modulo -13 -4))
(newline)

(display (remainder -13 -4))
(newline)

(display (modulo 0 86400))
(newline)

(display (modulo 0 -86400))
(newline)

(define (divtest n1 n2)
	(= n1 (+ (* n2 (quotient n1 n2))
		 (remainder n1 n2))))

(display (divtest 238 9))
(newline)

(display (divtest -238 9))
(newline)

(display (divtest 238 -9))
(newline)

(display (divtest -238 -9))
(newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;3
;;;5
;;;6
;;;0
;;;
;;;2
;;;5
;;;6
;;;1
;;;
;;;-1
;;;-5
;;;5
;;;
;;;4
;;;0
;;;-14
;;;0
;;;7
;;;3
;;;0
;;;4
;;;1
;;;-1
;;;-3
;;;7
;;;7
;;;0
;;;5
;;;-5
;;;-5
;;;5
;;;1
;;;1
;;;3
;;;-1
;;;-3
;;;1
;;;-1
;;;-1
;;;0
;;;0
;;;#t
;;;#t
;;;#t
;;;#t
