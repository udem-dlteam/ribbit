;; Equality

(display (= 1 2))
(newline)

(display (= 1 1 1 1 1))
(newline)


(display (= 1 1 1 1 2))
(newline)


(display (= 1 2))
(newline)

(display (= 2 2))
(newline)

(display "-----")
(newline)

;; Greater than

(display (> 1 2))
(newline)

(display (> 5 4 3 2 1 -10))
(newline)

(display (> 5 4 3 3 2 1 -10))
(newline)

(display "-----")
(newline)

;; Lesser than

(display (< 1 2))
(newline)

(display (< -10 1 2 3 4 5))
(newline)

(display (< -10 1 2 3 3 4 5))
(newline)

(display "-----")
(newline)

;; Lesser or equal

(display (<= 1 2))
(newline)

(display (<= 1 1))
(newline)

(display (<= -10 1 2 3 4 5))
(newline)

(display (<= -10 1 2 3 3 4 5))
(newline)

(display (<= -10 1 2 4 3 4 5))
(newline)

(display "-----")
(newline)

;; Greater or equal 

(display (>= 1 2))
(newline)

(display (>= 1 1))
(newline)

(display (>= 5 4 3 2 1 -10))
(newline)

(display (>= 5 4 3 3 2 1 -10))
(newline)

(display (>= 5 4 3 4 2 1 -10))
(newline)

;;;r4rs-run: -l r4rs/io -l r4rs/number
;;;expected:
;;;#f
;;;#t
;;;#f
;;;#f
;;;#t
;;;-----
;;;#f
;;;#t
;;;#f
;;;-----
;;;#t
;;;#t
;;;#f
;;;-----
;;;#t
;;;#t
;;;#t
;;;#t
;;;#f
;;;-----
;;;#f
;;;#t
;;;#t
;;;#t
;;;#f
