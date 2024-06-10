(write (number->string 0))
(newline)

(write (number->string 100))
(newline)

(write (number->string 256 16))
(newline)

(display (string->number "100"))
(newline)

(display (string->number "100" 16))
(newline)

(display (string->number ""))
(newline)

(display (string->number "."))
(newline)

(display (string->number "d"))
(newline)

(display (string->number "D"))
(newline)

(display (string->number "i"))
(newline)

(display (string->number "I"))
(newline)

(display (string->number "3i"))
(newline)

(display (string->number "3I"))
(newline)

(display (string->number "33i"))
(newline)

(display (string->number "33I"))
(newline)

(display (string->number "3.3i"))
(newline)

(display (string->number "3.3I"))
(newline)

(display (string->number "-"))
(newline)

(display (string->number "+"))
(newline)

(display (or (not (string->number "8000000" 16))
             (positive? (string->number "8000000" 16))))
(newline)

(display (or (not (string->number "-8000000" 16))
             (negative? (string->number "-8000000" 16))))
(newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;"0"
;;;"100"
;;;"100"
;;;100
;;;256
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#t
;;;#t
