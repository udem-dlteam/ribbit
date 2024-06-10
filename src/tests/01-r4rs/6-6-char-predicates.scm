(display (eqv? '#\  #\Space))
(newline)

(display (eqv? #\space '#\Space))
(newline)

(display (char? #\a))
(newline)

(display (char? #\())
(newline)

(display (char? #\ ))
(newline)

(display (char? '#\newline))
(newline)

(display (char-alphabetic? #\a))
(newline)

(display (char-alphabetic? #\A))
(newline)

(display (char-alphabetic? #\z))
(newline)

(display (char-alphabetic? #\Z))
(newline)

(display (char-alphabetic? #\0))
(newline)

(display (char-alphabetic? #\9))
(newline)

(display (char-alphabetic? #\space))
(newline)

(display (char-alphabetic? #\;))
(newline)

(display (char-numeric? #\a))
(newline)

(display (char-numeric? #\A))
(newline)

(display (char-numeric? #\z))
(newline)

(display (char-numeric? #\Z))
(newline)

(display (char-numeric? #\0))
(newline)

(display (char-numeric? #\9))
(newline)

(display (char-numeric? #\space))
(newline)

(display (char-numeric? #\;))
(newline)

(display (char-whitespace? #\a))
(newline)

(display (char-whitespace? #\A))
(newline)

(display (char-whitespace? #\z))
(newline)

(display (char-whitespace? #\Z))
(newline)

(display (char-whitespace? #\0))
(newline)

(display (char-whitespace? #\9))
(newline)

(display (char-whitespace? #\space))
(newline)

(display (char-whitespace? #\;))
(newline)

(display (char-upper-case? #\0))
(newline)

(display (char-upper-case? #\9))
(newline)

(display (char-upper-case? #\space))
(newline)

(display (char-upper-case? #\;))
(newline)

(display (char-lower-case? #\0))
(newline)

(display (char-lower-case? #\9))
(newline)

(display (char-lower-case? #\space))
(newline)

(display (char-lower-case? #\;))
(newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
;;;#t
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
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#t
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
;;;#f
