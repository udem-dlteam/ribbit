;
(define this-file (open-input-file "../../tests/r4rs/6-10-2-input.scm"))
(write (peek-char this-file))
(newline)

(write (read-char this-file))
(newline)

(write (read this-file))
(newline)

(write (read-char this-file))
(newline)

(write (peek-char this-file))
(newline)

(write (read this-file))
(newline)

(close-input-port this-file)
(close-input-port this-file)

;;;options: -l r4rs
;;;expected:
;;;#\;
;;;#\;
;;;(define this-file (open-input-file "../../tests/r4rs/6-10-2-input.scm"))
;;;#\newline
;;;#\(
;;;(write (peek-char this-file))
