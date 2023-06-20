;
(define this-file (open-input-file "../../tests/r4rs/6-10-2-input.scm"))
(write (peek-char this-file))
(newline)

(write (read-char this-file))
(newline)

(display (read this-file))
(newline)

(write (read-char this-file))
(newline)

(write (peek-char this-file))
(newline)

(display (read this-file))
(newline)

(close-input-port this-file)
(close-input-port this-file)

;;;options: -l r4rs
;;;expected:
;;;#\;
;;;#\;
;;;(DEFINE THIS-FILE (OPEN-INPUT-FILE "../../tests/r4rs/6-10-2-input.scm"))
;;;#\newline
;;;#\(
;;;(WRITE (PEEK-CHAR THIS-FILE))
