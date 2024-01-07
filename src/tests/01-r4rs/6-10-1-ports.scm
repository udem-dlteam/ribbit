(display (input-port? (current-input-port)))
(newline)

(display (output-port? (current-output-port)))
(newline)

(display (call-with-input-file "tests/01-r4rs/6-10-1-ports.scm" input-port?))
(newline)

(define this-file (open-input-file "tests/01-r4rs/6-10-1-ports.scm"))
(display (input-port? this-file))
(newline)

;;;options: -l r4rs
;;;expected:
;;;#t
;;;#t
;;;#t
;;;#t
