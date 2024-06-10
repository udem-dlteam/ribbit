(define data-file (open-input-file "tests/data/70-input-data.txt"))

(write (input-port? data-file))
(newline)

(display "peek1: ")
(write (peek-char data-file))
(newline)

(display "peek2: ")
(write (peek-char data-file))
(newline)

(display "read1: ")
(write (read-char data-file))
(newline)

(display "read2: ")
(write (read-char data-file))
(newline)

(display "eof1: ")
(write (eof-object? (read-char data-file)))
(newline)

(display "eof2: ")
(write (eof-object? (read-char data-file)))
(newline)

(display "eof3: ")
(write (eof-object? (read-char data-file)))
(newline)

(close-input-port data-file)
(close-input-port data-file)

;;;r4rs-run: -l r4rs/io 
;;;expected:
;;;#t
;;;peek1: #\B
;;;peek2: #\B
;;;read1: #\B
;;;read2: #\newline
;;;eof1: #t
;;;eof2: #t
;;;eof3: #t
