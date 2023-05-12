(define data-file (open-input-file "../../tests/data/70-input-data.txt"))

(display (input-port? data-file))
(newline)

(display "peek1: ")
(display (peek-char data-file))
(newline)

(display "peek2: ")
(display (peek-char data-file))
(newline)

(display "read1: ")
(display (read-char data-file))
(newline)

(display "read2: ")
(display (read-char data-file))
(newline)

(display "eof1: ")
(display (eof-object? (read-char data-file)))
(newline)

(display "eof2: ")
(display (eof-object? (read-char data-file)))
(newline)

(display "eof3: ")
(display (eof-object? (read-char data-file)))
(newline)

(close-input-port data-file)
(close-input-port data-file)

(display "input-port-close: ")
(display (input-port-close? data-file))
(newline)

;;;options: -l io 
;;;expected:
;;;#t
;;;peek1: 66
;;;peek2: 66
;;;read1: 66
;;;read2: 10
;;;eof1: #t
;;;eof2: #t
;;;eof3: #t
;;;input-port-close: #t
