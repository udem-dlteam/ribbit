(define data-file (open-input-file "../../tests/data/70-input-data.txt"))

(display (input-port? data-file))
(newline)

(display (read-char data-file))
(newline)

(display (peek-char data-file))
(newline)

(display (read-char data-file))
(newline)

(display (eof-object? (read-char data-file)))
(newline)

(close-input-port data-file)
(close-input-port data-file)

(display (input-port-close? data-file))
(newline)

;;;options: -l io 
;;;expected:
;;;#t
;;;66
;;;66
;;;10
;;;#t
;;;#t
