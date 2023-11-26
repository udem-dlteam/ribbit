(define data-file (open-input-file "tests/data/72-read-data.txt"))

(display (input-port? data-file))
(newline)

(write (read data-file))
(newline)

(close-input-port data-file)
(close-input-port data-file)

;;;options: -l r4rs/io 
;;;fancy-compiler
;;;expected:
;;;#t
;;;("abc" 1 (1 2 3) #f #t (list "xyz" 1 2))
