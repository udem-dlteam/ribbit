(define data-file (open-input-file "../../tests/data/72-read-data.txt"))

(display (input-port? data-file))
(newline)

(write (read data-file))
(newline)

(close-input-port data-file)
(close-input-port data-file)

(display (input-port-close? data-file))
(newline)

;;;options: -l io 
;;;expected:
;;;#t
;;;"abc"
;;;#t
