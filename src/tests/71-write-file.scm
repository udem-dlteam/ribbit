(define data-file (open-output-file "../../tests/data/70-output-data.txt"))

(display (output-port? data-file))
(newline)

(write "bonjour" data-file)

(call-with-input-file "../../tests/data/70-output-data.txt" 
                      (lambda (port)
                        (let loop ((ch (read-char port)))
                          (if (eof-object? ch)
                            (newline)
                            (begin (write-char ch) (loop (read-char port))))
                        )))

(close-output-port data-file)
(close-output-port data-file)
(display (output-port-close? data-file))
(newline)

;;;cleanup: rm ../../tests/data/70-output-data.txt
;;;options: -l io 
;;;expected:
;;;#t
;;;"bonjour"
;;;#t
