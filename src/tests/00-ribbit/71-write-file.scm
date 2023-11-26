(define data-file (open-output-file "tests/data/71-output-data.txt"))

(display "output-port? ")
(display (output-port? data-file))
(newline)

(write "bonjour" data-file)

(close-output-port data-file)

(define input-port-test '())

(display "File content: ")
(call-with-input-file "tests/data/71-output-data.txt" 
                      (lambda (port)
                        (let loop ((ch (read-char port)))
                          (if (eof-object? ch)
                            (newline)
                            (begin (write-char ch) (loop (read-char port))))
                          (set! input-port-test port))))

(close-output-port data-file)
(close-output-port data-file)

;;;cleanup: rm ../../tests/data/71-output-data.txt
;;;options: -l r4rs/io 
;;;fancy-compiler
;;;expected:
;;;output-port? #t
;;;File content: "bonjour"
