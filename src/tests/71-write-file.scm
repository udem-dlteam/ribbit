(define data-file (open-output-file "../../tests/data/71-output-data.txt"))

(display "output-port? ")
(display (output-port? data-file))
(newline)

(write "bonjour" data-file)

(close-output-port data-file)

(define input-port-test '())

(display "File content: ")
(call-with-input-file "../../tests/data/71-output-data.txt" 
                      (lambda (port)
                        (let loop ((ch (read-char port)))
                          (if (eof-object? ch)
                            (newline)
                            (begin (write-char ch) (loop (read-char port))))
                          (set! input-port-test port))))

(close-output-port data-file)
(close-output-port data-file)
(display "output-port-close? ")
(display (output-port-close? data-file))
(newline)

(display "input-port-close? ")
(display (input-port-close? input-port-test))
(newline)

;;;cleanup: rm ../../tests/data/71-output-data.txt
;;;options: -l io 
;;;fancy-compiler
;;;expected:
;;;output-port? #t
;;;File content: "bonjour"
;;;output-port-close? #t
;;;input-port-close? #t
