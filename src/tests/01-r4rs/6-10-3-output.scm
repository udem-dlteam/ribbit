(define write-test-obj
  '(#t #f a () 9739 -3 . #((test) "te \" \" st" "" test #() b c)))

(define load-test-obj
  (list 'define 'foo (list 'quote write-test-obj)))

(define test-file (open-output-file "./tmp2"))
(write-char #\; test-file)
(display #\; test-file)
(display ";" test-file)
(write write-test-obj test-file)
(newline test-file)
(write load-test-obj test-file)
(display (output-port? test-file))
(newline)
(close-output-port test-file)

;; test
(call-with-input-file 
  "./tmp2"
  (lambda (port)
    (write (read-char port))
    (newline)

    (write (read-char port))
    (newline)

    (write (read-char port))
    (newline)

    (display (equal? write-test-obj (read port)))
    (newline)

    (display (equal? load-test-obj (read port)))
    (newline)))

;;;r4rs-run: -l r4rs
;;;cleanup: rm ./tmp2
;;;expected:
;;;#t
;;;#\;
;;;#\;
;;;#\;
;;;#t
;;;#t
