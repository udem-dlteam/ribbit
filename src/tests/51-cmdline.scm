(define args (cmd-line))

(display (field1 args))
(newline)

;; (define prog-name (field0 args))
;; (write (substring prog-name (- (string-length prog-name) 15) (string-length prog-name)))
;; (write prog-name)
;; (newline)

;;;options: -l r4rs/io -l r4rs/misc 
;;;argv: --name abc --other xyz foo -b ar 12
;;;expected:
;;;("--name" "abc" "--other" "xyz" "foo" "-b" "ar" "12")
