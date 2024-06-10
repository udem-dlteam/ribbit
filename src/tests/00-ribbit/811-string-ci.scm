(define x (string #\a #\b #\c))

(write x)
(newline)

(display (string-ci=? x (string #\A #\b #\C)))
(newline)

(display (string-ci<? x (string #\A #\b #\d)))
(newline)

(display (string-ci<? x (string #\A #\b #\A)))
(newline)

(display (string-ci>? x (string #\A #\b #\c #\d)))
(newline)

(display (string=? x (string #\A #\b #\C)))
(newline)

;;;r4rs-run: -l r4rs/io -l r4rs/string 
;;;expected:
;;;"abc"
;;;#t
;;;#t
;;;#f
;;;#f
;;;#f
