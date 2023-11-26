(define x (string #\a #\b #\c))

(write x)
(newline)

(define y (string-append "def" x "ghi"))
(write y)
(newline)

;;;fancy-compiler
;;;options: -l r4rs/io -l r4rs/string 
;;;expected:
;;;"abc"
;;;"defabcghi"
