(write (number->string 3 2))
(newline)

(write (number->string 255 16))
(newline)

(write (string->number "11" 2))
(newline)

(write (string->number "1f" 16))
(newline)

(write (string->number "1F" 16))
(newline)

(write (string->number "28" 8))
(newline)

;;;fancy-compiler
;;;options: -l r4rs/io -l r4rs/types
;;;expected:
;;;"11"
;;;"FF"
;;;3
;;;21
;;;21
;;;24
