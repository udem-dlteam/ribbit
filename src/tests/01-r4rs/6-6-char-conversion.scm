(write (integer->char (char->integer #\.)))
(newline)

(write (integer->char (char->integer #\A)))
(newline)

(write (integer->char (char->integer #\a)))
(newline)

(write (char-upcase #\A))
(newline)

(write (char-upcase #\a))
(newline)

(write (char-downcase #\A))
(newline)

(write (char-downcase #\a))
(newline)

;;;options: -l r4rs
;;;expected:
;;;#\.
;;;#\A
;;;#\a
;;;#\A
;;;#\A
;;;#\a
;;;#\a
