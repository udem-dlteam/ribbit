(define f (make-string 3 #\*))

(write (begin (string-set! f 0 #\?) f))
(newline)

(write (string #\a #\b #\c))
(newline)

(write (string))
(newline)

(write (string-length "abc"))
(newline)

(write (string-ref "abc" 0))
(newline)

(write (string-ref "abc" 2))
(newline)

(write (string-length ""))
(newline)

(write (substring "ab" 0 0))
(newline)

(write (substring "ab" 1 1))
(newline)

(write (substring "ab" 2 2))
(newline)

(write (substring "ab" 0 1))
(newline)

(write (substring "ab" 1 2))
(newline)

(write (substring "ab" 0 2))
(newline)

(write (string-append "foo" "bar"))
(newline)

(write (string-append "foo"))
(newline)

(write (string-append "foo" ""))
(newline)

(write (string-append "" "foo"))
(newline)

(write (string-append))
(newline)

(write (make-string 0))
(newline)

;;;r4rs-run: -l r4rs
;;;expected:
;;;"?**"
;;;"abc"
;;;""
;;;3
;;;#\a
;;;#\c
;;;0
;;;""
;;;""
;;;""
;;;"a"
;;;"b"
;;;"ab"
;;;"foobar"
;;;"foo"
;;;"foo"
;;;"foo"
;;;""
;;;""
