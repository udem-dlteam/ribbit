(write (symbol? 'foo))
(newline)

(write (symbol? (car '(a b))))
(newline)

(write (symbol? "bar"))
(newline)

(write (symbol? 'nil))
(newline)

(write (symbol? '()))
(newline)

(write (symbol? #f))
(newline)

;;; But first, what case are symbols in?  Determine the standard case:
(define char-standard-case char-upcase)
(if (string=? (symbol->string 'A) "a")
    (set! char-standard-case char-downcase))

(write (string=? (symbol->string 'a) (symbol->string 'A)))
(newline)

(write 
  (or (string=? (symbol->string 'a) "A")
      (string=? (symbol->string 'A) "a")))
(newline)

(define (string-standard-case s)
  (list->string (map char-standard-case (string->list s))))

(write (equal? (string-standard-case "flying-fish") (symbol->string 'flying-fish)))
(newline)

(write (equal? (string-standard-case "martin") (symbol->string 'Martin)))
(newline)

(write (symbol->string (string->symbol "Malvina")))
(newline)

(write (eq? 'a 'A))
(newline)

(define x (string #\a #\b))
(define y (string->symbol x))
(string-set! x 0 #\c)

(write x)
(newline)

(write (symbol->string y))
(newline)

(write (equal? y (string->symbol "ab")))
(newline)

(write (eq? 'mISSISSIppi 'mississippi))
(newline)

(write (eq? 'bitBlt (string->symbol "bitBlt")))
(newline)

(write (eq? 'JollyWog (string->symbol (symbol->string 'JollyWog))))
(newline)

(write (string=? "K. Harper, M.D."
                   (symbol->string (string->symbol "K. Harper, M.D."))))
(newline)

;;;options: -l r4rs
;;;expected:
;;;#t
;;;#t
;;;#f
;;;#t
;;;#f
;;;#f
;;;#t
;;;#t
;;;#t
;;;#t
;;;"Malvina"
;;;#t
;;;"cb"
;;;"ab"
;;;#t
;;;#t
;;;#f
;;;#t
;;;#t
