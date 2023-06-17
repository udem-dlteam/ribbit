(display (symbol? 'foo))
(newline)

(display (symbol? (car '(a b))))
(newline)

(display (symbol? "bar"))
(newline)

(display (symbol? 'nil))
(newline)

(display (symbol? '()))
(newline)

(display (symbol? #f))
(newline)

;;; But first, what case are symbols in?  Determine the standard case:
(define char-standard-case char-upcase)
(if (string=? (symbol->string 'A) "a")
    (set! char-standard-case char-downcase))

(display (string=? (symbol->string 'a) (symbol->string 'A)))
(newline)

(display 
  (or (string=? (symbol->string 'a) "A")
      (string=? (symbol->string 'A) "a")))
(newline)

(define (string-standard-case s)
  (list->string (map char-standard-case (string->list s))))

(display (equal? (string-standard-case "flying-fish") (symbol->string 'flying-fish)))
(newline)

(display (equal? (string-standard-case "martin") (symbol->string 'Martin)))
(newline)

(write (symbol->string (string->symbol "Malvina")))
(newline)

(display (eq? 'a 'A))
(newline)

(define x (string #\a #\b))
(define y (string->symbol x))
(string-set! x 0 #\c)

(write x)
(newline)

(write (symbol->string y))
(newline)

(display (equal? y (string->symbol "ab")))
(newline)

(display (eq? 'mISSISSIppi 'mississippi))
(newline)

(display (eq? 'bitBlt (string->symbol "bitBlt")))
(newline)

(display (eq? 'JollyWog (string->symbol (symbol->string 'JollyWog))))
(newline)

(display (string=? "K. Harper, M.D."
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
