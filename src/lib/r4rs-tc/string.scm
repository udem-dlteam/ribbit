(##include-once "./bool.scm")
(##include-once "./types.scm")
(##include-once "./pair-list.scm")
(##include-once "./char.scm")
(##include-once "./number.scm")
(##include-once "./control.scm")

;; Strings (R4RS section 6.7).

(define (string-length x) (##field1 x))

(define-signature
  string-length
  ((x
     guard: (string? x)
     expected: "STRING")))



(define (string-ref str i) (integer->char (list-ref (##field0 str) i)))

(define-signature
  string-ref
  ((str 
     guard: (string? str)
     expected: "STRING")
   (i
     guard: (< -1 i (##field1 str))
     expected: (string-append "A NUMBER between 0 and " (number->string (string-length str))))))


(define (string-set! str i ch) (list-set! (##field0 str) i (##field0 ch)))

(define-signature
  string-set!
  ((str 
     guard: (string? str)
     expected: "STRING")
   (i
     guard: (and (integer? i) (< -1 i (##field1 str)))
     expected: (string-append "INTEGER between 0 and " (number->string (string-length str))))
   (ch
     guard: (char? ch)
     expected: "CHARACTER")))



(define (make-string k ch) (list->string (make-list k ch)))

(define-signature
  make-string
  ((k
     guard: (integer? k)
     expected: "INTEGER")
   (ch
     default: #\space
     guard: (char? ch)
     expected: "CHARACTER")))




(define (string . chars) (list->string chars))

(define-signature
  string
  ((chars 
     rest-param:
     guard: (all char? chars)
     expected: "CHARACTERs")))


(define (string=? str1 str2) (equal? str1 str2))
(define (string<? str1 str2) (##< (string-cmp str1 str2) 0))
(define (string>? str1 str2) (##< 0 (string-cmp str1 str2)))

(define (string<=? str1 str2) (not (string>? str1 str2)))
(define (string>=? str1 str2) (not (string<? str1 str2)))

(define (string-ci=? str1 str2) (##eqv? (string-cmp-ci str1 str2) 0))
(define (string-ci<? str1 str2) (##< (string-cmp-ci str1 str2) 0))
(define (string-ci>? str1 str2) (##< 0 (string-cmp-ci str1 str2)))
(define (string-ci<=? str1 str2) (not (string-ci>? str1 str2)))
(define (string-ci>=? str1 str2) (not (string-ci<? str1 str2)))

(define-signatures
  (string=? string<? string>? string<=? string>=? string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
  ((str1 
     guard: (string? str1)
     expected: "STRING")
   (str2 
     guard: (string? str2)
     expected: "STRING")))



(define (substring str start end)

  (define (substring-aux str start end tail)
    (if (##< start end)
      (let ((i (##- end 1)))
        (substring-aux str start i (cons (list-ref (##field0 str) i) tail)))
      (##list->string tail)))

  (substring-aux str start end '()))

(define-signature
  substring
  ((str 
     guard: (string? str)
     expected: "STRING")
   (start 
     guard: (and (integer? start) (<= 0 start end))
     expected: (string-append "INTEGER between 0 and the end value (" (number->string end) ")"))
   (end 
     guard: (and (integer? end) (<= end (string-length str)))
     expected: (string-append "INTEGER between the start value (" 
                              (number->string start) ") and " 
                              (number->string  (string-length str))))))



(define (string-append . strs)
  (list->string (apply append (map string->list strs))))

(define-signature
  string-append
  ((strs 
     rest-param:
     guard: (all string? strs)
     expected: "STRINGs")))

;; ---------------------- UTILS NOT IN R4RS ---------------------- ;;

(define (string-cmp str1 str2)
  (define (string-cmp-aux lst1 lst2)
    (if (pair? lst1)
      (if (pair? lst2)
        (let ((c1 (##field0 lst1)) (c2 (##field0 lst2)))
          (if (##< c1 c2)
            -1
            (if (##< c2 c1)
              1
              (string-cmp-aux (##field1 lst1) (##field1 lst2)))))
        1)
      (if (pair? lst2)
        -1
        0)))
  (string-cmp-aux (##field0 str1) (##field0 str2)))

(define (string-cmp-ci str1 str2)
  (define (string-cmp-ci-aux lst1 lst2)
      (if (pair? lst1) 
        (if (pair? lst2)
          (let ((c1 (##field0 lst1)) (c2 (##field0 lst2)))
            (if (char-ci=? c1 c2)
              (string-cmp-ci-aux (##field1 lst1) (##field1 lst2))
              (if (char-ci<? c1 c2)
                -1
                1)))
        1)
      (if (pair? lst2)
        -1
        0)))

  (string-cmp-ci-aux (string->list str1) (string->list str2)))

(define (string-upcase str)
  (list->string (apply map `(,char-upcase ,@(string->list str)))))

(define (string-downcase str)
  (list->string (apply map `(,char-downcase ,@(string->list str)))))

(define (string-at? str sub-str start end)
  (let ((sub-str-len (string-length sub-str))
        (str-len (string-length str)))
    (and (>= str-len sub-str-len)
         (string=? (substring str start end) sub-str))))

(define (string-suffix? suffix str)
  (string-at? str suffix (- (string-length str) (string-length suffix)) (string-length str)))

(define (string-prefix? prefix str)
  (string-at? str prefix 0 (string-length prefix)))

(define (string-contains str sub-str)
  (let ((str-len (string-length str))
        (sub-str-len (string-length sub-str)))
    (let loop ((i 0))
      (and (<= i (- str-len sub-str-len))
           (or (string-at? str sub-str i sub-str-len)
               (loop (+ 1 i)))))))

(define (string-find str sub-str)
  (let ((str-len (string-length str))
        (sub-str-len (string-length sub-str)))
    (let loop ((i 0))
      (and (<= i (- str-len sub-str-len))
           (if (string-at? str sub-str i sub-str-len)
             i
             (loop (+ 1 i)))))))

(define (string-concatenate list-str (sep ""))
  (if (null? list-str)
    ""
    (let loop ((final (##field0 list-str)) (s (##field1 list-str)))
      (if (null? s)
        final 
        (loop (string-append final sep (##field0 s)) (##field1 s))))))

;; Pattern can be a char or a procedure
(define (string-split str (pattern char-whitespace?))
  (let ((pattern? (if (char? pattern) (lambda (c) (char=? c pattern)) pattern))
        (final '("")))
    (for-each 
      (lambda (c)
        (if (pattern? c)
          (set! final (cons "" final))
          (set! final (cons (string-append (car final) (string c)) (cdr final)))))
      (string->list str))
    (reverse final)))
