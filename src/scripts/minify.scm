#!/usr/bin/env gsi

(define ident-chars #f)
(define substitutions '())
(define comment-char1 #f)
(define comment-char2 #f)
(define keep-spaces? #f)
(define keep-ident-case? #f)
(define prefix "")

(define (minify)

  (define taken (map cdr substitutions))
  (define names (make-table))

  (define output '())

  (define (add-output x)
    (set! output (cons output x)))

  (define (output-name name)
    (add-output
     (let ((x (table-ref names name #f)))
       (if x
           (begin
             (set-car! x (+ (car x) 1))
             (cdr x))
           (let ((x (list 1 name)))
             (table-set! names name x)
             (cdr x))))))

  (define (output-text text)
    (add-output text))

  (define (output-object obj)
    (add-output (object->string obj)))

  (define (parse)
    (let loop ((token #f))

      (define (end-token)
        (if (and token (not (null? token)))
            (let ((str (list->string (reverse token))))
              (if (or (string->number str) (string=? str "#"))
                  (output-text str)
                  (output-name str))
              '())
            token))

      (define (skip-line)
        (let ((c (peek-char)))
          (if (not (or (eof-object? c) (char=? c #\newline)))
              (begin
                (read-char)
                (skip-line)))))

      (let ((c (peek-char)))
        (cond ((eof-object? c)
               (end-token))
              ((char<=? c #\space)
               (let ((t (end-token)))
                 (if keep-spaces? (output-text c))
                 (read-char)
                 (loop (if keep-spaces? #f t))))
              ((eqv? c comment-char1)
               (read-char)
               (if comment-char2
                   (if (eqv? (peek-char) comment-char2)
                       (begin
                         (skip-line)
                         (loop (end-token)))
                       (begin
                         (end-token)
                         (output-text c)
                         (loop #f)))
                   (begin
                     (skip-line)
                     (loop (end-token)))))
              ((char=? c #\")
               (end-token)
               (output-object (read))
               (loop #f))
              ((if ident-chars
                   (not (memv c ident-chars))
                   (or (char=? c #\()
                       (char=? c #\))
                       (char=? c #\')
                       (char=? c #\`)
                       (char=? c #\,)))
               (end-token)
               (output-text (read-char))
               (loop #f))
              (else
               (if (null? token)
                   (output-text #\space))
               (let ((t (cons (read-char) (or token '()))))
                 (if (and (equal? t '(#\\ #\#)) ;; character prefix?
                          (not (char-alphabetic? (peek-char))))
                     (begin
                       (output-text "#\\")
                       (output-text (read-char))
                       (loop '()))
                     (loop t))))))))

  (define (shorten)

    (define i 0)

    (define (next-short-name name)

      (define (index->string i initial?)
        (let* ((chars
                (if initial?
                    (if keep-ident-case?
                        lowercase-letters
                        valid-initial-identifier-chars)
                    valid-next-identifier-chars))
               (n
                (vector-length chars))
               (c
                (vector-ref chars (modulo i n)))
               (s
                (string
                 (if (and initial?
                          keep-ident-case?
                          (char-upper-case? (string-ref name 0)))
                     (char-upcase c)
                     c))))
          (if (>= i n)
              (string-append s (index->string (quotient (- i n) n) #f))
              s)))

      (set! i (+ i 1))
      (string-append prefix (index->string (- i 1) #t)))

    (for-each
     (lambda (cell)
       (let* ((name (car cell))
              (x (assoc name substitutions)))
         (set-car!
          cell
          (cond (x
                 (cdr x))
                ((char=? #\# (string-ref name 0))
                 name)
                (else
                 (cond ((assq name substitutions)
                        =>
                        cdr)
                       (else
                        (let loop ()
                          (let ((short-name (next-short-name name)))
                            (if (member short-name taken)
                                (loop)
                                short-name))))))))))
     (map cdr
          (list-sort (lambda (x y) (> (car x) (car y)))
                     (map cdr (table->list names))))))

  (parse)
  (shorten)

  (print output))

(define (list-sort! compare list)

  ;; Stable mergesort algorithm

  (define (sort list len)
    (if (= len 1)
        (begin
          (set-cdr! list '())
          list)
        (let ((len1 (quotient len 2)))
          (let loop ((n len1) (tail list))
            (if (> n 0)
                (loop (- n 1) (cdr tail))
                (let ((x (sort tail (- len len1))))
                  (merge (sort list len1) x)))))))

  (define (merge list1 list2)
    (if (pair? list1)
        (if (pair? list2)
            (let ((x1 (car list1))
                  (x2 (car list2)))
              (if (compare x2 x1)
                  (merge-loop list2 list2 list1 (cdr list2))
                  (merge-loop list1 list1 (cdr list1) list2)))
            list1)
        list2))

  (define (merge-loop result prev list1 list2)
    (if (pair? list1)
        (if (pair? list2)
            (let ((x1 (car list1))
                  (x2 (car list2)))
              (if (compare x2 x1)
                  (begin
                    (set-cdr! prev list2)
                    (merge-loop result list2 list1 (cdr list2)))
                  (begin
                    (set-cdr! prev list1)
                    (merge-loop result list1 (cdr list1) list2))))
            (begin
              (set-cdr! prev list1)
              result))
        (begin
          (set-cdr! prev list2)
          result)))

  (let ((len (length list)))
    (if (= 0 len)
        '()
        (sort list len))))

(define (list-sort compare list)
  (list-sort! compare (append list '())))

(define lowercase-letters '#(
#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
#\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
))

(define valid-initial-identifier-chars '#(
#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
#\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
#\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
#\_
))

(define valid-next-identifier-chars '#(
#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
#\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
#\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
#\_
#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
;;#\= #\< #\> #\+ #\- #\* #\/ #\~ #\! #\$ #\% #\^ #\& #\? #\@
))

(define (main . args)

  (let loop ((args args))
    (if (pair? args)
        (let ((arg (car args))
              (rest (cdr args)))
          (cond ((and (pair? rest) (member arg '("--ident-chars")))
                 (set! ident-chars
                   (string->list (car rest)))
                 (loop (cdr rest)))
                ((and (pair? rest) (member arg '("--comment-char")))
                 (set! comment-char1 (string-ref (car rest) 0))
                 (loop (cdr rest)))
                ((and (pair? rest) (member arg '("--comment-char2")))
                 (set! comment-char2 (string-ref (car rest) 0))
                 (loop (cdr rest)))
                ((and (pair? rest) (member arg '("--keep-spaces")))
                 (set! keep-spaces? #t)
                 (loop rest))
                ((and (pair? rest) (member arg '("--keep-ident-case")))
                 (set! keep-ident-case? #t)
                 (loop rest))
                ((and (pair? rest) (member arg '("--prefix")))
                 (set! prefix (car rest))
                 (loop (cdr rest)))
                (else
                 (let ((names
                        (call-with-input-string
                            arg
                          (lambda (p)
                            (read-all p
                                      (lambda (p)
                                        (read-line p #\space)))))))
                   (set! substitutions
                     (cons
                      (cons (car names)
                            (if (pair? (cdr names))
                                (cadr names)
                                (car names)))
                      substitutions))
                   (loop rest)))))))

  (if (and (not comment-char1) (not ident-chars))
      (set! comment-char1 #\;)) ;; Scheme

  (minify))
