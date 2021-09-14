#!/usr/bin/env gsi

(define ident-chars #f)
(define substitutions '())
(define comment-char1 #f)
(define comment-char2 #f)

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
        (let ((c (read-char)))
          (if (not (or (eof-object? c) (char=? c #\newline)))
              (skip-line))))

      (let ((c (peek-char)))
        (cond ((eof-object? c)
               (end-token))
              ((char<=? c #\space)
               (read-char)
               (loop (end-token)))
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

    (define (next-short-name)

      (define (index->string i)
        (let* ((n (vector-length valid-identifier-chars))
               (s (string (vector-ref valid-identifier-chars (modulo i n)))))
          (if (>= i n)
              (string-append (index->string (quotient (- i n) n)) s)
              s)))

      (set! i (+ i 1))
      (index->string (- i 1)))

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
                          (let ((short-name (next-short-name)))
                            (if (member short-name taken)
                                (loop)
                                short-name))))))))))
     (map cdr
          (list-sort (lambda (x y) (> (car x) (car y)))
                     (map cdr (table->list names))))))

  (parse)
  (shorten)

  (print output))

(define valid-identifier-chars '#(
#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
#\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
#\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
#\_
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
