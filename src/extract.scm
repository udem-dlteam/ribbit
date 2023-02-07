
(define (string-from-file path)
  (call-with-input-file path (lambda (port) (read-line port #f))))

#;(define (extract-forms str)

  




  )

#;(define (string-split str sep-code)
  
  (let loop ((start-split 0)
             (char-index 0)
             (result '()))
    (if (< char-index (string-length str))
      (let ((current-char (char->integer (string-ref str char-index))))
        (if (= current-char sep-code) ; #;@
          (loop (+ 1 char-index) 
                (+ 1 char-index) 
                (append 
                  result 
                  (cons (substring str start-split char-index) '())))
          (loop start-split
                (+ 1 char-index)
                result)))
      result)))

#;(define (escape-string str)
  (let loop ((i 0) (start 0) (result ""))
    (let ((current-char (char->integer (string-ref str i))))
      (if (< i (- (string-length str) 1))
        (if (= current-char 34) ; #\"
          (begin
            (loop
              (+ 1 i)
              (+ 1 i)
              (string-append 
                result
                (string-append (substring str start i) "\\\""))))
          (loop
            (+ 1 i)
            start
            result))    

        (string-append "\""
                       (string-append result  
                                      (string-append (substring str start i)
                                                     "\"")))))))

#;(define (escape-string str)
  (let ((p (open-output-string)))
    (write str p)
    (get-output-string p)))

(define (parse-host-file file-str)
  (define MACRO_CHAR 64) ;#\@

  (let ((str-len (string-length file-str)))
    (let loop ((i 0) 
               (start 0)
               (last-new-line 0)
               (current-macro #f)
               (parsed-file '()))

      (if (< i (- str-len 2))
        (let* ((current-char      (char->integer (string-ref file-str i)))
               (next-char         (char->integer (string-ref file-str (+ i 1))))
               (next-next-char    (char->integer (string-ref file-str (+ i 2))))
               (last-new-line     (if (= current-char 10) i last-new-line)))
          (cond 
            ((and 
               (not current-macro)
               (= current-char MACRO_CHAR)
               (= next-char    MACRO_CHAR)
               (= next-next-char 40))
             (loop (+ 2 i)
                   start
                   last-new-line
                   (cons (cons 'head (cons last-new-line (cons i '()))) '()) ;; '((head ,last-new-line ,i))
                   (append parsed-file (cons (cons '%str (cons start (cons i '()))) '()))))

            ((and 
               (not current-macro)
               (= current-char MACRO_CHAR)
               (= next-char    MACRO_CHAR)
               (= next-next-char 41))
             (cons (cons (+ 2 i) last-new-line) 
                   (append parsed-file 
                           (cons (cons '%str 
                                       (cons 
                                         start
                                         (cons i '()))) 
                                 '()))))
             ((and
                current-macro
                (= current-char MACRO_CHAR)
                (= next-char MACRO_CHAR))
              (let* ((num-parents 1)
                     #;(_ (pp current-macro))
                     (macro-start (+ 2 (caddr (assq 'head current-macro))))
                     (macro-string (substring file-str macro-start i))
                     (p (open-input-string 
                          (string-append macro-string (make-string num-parents (integer->char 41)))))
                     (macro-sexp (read p))
                     (last-char (read-char p))
                     (macro-ended (not (eq? last-char #!eof))))
                (if macro-ended
                  (loop (+ i 2)
                        (+ i 2)
                        last-new-line
                        #f
                        (append
                          parsed-file
                          (cons
                            (append macro-sexp current-macro)
                            '())))
                  (let* ((body-pair
                           (loop (+ i 2)
                                 (+ i 2)
                                 last-new-line
                                 #f
                                 '()))
                         (body-i (caar body-pair))
                         (body-last-new-line (cdar body-pair))
                         (body (cdr body-pair)))
                    (loop body-i
                          body-i
                          body-last-new-line
                          #f
                          (append 
                            parsed-file
                            (cons (append macro-sexp (append current-macro (cons (cons 'body (cons body '())) '()))) '())))))))

             (else
               (loop (+ i 1)
                     start
                     last-new-line
                     current-macro
                     parsed-file))))
          (append parsed-file (cons (cons '%str (cons start (cons (+ 2 i) '()))) '()))))))

#;(define (extract-primitives parsed-file)
  (let* ((primitives (assoc 'primitives parsed-file))
         (body (cadr (assoc 'body (cdr primitives)))))
    (let loop ((body body))
      (if (and (pair? body)
               (pair? (car body)))
        (if (eq? (caar body) 'primitive)
          (cons (car body) (loop (cdr body)))
          (loop (cdr body))
          )
        '()))))

#;(define (extract walker parsed-file base)
  (let loop ((acc base) (lst parsed-file))
    (if (pair? lst)
      (loop (walker (car lst) acc) 
            (cdr lst))
      acc)))

(define (find predicate lst)
  (if (pair? lst)
    (if (predicate (car lst))
      (car lst)
      (find predicate (cdr lst)))
    #f))

(define (find-all predicate lst)
  (fold (lambda (x acc) (if (predicate x) (cons x acc) acc))
        lst
        '()))

(define (soft-assoc sym lst)
  (find (lambda (e) (and (pair? e) (eq? (car e) sym)))
        lst))


(define (extract-primitives parsed-file)
  (extract-predicate (lambda (prim) (eq? (car prim) 'primitive)) parsed-file))

(define (extract-features parsed-file)
  (extract-predicate (lambda (prim) (eq? (car prim) 'feature)) parsed-file))

(define (extract-predicate predicate parsed-file)
  (extract (lambda (prim acc rec)
             (if (predicate prim)
               (append (append (rec) acc) (cons prim '()))
               (append (rec) acc)))
           parsed-file
           '()))

(define (extract walker parsed-file base)
  (letrec ((func 
             (lambda (prim acc)
               (let* ((name (car prim))
                      (body (soft-assoc 'body (cdr prim)))
                      (rec (lambda ()
                             (if body 
                               (fold func base (cadr body))
                               base))))
                 (walker prim acc rec)))))
    (fold
      func
      base
      parsed-file)))

(define (unique-aux lst1 lst2)
  (if (pair? lst1)
    (if (memq (car lst1) lst2)
      (unique-aux (cdr lst1) lst2)
      (unique-aux (cdr lst1) (cons (car lst1) lst2)))
    lst2))

(define (unique lst)
  (unique-aux lst '()))

(define (needed-features features activated-prims)
  (let loop ((to-process (unique
                           (fold (lambda (prim acc) 
                                   (let ((uses (soft-assoc 'uses prim)))
                                     (if uses
                                       (append (cdr uses) acc)
                                       acc)))
                                 '()
                                 activated-prims)))
             (needed-features '()))
    (if (pair? to-process)

      (let* ((current (car to-process))
             (current-feature (find (lambda (feature) (eq? (car feature) current))
                                    features))
             (current-used (soft-assoc 'used current-feature))
             (current-used (if current-used (cdr current-used) '())))
        (loop 
          (fold (lambda (x acc) 
                  (if (not (and (memq x to-process)
                                (memq x (needed-features))))
                    (cons x acc)
                    acc))
                (cdr to-process)
                current-used)
          (cons current needed-features)))
      needed-features)))


(define (extract-prims parsed-file)
  (let* ((replace-value "")
         (prefix-first-value "")
         (prefix-rest-value "")
         (result '())
         (extract-prims-for-each
           (lambda (expr)
             (case (car expr)
               ((rvm-prim) 
                (set! result 
                  (append
                    result
                    (cons 
                      (cons (cadr expr)   ;;signature of primitive
                            (caddr expr))
                      '())))) ;; code
               ((rvm-prim-generator-setup)
                (for-each 
                  (lambda (pair)
                    (if (pair? pair)
                      (case (car pair)
                        ((prefix) (set! prefix-first-value (cadr pair))
                                  (set! prefix-rest-value  (caddr pair)))
                        ((replace) (set! replace-value (cadr pair)))
                        (else (error "Unexpected keyword argument in (rvm-prim-generator-setup ...) in host file" (car pair))))))
                  (cdr expr)))
               ((string) (begin #f))
               (else (error "Invalid expression while evaluating parsed host document. Currently accepted are rvm-prim and rvm-prim-generator-setup." expr))))))
    (map extract-prims-for-each parsed-file)
    result))

(define (is-num? c)
  (and (>= c 47)
       (<= c 57)))

(define (find-pattern pattern str start) 

  (let loop ((i start) (lst-pattern pattern))
    (cond 
      ((not (pair? lst-pattern))
       (cons start (- i 1)))
      ((>= i (string-length str))
       #f)
      (else
        (let* ((c-str (char->integer (string-ref str i)))
               (c-pattern (car lst-pattern))
               (c-pattern (if (char? c-pattern) 
                            (char->integer c-pattern)
                            c-pattern)))
          (cond 
            ((number? c-pattern)
             (if (= c-pattern c-str)
               (loop (+ i 1) (cdr lst-pattern))
               (find-pattern pattern str (+ 1 start))))
            ((and (eq? c-pattern '%INDEX%)
                  (is-num? c-str))
             (let skip ((i (+ 1 i)))
               (if (>= i (string-length str))
                 (loop i (cdr pattern))
                 (let ((c-str (char->integer (string-ref str i))))
                   (if (is-num? c-str)
                     (skip (+ 1 i))
                     (loop i (cdr lst-pattern)))))))
            (else
              (find-pattern pattern str (+ 1 start)))))))))

(define (to-pattern lst)
  (fold (lambda (x lst) 
          (cond
            ((string? x)
             (append lst
                     (string->list x)))
            ((symbol? x)
             (append lst
                     (cons x '())))
            (else
              (error "Unknown pattern " x))))
  '()
  lst))


(define (test-find-pattern)
  (define (assert x y)
    (or (equal? x y)
        (error "This is not equal" x y)))

  (define (test-pattern pattern str output)
    (println "testing pattern \"" pattern "\" with string \"" str "\"")
    (assert (find-pattern (to-pattern pattern) str 0) output))

  (test-pattern '("hello") "hello" '(0 . 4))
  (test-pattern '("hello world") "" #f)
  ;(test-pattern '() "hello world" #f)
  (test-pattern '("case " %INDEX% " :") "gad case 0 :" '(4 . 11)))

#;(test-find-pattern)

(define (get-bodies prims str-file)
  (extract
    (lambda (prim acc rec)
      (case (car prim)
        ((primitive)
         (append acc (cons (cons 'prim (cons (cadr prim) (cons (cons 'body (cons (rec) '())) '()))) '())))
        ((%str)
         (substring str-file (cadr prim) (caddr prim)))))
    prims
    '()))

(define (generate-file nfeatures included-prims parsed-file str-file)
  (extract
    (lambda (prim acc rec)
        (case (car prim)
          ((%str)
           (string-append acc (substring str-file (cadr prim) (caddr prim))))
          ((feature)
           (let ((name (cadr prim)))
             (if (memq name nfeatures)
               (string-append acc (rec))
               acc)))
          ((primitives)
           (let* ((gen (cdr (soft-assoc 'gen prim)))
                  (generate-one 
                    (lambda (i body)
                      (let loop ((gen gen))
                        (if (pair? gen)
                          (string-append 
                              (begin
                                (cond ((string? (car gen)) (car gen))
                                    ((eq? (car gen) 'index) (number->string i))
                                    ((eq? (car gen) 'body) body)))
                              (loop (cdr gen)))
                          "")))))
             (string-append 
               acc 
               (cdr
                 (fold 
                   (lambda (x acc)
                     (let* ((i (car acc))
                            (body (soft-assoc 'body x)))
                       (cons (+ i 1)
                             (string-append 
                               (cdr acc)
                               (generate-one i (cadr body))))))
                   (cons 0 "")
                   included-prims)))))
          ((primitive)
           acc) 
          (else
            acc)))
    parsed-file
    ""))

(define (next-line last-new-line)
  (let loop ((cur last-new-line) (len 0))
    (if (or (not (pair? cur)) (eqv? (car cur) 10)) ;; new line
      (begin
        ;(pp (list->string* last-new-line (+ 1 len)) )
        (cons (and (pair? cur) (cdr cur)) (+ 1 len)))
      (loop (cdr cur) (+ len 1)))))

(define (detect-macro line len)
  (let loop ((cur line) (cur-next (cdr line)) (len len) (start #f) (macro-len 0))
    (if (<= len 1)
      (cons #f #f)
      (if (and (eqv? (car cur) 64) (eqv? (cadr cur) 64))
        (if start
          (cons start (+ 2 macro-len))
          (loop (cdr cur-next) (cddr cur-next) (- len 1) cur 2))
        (begin
          (loop cur-next 
              (cdr cur-next)
              (- len 1)
              start
              (if start (+ macro-len 1) macro-len)))))))


;; Can be redefined by ribbit to make this function really fast. It would only be (rib lst len string-type)
(define (list->string* lst len)
  (let ((str (make-string len #\0)))
    (let loop ((lst lst) (i 0))
      (if (< i len)
        (begin
          (string-set! str i (integer->char (car lst)))
          (loop (cdr lst) (+ i 1)))
        str))))

(define (string->list* str)
  (map char->integer (string->list str)))

(define (parse-host-file cur-line)
  (let loop ((cur-line cur-line)
             (parsed-file '())
             (start-len 0)
             (start-line cur-line))
    (if (pair? cur-line)
      (let* ((next-line-pair (next-line cur-line))
             (cur-end (car next-line-pair))
             (cur-len (cdr next-line-pair))
             (macro-pair (detect-macro cur-line cur-len))
             (macro (car macro-pair))
             (macro-len (cdr macro-pair))
             #;(_ (if macro (pp (list->string* macro macro-len)))))
        (cond 
          ((and (eqv? macro-len 5)
                (eqv? (caddr macro) 41))

           (cons cur-end
                 (reverse (cons (cons 'str (cons (list->string* start-line (+ cur-len start-len)) '())) parsed-file))))
          (macro 
            (let* ((parsed-file (if (eqv? start-len 0) parsed-file (cons (cons 'str (list->string* start-line start-len)) parsed-file)))
                   (macro-string (list->string* (cddr macro) (- macro-len 4)))
                   (p (open-input-string (string-append macro-string (make-string 1 (integer->char 41)))))
                   (macro-sexp (read p))
                   (last-char (read-char p))
                   (macro-ended (not (eof-object? last-char))))
              (if macro-ended
                (loop
                  cur-end
                  (cons (append macro-sexp (cons (cons 'head (cons (list->string* cur-line cur-len) '())) '())) parsed-file)
                  0
                  cur-end)
                (let* ((body-pair (parse-host-file cur-end))
                       (body-cur-end (car body-pair))
                       (body-parsed (cdr body-pair)))
                  (loop body-cur-end
                        (cons (append macro-sexp (cons (cons 'head (cons (list->string* cur-line cur-len) '())) (cons (cons 'body (cons body-parsed '())) '()))) parsed-file)
                        0
                        body-cur-end)))))
          (else
            (loop cur-end parsed-file (+ cur-len start-len) start-line))))
      (reverse (cons (cons 'str (list->string* start-line start-len)) parsed-file)))))

(let* ((str-file (string-from-file "host/c/rvm.c"))
       (parsed-file (parse-host-file (string->list* str-file)))
       ;(show-this '(rib rib? ))
       ;(prims (extract-predicate (lambda (p) (and (eq? (car p) 'primitive) (memq (caadr p) show-this))) parsed-file))
       ;(features (extract-features parsed-file))
       ;(nfeatures (needed-features features (map (lambda (x) (cdr x)) prims)))
       ;(generated (generate-file nfeatures (get-bodies prims str-file) parsed-file str-file))
       #;(prims-simplified 
         (map (lambda (x) (cons (caar x) (cdr x)))
              prims))
  )
  #;(pp parsed-file)
  #;(pp (get-bodies prims str-file))
  (pp parsed-file)
  #;(pp nfeatures)
  #;(pp prims)
  #;(pp features))


