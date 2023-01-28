
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

(define (escape-string str)
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
          (append parsed-file (cons (cons '%str (cons start (cons i '()))) '()))))))

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

(define (soft-assoc sym lst)
  (find (lambda (e) (and (pair? e) (eq? (car e) sym)))
        lst))


(define (extract-primitives parsed-file)
  (extract-predicate (lambda (prim) (eq? (car prim) 'primitive)) parsed-file))

(define (extract-features parsed-file)
  (extract-predicate (lambda (prim) (eq? (car prim) 'feature)) parsed-file))

(define (extract-predicate predicate parsed-file)
  (extract (lambda (prim acc)
             (if (predicate prim)
               (cons prim acc)
               acc))
           parsed-file
           '()))

(define (extract walker parsed-file base)
  (letrec ((func 
             (lambda (prim acc)
               (let* ((name (car prim))
                      (body (soft-assoc 'body (cdr prim)))
                      (rec (if body 
                             (fold func base (cadr body))
                             base)))
                 (walker prim (append acc rec))))))
    (fold
      func
      base
      parsed-file)))


(define (needed-features features activated-prims)
  
  (let loop ((used-features (fold (lambda (prim acc) 
                               (let ((uses (soft-assoc 'uses prim)))
                                 (if uses
                                   (append (cdr uses) acc)
                                   acc)))
                             '()
                             activated-prims))
             (needed-features '()))
    (let* ((current (car used-features))
           (current-used (find (lambda (feature) (eq? feature current))
                               features)))

      
      )))

#;(define (needed-features wanted-prims all-prims-and-features)
  
  
  )



#;(define (parse-host-file file-str)
  (define MACRO_CHAR 64) ;#\@

  (let ((str-len (string-length file-str)))
    (let loop ((i 0) 
               (start 0)
               (inside-macro #f)
               (enclose-code? #f) ; code read is enclosed inside an sexp
               (last-new-line 0)
               (current-str "(")) ;; start of list

      (if (< i (- str-len 2))
        (let* ((current-char      (char->integer (string-ref file-str i)))
               (next-char         (char->integer (string-ref file-str (+ i 1))))
               (next-next-char    (char->integer (string-ref file-str (+ i 2))))
               (last-new-line     (if (= current-char 10) i last-new-line)))

          (if (not inside-macro)
            (cond 
              ((and (= current-char MACRO_CHAR)  ;; #\@
                    (= next-char    MACRO_CHAR)
                    (or (= next-next-char    40)
                        (= next-next-char    41))) ;; #\(
               (loop (+ 3 i)
                     (+ 2 i)
                     #t
                     enclose-code?
                     last-new-line
                     (if enclose-code?
                       (string-append current-str 
                                      (escape-string (substring file-str start i)))
                       (string-append current-str
                                      (string-append "(string "
                                                     (string-append (escape-string (substring file-str start last-new-line))
                                                                    ")")))))) 

              (else
                (loop (+ 1 i)
                      start
                      inside-macro
                      enclose-code?
                      last-new-line
                      current-str)))
            (if (and (= current-char MACRO_CHAR)
                     (= next-char    MACRO_CHAR)) ;; #\@
              (let* ((num-parents 2)
                     (input-string 
                       (string-append current-str
                                      (substring file-str start i)))
                     (p (open-input-string 
                          (string-append input-string (make-string num-parents (integer->char 41)))))
                     (last-char (begin (read p) (read-char p)))
                     (last-char-int (if (eq? last-char #!eof) -1 (char->integer last-char)))
                     (new-enclose-code? (not (= last-char-int 41)))) ; #\)
                (loop (+ 2 i)
                      (if new-enclose-code?
                        last-new-line
                        (+ 2 i))
                      #f
                      new-enclose-code?
                      last-new-line
                      (if (and (not new-enclose-code?) (not enclose-code?)) ; happen when same line expression
                        (string-append 
                            current-str
                            (string-append 
                              (substring file-str start (- i 1))
                              (string-append 
                                (escape-string (substring file-str last-new-line i))
                                ")")))
                        input-string)

                      ))
              (loop (+ 1 i)
                    start
                    inside-macro
                    enclose-code?
                    last-new-line
                    current-str))))
        (string-append current-str 
                       (string-append "(string "
                                      (string-append (escape-string (substring file-str start (+ i 2)))
                                                     "))")))))))


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
  lst)
  )


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

(test-find-pattern)

(define (generate-file parsed-file prims)
  (let* ((replace-value "")
         (prefix-first-value "")
         (prefix-rest-value "")
         (first-rvm-prim #t)
         (generated-file "")
         (apply-modifs 
           (lambda (prim-code first?) 
             (let ((prim-code (replace replace-value prim-code)))
               (if first? 
                 (string-append prefix-first-value prim-code)
                 (string-append prefix-rest-value  prim-code))))))
    (let loop ((parsed-file parsed-file))
      (if (not (pair? parsed-file))
        generated-file
        (let ((expr (car parsed-file)))
          (case (car expr) 
            ((rvm-prim) 
             (if first-rvm-prim
               (begin
                 (set! generated-file
                   (string-append
                     generated-file
                     (apply string-append 
                            (cons (apply-modifs (car prims) #t)
                                  (map 
                                    (lambda (prim) 
                                      (apply-modifs prim #f) ) (cdr prims))))))
                 (set! first-rvm-prim #f))))
            ((rvm-prim-generator-setup)
             (for-each 
               (lambda (pair)
                 (if (pair? pair)
                   (case (car pair)
                     ((prefix) (set! prefix-first-value (cadr pair))
                               (set! prefix-rest-value  (caddr pair)))
                     ((replace) (set! replace-value (cadr pair)))
                     (else (error "Unexpected keyword argument in (define-prim-generator ...) in host file" (car pair))))))
               (cdr expr)))

            ((string)
             (if (or first-rvm-prim
                     (not (assq 'rvm-prims parsed-file)))
               (set! generated-file (string-append generated-file (cadr expr)))))
            (else (error "Invalid expression while evaluating parsed host document" expr)))
          (loop (cdr parsed-file)))))))


(let* ((r (parse-host-file (string-from-file "host/c/rvm.c")))
       (show-this '(rib rib? putchar getchar eqv? add))
       (prims (extract-primitives r))
       (features (extract-features r))
       #;(prims-simplified 
         (map (lambda (x) (cons (caar x) (cdr x)))
              prims)))
  
  (pp prims)
  (pp features))


