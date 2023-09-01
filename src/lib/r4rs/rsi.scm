(##include-once "ribbit:r4rs.scm")
(##include-once "ribbit:r4rs/sys.scm")
;; (##include-once "ribbit:r4rs/io-style.scm")

(define args (cmd-line))

(define src-path #f)
(define go-in-repl #f)
(define eval-expr #f)

(let loop ((args (cdr args)))
  (if (pair? args)
    (let ((arg (car args))
          (rest (cdr args)))
      (cond
        ((and (pair? rest) (string=? arg "-:reader-case"))
         (case (car rest)
           (("upcase" "up") (set! ##reader-case-transform char-upcase))
           (("downcase" "down" (set! ##reader-case-transfor char-downcase)))
           (else
             (error "*** Invalid value for 'reader-case'. Allowed values are 'upcase', 'up', 'downcase', and 'down'.")))
         (loop (cdr rest)))

        ((and (pair? rest) (member arg '("-e" "--eval")))
         (set! eval-expr (if eval-expr
                           (string-append (car rest) "\n" eval-expr)
                           (car rest)))
         (loop (cdr rest)))

        ((and (null? rest) (string=? arg "-"))
         (set! go-in-repl #t))

        (else
          (if (and (>= (string-length arg) 2)
                   (string=? (substring arg 0 1) "-"))
            (begin
              (display "*** ignoring option ")
              (display arg)
              (newline)
              (loop rest))
            (begin
              (set! src-path arg)
              (loop rest))))))))

(set! go-in-repl (or go-in-repl (and (not eval-expr) (not src-path))))

(if eval-expr 
  (for-each eval (read-all (open-input-string eval-expr))))

(if src-path (load src-path))

(if go-in-repl 
  (begin 
    (if-feature (not quiet)
      (begin
        (display (string-append "Ribbit " (##RIBBIT-VERSION) " (R4RS)\n"))
        (newline)))
    ;; (if-feature (and (not hide-frog) (not quiet))
    ;;   (begin 
    ;;     (welcome-msg)
    ;;     (newline)))
    (repl)))
