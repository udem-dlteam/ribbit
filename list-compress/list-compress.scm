;; Compress & Decompress a list of VM bytecode

(define ex '(2 1 1 0 0 4 2 (0 foo 3 1) 0 1 2 5 3 1))
(define almost '(4 1 2 1 2 1 5 4 2 8 3 1 6 1 3 7 7 1 3 10 2 (1 1 1 4 7 1 7 4 2 3 3) 1 3 8 1 7 4 1 2 (1 2 1 2 9 1 8 4 1 1 4 1 8 4 2 3 3) 1 1 1 3 1 5 8 1 11 4 3 3 4 6 1 2 1 8 4 1 2 (1 3 3 1 3 1) 1 2 7 1 2 7 9 1 2 8 1 4 8 1 6 1 13 4 3 9 3 1 4 1 2 1 10 4 1 2 (1 2 1 2 1 12 4 2 3 3) 1 2 1 12 4 1 2 (1 1 1 3 8 1 4 7 1 15 4 3 3 4) 1 2 3 1 3 1 6 1 3 0 quote 10 2 (1 2 7 3 1) 1 3 0 set! 10 2 (1 1 1 3 1 16 4 1 1 17 4 2 1 3 7 1 3 1 12 4 3 3 4) 1 3 0 define 10 2 (1 1 1 3 1 17 4 1 1 18 4 2 1 3 7 1 3 1 12 4 3 3 4) 1 3 0 lambda 10 2 (1 1 1 3 1 18 4 2 3 3) 1 3 0 begin 10 2 (1 1 1 3 0 #f 1 20 4 3 3 4) 1 3 0 cond 10 2 (1 1 1 3 1 20 4 2 3 3) 1 1 1 3 1 5 1 22 4 3 3 4 4 1 2 1 2 9 3 1 6 1 2 1 8 4 1 2 (1 3 3 1 3 1) 1 1 1 3 8 1 3 1 5 7 1 20 4 2 1 21 4 3 3 4 4 1 1 1 3 7 7 1 17 4 2 2 (1 1 1 3 7 1 16 4 1 1 17 4 2 3 3) 1 2 8 1 13 4 1 2 (1 1 1 3 8 1 20 4 2 3 3) 1 19 4 0 3 1 6 1 1 1 3 1 23 4 2 1 2 1 5 1 19 4 2 1 24 4 2 3 3 4 1 2 1 7 4 1 2 (0 () 3 1) 1 1 1 3 7 1 17 4 2 1 2 1 4 8 1 24 4 2 9 3 1 4 1 2 7 0 prim 10 2 (1 2 8 0 read 10 2 (1 22 4 0 3 1) 1 2 8 0 newline 10 2 (1 23 4 0 3 1) 1 2 8 0 write 10 2 (1 1 7 1 25 4 1 3 2) 1 2 8 0 symbol? 10 2 (1 1 7 1 11 4 1 3 2) 1 2 8 0 null? 10 2 (1 1 7 1 7 4 1 3 2) 1 2 8 0 pair? 10 2 (1 1 7 1 13 4 1 3 2) 1 2 8 0 car 10 2 (1 1 7 7 3 1) 1 2 8 0 cdr 10 2 (1 1 7 8 3 1) 1 2 8 0 cadr 10 2 (1 1 7 1 15 4 1 3 2) 1 2 8 0 cons 10 2 (1 1 7 1 2 1 16 4 1 9 3 1) 1 2 8 0 set-cdr! 10 2 (1 1 1 15 4 1 1 2 7 1 7 4 2 3 3) 1 2 8 0 assoc 10 2 (1 1 1 15 4 1 1 2 7 1 6 4 2 3 3) 1 2 8 0 eq? 10 2 (1 1 7 1 2 1 16 4 1 10 3 1) 1 2 8 0 list 10 2 (1 1 3 1 3 1) 1 2 8 0 + 10 2 (1 1 1 15 4 1 1 2 7 1 27 4 2 3 3) 1 2 8 0 - 10 2 (1 1 1 15 4 1 1 2 7 1 28 4 2 3 3) 1 2 8 0 < 10 2 (1 1 7 1 2 1 16 4 1 5 3 1) 1 2 8 0 = 10 2 (1 1 7 1 2 1 16 4 1 6 3 1) 1 2 8 0 real-time 10 2 (1 27 4 0 3 1) 1 20 4 0 3 1) 1 1 1 3 7 7 1 4 8 1 12 4 3 1 3 7 1 16 4 1 1 17 4 2 3 3))

(define LPAR 30)
(define RPAR 31)
(define START-SYM 29)
(define NIL 28)

(define (wrap head tail lst)
  (cons head (append lst (list tail))))

(define (rwrap head tail lst)
  (reverse (wrap head tail lst)))

(define (char->alpha char)
  (- (char->integer char) (char->integer #\a)))

(define (chars->alpha str) (map char->alpha str))

(define (transform code)
  (let loop ((todo code)
             (done '()))
    (if (null? todo)
        (reverse done)
        (let ((curr (car todo))
              (todo (cdr todo)))
          (loop
            todo
            (cond ((number? curr) (cons curr done))
                  ((boolean? curr) (cons NIL done))
                  ((list? curr)
                   (let ((trnsf (transform curr)))
                     (append (rwrap LPAR RPAR trnsf) done)))
                  ((symbol? curr)
                   (let* ((chars (string->list (symbol->string curr)))
                          (alpha (reverse (chars->alpha chars))))
                     (append (rwrap START-SYM RPAR alpha) done)))
                  (else
                    (begin
                      (step)
                      (error (string-append "unknown thing: " (object->string curr)))))))))))
