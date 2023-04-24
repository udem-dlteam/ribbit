(display "hello")

(define (read-all)
  (let ((x (read)))
    (if (eof-object? x)
        '()
        (cons x (read-all)))))

(display (read-all))
