(define-macro (%rib x y z) `(%%rib ,x ,y ,z))
(define-macro (%rib? x) `(%%rib? ,x))

(define-macro (%id x) `(%%id ,x))

(define-macro (%close x)  `(%%close ,x))
(define-macro (%arg1 x y) `(%%arg1 ,x ,y))
(define-macro (%arg2 x y) `(%%arg2 ,x ,y))
(define-macro (%field0 x) `(%%field0 ,x))
(define-macro (%field1 x) `(%%field1 ,x))
(define-macro (%field2 x) `(%%field2 ,x))
(define-macro (%field0-set! x y) `(%%field0-set! ,x ,y))
(define-macro (%field1-set! x y) `(%%field1-set! ,x ,y))
(define-macro (%field2-set! x y) `(%%field2-set! ,x ,y))

(define-macro (%exit code) `(%%exit ,code))

(define-macro (%eqv? x y) `(%%eqv? ,x ,y))
(define-macro (%< x y) `(%%< ,x ,y))
(define-macro (%+ x y) `(%%+ ,x ,y))
(define-macro (%- x y) `(%%- ,x ,y))
(define-macro (%* x y) `(%%* ,x ,y))
(define-macro (%quotient x y) `(%%quotient ,x ,y))

(define-macro (%getchar) '(%%getchar))
(define-macro (%putchar x) `(%%putchar ,x))
