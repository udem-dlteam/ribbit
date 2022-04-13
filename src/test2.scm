(begin ;; display "running test2.scm"
  ($putchar 114) ($putchar 117) ($putchar 110) ($putchar 110) ($putchar 105)
  ($putchar 110) ($putchar 103) ($putchar 32) ($putchar 116) ($putchar 101)
  ($putchar 115) ($putchar 116) ($putchar 50) ($putchar 46) ($putchar 115)
  ($putchar 99) ($putchar 109) ($putchar 10))

(begin ;; display "let -- ok"
  ($putchar 108) ($putchar 101) ($putchar 116) ($putchar 32)
  ($putchar 45) ($putchar 45) ($putchar 32))
(let ((o 111) (k 107))
  ($putchar o) ($putchar k) ($putchar 10))

(begin ;; display "$field0 -- ok"
  ($putchar 36) ($putchar 102) ($putchar 105) ($putchar 101)
  ($putchar 108) ($putchar 100) ($putchar 48) ($putchar 32)
  ($putchar 45) ($putchar 45) ($putchar 32))
(let ((a ($rib 111 95 95)) (b ($rib 107 95 95)))
  ($putchar ($field0 a)) ($putchar ($field0 b)) ($putchar 10))

(begin ;; display "$field1 -- ok"
  ($putchar 36) ($putchar 102) ($putchar 105) ($putchar 101)
  ($putchar 108) ($putchar 100) ($putchar 49) ($putchar 32)
  ($putchar 45) ($putchar 45) ($putchar 32))
(let ((a ($rib 95 111 95)) (b ($rib 95 107 95)))
  ($putchar ($field1 a)) ($putchar ($field1 b)) ($putchar 10))

(begin ;; display "$field2 -- ok"
  ($putchar 36) ($putchar 102) ($putchar 105) ($putchar 101)
  ($putchar 108) ($putchar 100) ($putchar 50) ($putchar 32)
  ($putchar 45) ($putchar 45) ($putchar 32))
(let ((a ($rib 95 95 111)) (b ($rib 95 95 107)))
  ($putchar ($field2 a)) ($putchar ($field2 b)) ($putchar 10))

(begin ;; display "$field0-set! -- ok"
  ($putchar 102) ($putchar 105) ($putchar 101) ($putchar 108)
  ($putchar 100) ($putchar 48) ($putchar 45) ($putchar 115)
  ($putchar 101) ($putchar 116) ($putchar 33) ($putchar 32)
  ($putchar 45) ($putchar 45) ($putchar 32))
(let ((a ($rib 95 95 95)) (b ($rib 95 95 95)))
  ($field0-set! a 111)
  ($field0-set! b 107)
  ($putchar ($field0 a)) ($putchar ($field0 b)) ($putchar 10))

(begin ;; display "$field1-set! -- ok"
  ($putchar 102) ($putchar 105) ($putchar 101) ($putchar 108)
  ($putchar 100) ($putchar 49) ($putchar 45) ($putchar 115)
  ($putchar 101) ($putchar 116) ($putchar 33) ($putchar 32)
  ($putchar 45) ($putchar 45) ($putchar 32))
(let ((a ($rib 95 95 95)) (b ($rib 95 95 95)))
  ($field1-set! a 111)
  ($field1-set! b 107)
  ($putchar ($field1 a)) ($putchar ($field1 b)) ($putchar 10))

(begin ;; display "$field2-set! -- ok"
  ($putchar 102) ($putchar 105) ($putchar 101) ($putchar 108)
  ($putchar 100) ($putchar 50) ($putchar 45) ($putchar 115)
  ($putchar 101) ($putchar 116) ($putchar 33) ($putchar 32)
  ($putchar 45) ($putchar 45) ($putchar 32))
(let ((a ($rib 95 95 95)) (b ($rib 95 95 95)))
  ($field2-set! a 111)
  ($field2-set! b 107)
  ($putchar ($field2 a)) ($putchar ($field2 b)) ($putchar 10))

(begin ;; display "call lambda -- ok"
  ($putchar 99) ($putchar 97) ($putchar 108) ($putchar 108) ($putchar 32)
  ($putchar 108) ($putchar 97) ($putchar 109) ($putchar 98) ($putchar 100)
  ($putchar 97) ($putchar 32) ($putchar 45) ($putchar 45) ($putchar 32))
((lambda (o k) ($putchar o) ($putchar k) ($putchar 10)) 111 107)

(begin ;; display "tail call -- ok"
  ($putchar 116) ($putchar 97) ($putchar 105) ($putchar 108) ($putchar 32)
  ($putchar 99) ($putchar 97) ($putchar 108) ($putchar 108)
  ($putchar 32) ($putchar 45) ($putchar 45) ($putchar 32))
((lambda (o) ($putchar o) ((lambda (k) ($putchar k) ($putchar 10)) 107)) 111)

(begin ;; display "tail if -- ok"
  ($putchar 116) ($putchar 97) ($putchar 105) ($putchar 108) ($putchar 32)
  ($putchar 105) ($putchar 102)
  ($putchar 32) ($putchar 45) ($putchar 45) ($putchar 32))
((lambda (o k)
   (if #t
       (begin ($putchar o) ($putchar k) ($putchar 10))
       (begin
         ($putchar 101) ($putchar 114) ($putchar 114) ($putchar 111)
         ($putchar 114) ($putchar 33) ($putchar 10))))
 111
 107)

(begin ;; display "lambda with 1 free var -- ok"
  ($putchar 108) ($putchar 97) ($putchar 109) ($putchar 98) ($putchar 100)
  ($putchar 97) ($putchar 32) ($putchar 119) ($putchar 105) ($putchar 116)
  ($putchar 104) ($putchar 32) ($putchar 49) ($putchar 32) ($putchar 102)
  ($putchar 114) ($putchar 101) ($putchar 101) ($putchar 32) ($putchar 118)
  ($putchar 97) ($putchar 114) ($putchar 32) ($putchar 45) ($putchar 45)
  ($putchar 32))
(if ($eqv? 3
           (((lambda (a) (lambda (x) (let ((a ($+ a x))) a))) 2) 1))
    (begin
      ($putchar 111) ($putchar 107) ($putchar 10))
    (begin
      ($putchar 101) ($putchar 114) ($putchar 114) ($putchar 111)
      ($putchar 114) ($putchar 33) ($putchar 10)))

(begin ;; display "lambda with 2 free vars -- ok"
  ($putchar 108) ($putchar 97) ($putchar 109) ($putchar 98) ($putchar 100)
  ($putchar 97) ($putchar 32) ($putchar 119) ($putchar 105) ($putchar 116)
  ($putchar 104) ($putchar 32) ($putchar 50) ($putchar 32) ($putchar 102)
  ($putchar 114) ($putchar 101) ($putchar 101) ($putchar 32) ($putchar 118)
  ($putchar 97) ($putchar 114) ($putchar 115) ($putchar 32) ($putchar 45)
  ($putchar 45) ($putchar 32))
(if ($eqv? 6
           (((lambda (a b) (lambda (x) (let ((b ($+ ($+ a b) x))) b))) 2 3) 1))
    (begin
      ($putchar 111) ($putchar 107) ($putchar 10))
    (begin
      ($putchar 101) ($putchar 114) ($putchar 114) ($putchar 111)
      ($putchar 114) ($putchar 33) ($putchar 10)))

(define (fact n)
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (odd n)  (if (= n 0) #f (even (- n 1))))
(define (even n) (if (= n 0) #t (odd  (- n 1))))

(begin ;; display "($eqv? 3628800 (fact 10)) -- ok"
  ($putchar 40) ($putchar 36) ($putchar 101) ($putchar 113) ($putchar 118)
  ($putchar 63) ($putchar 32) ($putchar 51) ($putchar 54) ($putchar 50)
  ($putchar 56) ($putchar 56) ($putchar 48) ($putchar 48) ($putchar 32)
  ($putchar 40) ($putchar 102) ($putchar 97) ($putchar 99) ($putchar 116)
  ($putchar 32) ($putchar 49) ($putchar 48) ($putchar 41) ($putchar 41)
  ($putchar 32) ($putchar 45) ($putchar 45) ($putchar 32))
(if ($eqv? 3628800 (fact 10))
    (begin
      ($putchar 111) ($putchar 107) ($putchar 10))
    (begin
      ($putchar 101) ($putchar 114) ($putchar 114) ($putchar 111)
      ($putchar 114) ($putchar 33) ($putchar 10)))

(begin ;; display "($eqv? 832040 (fib 30)) -- ok"
  ($putchar 40) ($putchar 36) ($putchar 101) ($putchar 113) ($putchar 118)
  ($putchar 63) ($putchar 32) ($putchar 56) ($putchar 51) ($putchar 50)
  ($putchar 48) ($putchar 52) ($putchar 48) ($putchar 32) ($putchar 40)
  ($putchar 102) ($putchar 105) ($putchar 98) ($putchar 32) ($putchar 51)
  ($putchar 48) ($putchar 41) ($putchar 41) ($putchar 32) ($putchar 45)
  ($putchar 45) ($putchar 32))
(if ($eqv? 832040 (fib 30))
    (begin
      ($putchar 111) ($putchar 107) ($putchar 10))
    (begin
      ($putchar 101) ($putchar 114) ($putchar 114) ($putchar 111)
      ($putchar 114) ($putchar 33) ($putchar 10)))

(begin ;; display "(even 100000000) -- ok"
  ($putchar 40) ($putchar 101) ($putchar 118) ($putchar 101) ($putchar 110)
  ($putchar 32) ($putchar 49) ($putchar 48) ($putchar 48) ($putchar 48)
  ($putchar 48) ($putchar 48) ($putchar 48) ($putchar 48) ($putchar 48)
  ($putchar 41) ($putchar 32) ($putchar 45) ($putchar 45) ($putchar 32))
(if (even 100000000)
    (begin
      ($putchar 111) ($putchar 107) ($putchar 10))
    (begin
      ($putchar 101) ($putchar 114) ($putchar 114) ($putchar 111)
      ($putchar 114) ($putchar 33) ($putchar 10)))

(begin ;; display "($eqv? 'hello (string->symbol \"hello\")) -- ok"
  ($putchar 40) ($putchar 36) ($putchar 101) ($putchar 113) ($putchar 118)
  ($putchar 63) ($putchar 32) ($putchar 39) ($putchar 104) ($putchar 101)
  ($putchar 108) ($putchar 108) ($putchar 111) ($putchar 32) ($putchar 40)
  ($putchar 115) ($putchar 116) ($putchar 114) ($putchar 105) ($putchar 110)
  ($putchar 103) ($putchar 45) ($putchar 62) ($putchar 115) ($putchar 121)
  ($putchar 109) ($putchar 98) ($putchar 111) ($putchar 108) ($putchar 32)
  ($putchar 34) ($putchar 104) ($putchar 101) ($putchar 108) ($putchar 108)
  ($putchar 111) ($putchar 34) ($putchar 41) ($putchar 41) ($putchar 32)
  ($putchar 45) ($putchar 45) ($putchar 32))
(if ($eqv? 'hello (string->symbol "hello"))
    (begin
      ($putchar 111) ($putchar 107) ($putchar 10))
    (begin
      ($putchar 101) ($putchar 114) ($putchar 114) ($putchar 111)
      ($putchar 114) ($putchar 33) ($putchar 10)))

(write (read)) ;; echo stdin
(newline)

(display "test2.scm done!\n")
