;; modified test taken from: https://github.com/alaricsp/chicken-scheme/blob/master/tests/r4rstest.scm

(define disjoint-type-functions
  (list boolean? char? null? number? pair? procedure? string? symbol? vector?))

(define type-examples
  (list #t #f #\a '() 9739 '(test) (lambda (x) x) "test" "" 'test '#() '#(a b c)))

(define type-matrix
  (map (lambda (x)
         (let ((t (map (lambda (f) (f x)) disjoint-type-functions)))
           (write t)
           (write x)
           (newline)
           t))
       type-examples))

;; (##id type-matrix)

(define i 0)
(define j 0)

(for-each 
  (lambda (x y)
    (set! j (+ 1 j))
    (set! i 0)
    (for-each (lambda (f)
                (set! i (+ 1 i))
                (cond 
                  ((eqv? i j)
                   (write (eqv? (f x) (f y))))
                  (else 
                   (write (not (eqv? (f x) (f y)))))))
              disjoint-type-functions)
    (newline))

  (list #t #\a '() 9739 '(test) (lambda (x) x) "test" 'car '#(a b c))
  (list #f #\newline '() -3252 '(t . t) car "" 'nil '#()))

;;;options: -l r4rs
;;;expected:
;;;(#t #f #f #f #f #f #f #f #f)#t
;;;(#t #f #f #f #f #f #f #f #f)#f
;;;(#f #t #f #f #f #f #f #f #f)#\a
;;;(#f #f #t #f #f #f #f #f #f)()
;;;(#f #f #f #t #f #f #f #f #f)9739
;;;(#f #f #f #f #t #f #f #f #f)(test)
;;;(#f #f #f #f #f #t #f #f #f)#p
;;;(#f #f #f #f #f #f #t #f #f)"test"
;;;(#f #f #f #f #f #f #t #f #f)""
;;;(#f #f #f #f #f #f #f #t #f)test
;;;(#f #f #f #f #f #f #f #f #t)#()
;;;(#f #f #f #f #f #f #f #f #t)#(a b c)
;;;#t#f#f#f#f#f#f#f#f
;;;#f#t#f#f#f#f#f#f#f
;;;#f#f#t#f#f#f#f#f#f
;;;#f#f#f#t#f#f#f#f#f
;;;#f#f#f#f#t#f#f#f#f
;;;#f#f#f#f#f#t#f#f#f
;;;#f#f#f#f#f#f#t#f#f
;;;#f#f#f#f#f#f#f#t#f
;;;#f#f#f#f#f#f#f#f#t
