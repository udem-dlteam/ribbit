(display (call-with-current-continuation
           (lambda (exit)
             (for-each (lambda (x) (if (negative? x) (exit x)))
                       '(54 0 37 -3 245 19))
             #t)))
(newline)

(define list-length
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
        (letrec ((r (lambda (obj) (cond ((null? obj) 0)
                                        ((pair? obj) (+ (r (cdr obj)) 1))
                                        (else (return #f))))))
          (r obj))))))

(display (list-length '(1 2 3 4)))
(newline)

(display (list-length '(a b . c)))
(newline)

(display (map cadr '()))
(newline)


;;; This tests full conformance of call-with-current-continuation.  It
;;; is a separate test because some schemes do not support call/cc
;;; other than escape procedures.  I am indebted to
;;; raja@copper.ucs.indiana.edu (Raja Sooriamurthi) for fixing this
;;; code.  The function leaf-eq? compares the leaves of 2 arbitrary
;;; trees constructed of conses.
(define (next-leaf-generator obj eot)
  (letrec ((return #f)
           (cont (lambda (x)
                   (recur obj)
                   (set! cont (lambda (x) (return eot)))
                   (cont #f)))
           (recur (lambda (obj)
                    (if (pair? obj)
                      (for-each recur obj)
                      (call-with-current-continuation
                        (lambda (c)
                          (set! cont c)
                          (return obj)))))))
    (lambda () (call-with-current-continuation
                 (lambda (ret) (set! return ret) (cont #f))))))

(define (leaf-eq? x y)
  (let ((eot (list 'eot)))
    (let ((xf (next-leaf-generator x eot))
          (yf (next-leaf-generator y eot)))
      (letrec ((loop (lambda (x y)
                       (cond ((not (eq? x y)) #f)
                             ((eq? eot x) #t)
                             (else (loop (xf) (yf)))))))
        (loop (xf) (yf))))))

(display (leaf-eq? '(a (b (c))) '((a) b c)))
(newline)

(display (leaf-eq? '(a (b (c))) '((a) b c d)))
(newline)

;;;options: -l r4rs
;;;expected:
;;;-3
;;;4
;;;#f
;;;()
;;;#t
;;;#f
