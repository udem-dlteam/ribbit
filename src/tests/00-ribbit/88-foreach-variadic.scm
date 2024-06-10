(for-each (lambda (x) (display x) (newline)) (list 1 2 3 4 5))

(for-each (lambda (x y) (display x) (display y) (newline)) (list 1 2 3 4 5) (list 6 7 8 9 10))

(for-each (lambda (x y z) (display x) (display y) (display z) (newline)) (list 1 2 3 4 5) (list 6 7 8 9 10) (list 11 12 13 14 15))

;;;r4rs-run: -l r4rs/io -l r4rs/control -l r4rs/pair-list
;;;expected:
;;;1
;;;2
;;;3
;;;4
;;;5
;;;16
;;;27
;;;38
;;;49
;;;510
;;;1611
;;;2712
;;;3813
;;;4914
;;;51015
