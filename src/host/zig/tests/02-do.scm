(do ((i 0 (set! i (+ 1 i)))) ((< i 10))
  (write i)
  (putchar 10))



;;;options: -l max
;;;expected:
;;;0
;;;1
;;;2
;;;3
;;;4
;;;5
;;;6
;;;7
;;;8
;;;9
