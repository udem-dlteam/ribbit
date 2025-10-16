;; Simple example to showcase the use of the FFI with Ribbit
;; It defines a simple square primitive in the host language
;; (here C and Python).
;;
;; To run this script, you need to have the Ribbit compiler
;; and run the following lines from the 'src' directory:
;;
;; $ ./rsc.exe -t c -o square.c examples/square.scm
;; $ gcc -o square square.c
;; $ ./square
;; @
;;
;; $ ./rsc.exe -t py -o square.py examples/square.scm
;; $ python square.py
;; @


(cond-expand ((host py) ;; Python host
              (define-primitive (square x)
                "lambda: push(pop()**2),"))
             ((host c) ;; C host
              (define-primitive (square x)
                "{
                  int x = NUM(pop());
                  push2(TAG_NUM(x*x), PAIR_TAG);
                 }")))

(%%putchar (square 8)) ;; prints '@' as 8*8 = 64, the ASCII value of '@'
(%%putchar 10) ;; prints a newline
