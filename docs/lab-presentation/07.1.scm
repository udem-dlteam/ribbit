;; Maintenant, nous allons compiler le fichier suivant avec rsc
;; Ce fichier affiche $, mais utilise des primitives !
;; $ gsi rsc.scm -t py -l empty ../presentation/07.1.scm -o ../presentation/07.2.py
;; $ gsi rsc.scm -t c -l empty ../presentation/07.1.scm -o ../presentation/07.2.c
;; $ gsi rsc.scm -t js -l empty ../presentation/07.1.scm -o ../presentation/07.2.js


(cond-expand
  ((host py)
   (define-primitive (square a)
                     "lambda: push(pop() ** 2),")
   )
  ((host js)
   (define-primitive (square a)
                     "() => push(pop() ** 2)")
   )
  ((host c)
   (define-primitive (square a)
                     "{PRIM1();push2(TAG_NUM((NUM(x) * NUM(x))), PAIR_TAG);break;}"))

  )

(putchar (square 6)) ;; Affiche $