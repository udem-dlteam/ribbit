;; en: Maintenant, nous allons compiler le fichier suivant avec rsc
;;     Ce fichier affiche HI!
;;
;; fr: Now we will compile the following file with rsc
;;     This file displays HI!

;; $ gsi rsc.scm -t c -l empty ../presentation/06.1.scm -o ../presentation/06.2.c

(putchar 72) ;; H
(putchar 73) ;; I
(putchar 33) ;; !

