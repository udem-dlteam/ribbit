;; This script features the usage of host-eval primitive that
;; can convert a JavaScript function into a Scheme function
;; at runtime.
;;
;; To run this file:
;; > cd src
;; > gsi rsc.scm -t js -l prim-host-eval -l r4rs -f+ js/node ../examples/host-eval-js.scm -o host-eval.js
;; > node host-eval.js
;;
;; You should see you public IP address printed.

(define fetch (host-eval "fetch"))
(define .text (host-eval "(response) => response.text()"))

(let ((response (fetch "https://api.ipify.org")))
  (if response
      (let ((ip (.text response)))
        (display "Your ip is: ")
        (display ip))
      (display "Cannot find your IP")))
