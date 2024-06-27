;; This script can collect ribs that will be printed at the end
;; To run this : 
;;  > gsi rsc.scm -t js -l max examples/rib_eater.scm -o eat_ribs.js
;;  > node eat_ribs.js


(define-feature 
  rib_eater
  (use scm2host)
  ((decl "collected_ribs = [];"
         "function eat_rib(r) { collected_ribs.push(scm2host(r)); }")
   (start "console.log('Rib eater is activated');")
   (end "console.log('Here are my lovely eaten ribs (miam) : ', collected_ribs)")))

(define-primitive 
  (eat rib)
  (use rib_eater)
  "() => {eat_rib(pop()); return push(FALSE)},")

(eat 42)
(eat eat)
(eat (lambda (foo bar) (+ foo bar)))
(eat 'hello_world)
