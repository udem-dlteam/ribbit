;; This file must be runned with the js target


(define-feature 
  rib_eater
  (use rib_to_any)
  (decl "collected_ribs = [];
         function eat_rib(r) { collected_ribs.push(rib_to_any(r)); }")
  (start "console.log('Rib eater is activated');")
  (end "console.log('Here are my lovely eaten ribs (miam) : ', collected_ribs)"))

(define-primitive 
  (eat rib)
  (use rib_eater)
  "() => {eat_rib(pop()); return push(FALSE)},")

(eat 42)
(eat eat)
(eat (lambda (foo bar) (+ foo bar)))
(eat 'hello_world)

