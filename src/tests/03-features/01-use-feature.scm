(if-feature activate-me
  (display "activate-me is activated\n")
  (display "activate-me is not activated\n"))

(if-feature deactivate-me
  (display "deactivate-me is activated\n")
  (display "deactivate-me is not activated\n"))

(if-feature activate-me2
  (display "activate-me2 is activated\n")
  (display "activate-me2 is not activated\n"))

(if-feature deactivate-me2
  (display "deactivate-me2 is activated\n")
  (display "deactivate-me2 is not activated\n"))

(use-feature 
  deactivate-me 
  activate-me)

(define (foo x)
  (use-feature activate-me2)
  x)

;; should get eliminated by liveness analysis
(define (bar x) 
  (use-feature deactivate-me2)
  x)

(foo 42)

;;;run: -l max -f+ activate-me -f- deactivate-me
;;;expected:
;;;activate-me is activated
;;;deactivate-me is not activated
;;;activate-me2 is activated
;;;deactivate-me2 is not activated
