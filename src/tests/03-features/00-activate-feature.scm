(if-feature activate-me
  (display "activate-me is activated\n")
  (display "activate-me is not activated\n"))

(if-feature deactivate-me
  (display "deactivate-me is activated\n")
  (display "deactivate-me is not activated\n"))

;;;run: -l max -f+ activate-me -f- deactivate-me
;;;run: -l max -f= activate-me #t -f= deactivate-me #f
;;;expected:
;;;activate-me is activated
;;;deactivate-me is not activated
