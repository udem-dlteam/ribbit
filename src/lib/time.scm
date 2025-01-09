;; File: time.scm
;;
;; Library to compute the time functions take. This library
;; assumes certain primitives to be available:
;;
;;  (##timer-start)   : Start an internal timer.
;;  (##timer-end)     : Ends an internal timer.
;;  (##timer-display) : Display to stdout the time elapsed between
;;                      the call to ##start-timer and ##end-timer.
;;
;; These primitives have been chosen to limit the computations done
;; inside scheme and to allow hosts to easily implement them.

(##include-once (ribbit "define-macro"))

;; Should be define for each host
(##include-once (ribbit "prim-timer"))

(define-macro (time thunk)
  `(begin
     (##timer-start)
     ,thunk
     (##timer-end)
     (##timer-display)
     0))


