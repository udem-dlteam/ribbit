(define (error . msg)
  (for-each display msg)
  (newline)
  (##exit 1))

;; (define stderr (##rib 2 #t output-port-type))

;; (if-feature 
;;   (not no-err)
;;   (define (crash (reason ""))
;;     (display (string-append "(._.') " reason) stderr)
;;     (newline stderr))

(define (crash)
  (error "(._.')"))

