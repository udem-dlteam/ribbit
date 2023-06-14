(define (error msg)
  (display msg)
  (newline)
  (##exit 1))

(define (crash)
  (error "(._.')"))
