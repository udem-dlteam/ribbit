(define-macro 
  (%comptime . body)
  (let ((name 'anonymous-comp-time-macro))
    (for-each eval body)
    '()))

