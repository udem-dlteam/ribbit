(%%include-once (ribbit "define-macro"))

(define-macro 
  (%%define-resource-reader name reader)
  (add-resource-str-reader!
    name
    (eval reader))
  '())


