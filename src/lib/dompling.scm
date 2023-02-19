;; A simple DOM manipulation library for ribbit programs targetting js

(cond-expand
  ((host js)
    (define-primitive (get-element-by-id id)
      "() => push(document.getElementById(pop())"
      )
    (define-primitive (get-attr attr element)
      "() => push(string_to_rib(pop()[rib_to_string(pop()))]"
      )
    )

  )