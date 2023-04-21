(let ((lst (list 65 108 108 111 33)))
  (begin(
	 (putchar (car lst))
	 (putchar (car (cdr lst)))
	 (putchar (car (cdr (cdr lst))))
	 (putchar (car (cdr (cdr (cdr lst)))))
	 (putchar (car (cdr (cdr (cdr (cdr lst))))))
	 ))
  )
(putchar 10)

;;;options: -l min
;;;input:
;;;expected:
;;;Allo!
