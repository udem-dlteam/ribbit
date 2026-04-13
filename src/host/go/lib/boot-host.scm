
(define-primitive (command-line)
  (use list2scm go/os)
  "{push(list2scm(os.Args[1:]))}")


(define (file-exists? file)
  (not (not (%%get-fd-input-file file))))
