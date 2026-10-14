
(define-primitive (command-line)
  (use list2scm go/os)
  "{push(list2scm(os.Args))}")


(define (file-exists? file)
  (not (not (%%get-fd-input-file file))))
