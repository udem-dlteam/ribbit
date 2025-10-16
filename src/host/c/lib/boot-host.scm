


(define-primitive (command-line)
  (use list2scm argv)
  "{push2(list2scm(argv, argc), PAIR_TAG);break;}")

;(define-primitive (file-exists? file)
;  (use bool2scm scm2str js/node/fs)
;  "prim1((filename) => bool2scm(fs.existsSync(scm2str(filename)))),")

(define (file-exists? file)
  (not (not (%%get-fd-input-file file))))
