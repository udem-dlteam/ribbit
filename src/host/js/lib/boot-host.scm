(define-primitive (command-line)
  (use list2scm)
  "() => push(list2scm(process.argv.slice(1))),")

;(define-primitive (file-exists? file)
;  (use bool2scm scm2str js/node/fs)
;  "prim1((filename) => bool2scm(fs.existsSync(scm2str(filename)))),")

(define (file-exists? file)
  (not (not (%%get-fd-input-file file))))

(define-primitive (console.log obj)
  (use list2scm)
  "prim1((obj) => { console.log(obj); return TRUE; }),")

