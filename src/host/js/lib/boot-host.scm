(define-primitive (command-line)
  (use list2scm)
  "() => push(list2scm(process.argv.slice(1))),")

(define-primitive (file-exists? file)
  (use bool2scm js/node/fs)
  "prim1((filename) => bool2scm(fs.existsSync(filename))),")

(define-primitive (console.log obj)
  (use list2scm)
  "prim1((obj) => { console.log(obj); return TRUE; }),")

