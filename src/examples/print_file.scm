;; This script prints the file passed as argument
;; To run this : 
;;  > gsi rsc.scm -t js -l max examples/print_file.scm -o print_file.js
;;  > node print_file.js file_to_print

(cond-expand
  ((host js)
   (define-primitive (string-from-file path)
                     (use scm2str str2scm js/node/fs)
                     "() => {try{return push(str2scm(fs.readFileSync(scm2str(pop()), 'utf-8').toString()))}catch{ return push(FALSE)}},")

   (define-primitive (command-line)
                     (use list2scm)
                     "() => push(list2scm(process.argv.splice(1))),")))

(let ((filename (cadr (command-line))))  ;; take the first argument of the script
  (display (string-append filename ":\n")) 
  (display (string-from-file filename))) ;; display the file
