(cond-expand
  ((host js)
   (define-primitive (hello_world)
                     (use str2scm)
                     "() => push(str2scm('hello world')),"
                     )
   (define-primitive (console.log s)
                     (use scm2str)
                     "() => {console.log(pop()); return push(NIL);},")


   (define-primitive (string-from-file path)
                     (use scm2str str_to_rib)
                     "() => {try{return push(str_to_rib(node_fs.readFileSync(scm2str(pop()), 'utf-8').toString()))}catch{ return push(FALSE)}},")

   (define-primitive (command-line)
                     (use list2scm)
                     "() => push(list2scm(process.argv)),")

   (define-primitive (debug-callback func)
                     (use debug_callback scm2function)
                     "() => debug_callback(scm2function(pop())),")

   (define-primitive (host-eval str)
                     (use scm2str host2scm host-call)
                     "() => push(host2scm(eval(scm2str(pop())))),")


   (%%id host-call)
   (%%id %%arg2)

   (define-primitive (host-call foreign_function lst_args)
                     (use host_call rest-param)
                     "host_call,")

   (define-primitive (foreign-eval str)
                     (use scm2str host2scm foreign)
                     "() => push(foreign(eval(scm2str(pop())))),")
   (define-primitive (step)
                     "() => {debugger;return push(TRUE)},")))

