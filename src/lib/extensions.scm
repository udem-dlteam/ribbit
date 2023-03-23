
(cond-expand
  ((host js)
   (define-primitive (hello_world)
                     (use str_to_rib)
                     "() => push(str_to_rib('hello world')),"
                     )
   ;(define-primitive (console.log s)
   ;                  (use rib_to_str)
   ;                  "() => console.log(rib_to_str(pop())),")


   (define-primitive (string-from-file path)
                     (use rib_to_str str_to_rib)
                     "() => {try{return push(str_to_rib(node_fs.readFileSync(rib_to_str(pop()), 'utf-8').toString()))}catch{ return push(FALSE)}},")

   ;(define-primitive (show-tab)
   ;                  (use list_to_rib)
   ;                  "() => push(list_to_rib([1, 2, 3]))")

   (define-primitive (command-line)
                     (use list_to_rib)
                     "() => push(list_to_rib(process.argv)),")

   (define-primitive (debug-callback func)
                     (use debug_callback rib_to_function)
                     "() => debug_callback(rib_to_function(pop())),")

   (define-primitive (host-eval str)
                     (use rib_to_str any_to_rib host-call)
                     "() => push(any_to_rib(eval(rib_to_str(pop())))),")

   (define-primitive (host-call foreign_function lst_args)
                     (use host_call)
                     "() => host_call(),")
   (define-primitive (foreign-eval str)
                     (use rib_to_str any_to_rib foreign)
                     "() => push(foreign(eval(rib_to_str(pop())))),")
   (define-primitive (step)
                     "() => {debugger;return push(TRUE)},")))


;(debug-callback (lambda () 3))
;(debug-callback (lambda () "hello world"))
;;(let ((c 3))
;;   (debug-callback (lambda () c)))
;
;;(display 42)
;
;(define ff (foreign-eval "(x) => {console.log(x); return x}"))
;(define result (host-call ff (cons 42 '())))
;
;(display result)
;(define console.log (host-eval "(id) => document.getElementById(id)"))
;(console.log 1)

;(console.log "hello world")
;(display "Hello world as a native string : \"")
;(display (hello_world))
;(display "\"")

;(display (console.log "hey there"))
;(display (string-from-file "./input.scm"))




