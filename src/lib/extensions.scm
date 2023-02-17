
(cond-expand
  ((host js)
   (define-primitive (hello_world)
                     (use str_to_rib)
                     "() => push('hello world'),"
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


   )
  

  )

(display (hello_world))

;(display (console.log "hey there"))
;(display (string-from-file "./input.scm"))




