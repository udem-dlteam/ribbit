(define-primitive (##apply f args)
  "() => {
     let num_args = 0;
     let arg = pop();
     let f = pop();
     while (arg !== NIL) {
         push(arg[0]);
         arg=arg[1];
         num_args++;
     }
     push(num_args); // @@(feature arity-check)@@
     return f;
   }, ")
