'(arg2 1 2)
(define-primitive
  (apply f args)
  (use find_sym arg2)
     "() => {
        let num_args = 0;
        let arg = pop();
        // we don't push the function, because it is already on top of the stack
        while (arg !== NIL) {
            push(arg[0]);
            arg=arg[1];
            num_args++;
        }
        push(num_args); // @@(feature arity-check)@@
        arg2 = find_sym('arg2', symtbl);
        pc = [0,0,[0, num_args + 1, [3, 2, [0, arg2, pc[2]]]]];
        return true;
     }, ")

(putchar (apply + '(35 30)))
(putchar 65)
