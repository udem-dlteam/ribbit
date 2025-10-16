(define-feature %%apply
  ((decl
"def prim_apply():
 _arg = pop()
 f = pop()
 num_args=0
 while _arg is not NIL:
  push(_arg[0])
  _arg=_arg[1]
  num_args += 1
 push(num_args) # @@(feature arity-check)@@
 return f

")))

(define-primitive (%%apply f args)
     "prim_apply,")
