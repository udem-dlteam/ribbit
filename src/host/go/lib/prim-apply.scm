(define-primitive (%%apply f args)
"_arg := pop()
f := pop()
numArgs := 0
for _arg != NIL {
  push(_arg.Field0())
  _arg = _arg.Field1()
  numArgs = numArgs + 1
}
push(tagNum(numArgs)) // @@(feature arity-check)@@
return f
")
