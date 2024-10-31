(define-primitive (##apply f args)
  " ,  (do
    args <- pop
    f <- pop
    let loop numArgs' arg'  = do
          if arg' == ribNil then 
            push numArgs' >> return f
          else do
            read0 arg' >>= push
            read1 arg' >>= loop (numArgs' + 1)
    loop (0::Int) args)")
