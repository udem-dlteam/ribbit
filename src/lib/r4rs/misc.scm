(cond-expand
  ((host js)

   (define-primitive 
     (cmd-line)
     (use js/node list2scm argv)
     "() => push(list2scm(process.argv.slice(1))),")

  (define-primitive
    (apply proc args)
    "prim2((args, proc) => )"))

  ((host c)
   (define-primitive
     (cmd-line)
     (use argv list2scm str2scm)
     "{
     push2(list2scm(argv, argc), PAIR_TAG);
     break;
     }"))

  ((host hs)
   (define-primitive
     (cmd-line)
     (use argv list2scm)
     " , (getArgs >>= \\args -> getProgName >>= \\progName -> (mapM toRibString (progName : args)) >>= toRibList) >>= push"
   ))
  )
