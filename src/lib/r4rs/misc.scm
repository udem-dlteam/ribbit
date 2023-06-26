(cond-expand
  ((host js)

   (define-primitive 
     (cmd-line)
     (use js/node list2scm argv)
     "() => push(list2scm(process.argv.slice(1))),")

   (define-primitive 
     (current-directory)
     (use js/node str2scm)
     "() => push(str2scm(__dirname)),")

   (define-primitive 
     (list-dir dir-name)
     (use js/node js/node/fs list2scm scm2str argv)
     "prim1(dirName => list2scm(fs.readdirSync(scm2str(dirName)))),"))

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
   )))
