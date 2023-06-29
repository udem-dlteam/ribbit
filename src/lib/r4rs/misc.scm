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
     "prim1(dirName => list2scm(fs.readdirSync(scm2str(dirName)))),")
   
   #;(define-feature js/node/child_process (decl "const child_p = require('child_process');\n"))

   #;(define-primitive
     (shell-cmd cmd args)
     (use js/node js/node/fs js/node/child_process scm2list list2scm scm2str shell-cmd)
     "prim2((args, cmd) => child_p.spawnSync(scm2str(cmd), scm2list(args))),"))

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
