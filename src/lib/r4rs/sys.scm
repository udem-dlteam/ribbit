(cond-expand
  ((host js)

   ;; (define-feature js/node/child-process (use) (decl "const child_p = require('child_process');\n"))

   (define-primitive 
     (##cmd-line)
     (use js/node list2scm argv)
     "() => push(list2scm(process.argv.slice(1))),")

   (define-primitive 
     (##current-directory)
     (use js/node str2scm)
     "() => push(str2scm(__dirname)),")

   (define-primitive 
     (##list-dir dir-name)
     (use js/node js/node/fs list2scm scm2str argv)
     "prim1(dirName => list2scm(fs.readdirSync(scm2str(dirName)))),")

   (define-primitive
     (##shell-cmd cmd)
     (use js/node js/node/fs scm2list list2scm scm2str str2scm)
     "prim1(cmd => str2scm(String(require('child_process').execSync(`sh -c '${scm2str(cmd)}'`)))),")

   (define (list-dir dir-name) (##list-dir dir-name))
   (define (current-directory) (##current-directory)))

  ((host py)

   (define-feature py/sys (use) (import "import os,sys"))

   (define-primitive 
     (##cmd-line)
     (use py/sys str2scm list_str2scm argv)
     "lambda: push(list_str2scm(sys.argv)),")

   (define-primitive 
     (##current-directory)
     (use py/sys str2scm)
     "lambda:push(str2scm(os.path.dirname(os.path.abspath(__file__)))),")

   (define-primitive
     (##shell-cmd cmd)
     (use py/sys scm2list list_str2scm scm2str str2scm)
     "prim1(lambda cmd: str2scm(os.popen(f'sh -c \\'{scm2str(cmd)}\\'').read())),")

   (define (list-dir dir-name) (##list-dir dir-name))
   (define (current-directory) (##current-directory)))
  ((host c)
   (define-primitive
     (##cmd-line)
     (use argv list2scm str2scm)
     "{
     push2(list2scm(argv, argc), PAIR_TAG);
     break;
     }")

   (define-primitive
     (##shell-cmd cmd)
     (use scm2str str2scm c/stdio)
     "{
     PRIM1();
     char* command = scm2str(x);
     FILE* process = popen(command, \"r\");
     if (process == NULL) {
       printf(\"Failed to run command '%s'\", command);
       exit(1);
     }
     char buffer[256];
     int len = 0;
     obj chrs = NIL;
     obj last_letter = NIL;
     // FIXME: Potential problem if a GC happens in the middle of this loop. 
     // Sorry future person fixing this, hopefully you are not me haha
     while (fgets(buffer, sizeof(buffer), process) != NULL) {
      int i = 0;
      while (buffer[i++]);
      len += i;
      obj next_last_letter = TAG_RIB(alloc_rib(TAG_NUM(buffer[i-1]), NIL, PAIR_TAG));
      obj chunk = next_last_letter;
      for (int j = i - 2; j >= 0; j--) chunk = TAG_RIB(alloc_rib(TAG_NUM(buffer[j]), chunk, PAIR_TAG));
      if (chrs == NIL) chrs = chunk;
      else CDR(last_letter) = chunk;
      last_letter = next_last_letter;
     }
     pclose(process);
     free((void *) command);
     push2(TAG_RIB(alloc_rib(chrs, TAG_NUM(len), STRING_TAG)), PAIR_TAG);
     break;
     }"))

  ((host hs)
   (define-primitive
     (##cmd-line)
     (use argv list2scm)
     " , (getArgs >>= \\args -> getProgName >>= \\progName -> (mapM toRibString (progName : args)) >>= toRibList) >>= push"
   )))

(define (cmd-line) (##cmd-line))
(define (shell-cmd cmd . args) (##shell-cmd (string-append cmd " " (string-concatenate args " "))))
