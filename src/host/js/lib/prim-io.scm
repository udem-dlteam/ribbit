(if-feature js/web
  (begin
    ;; web primitives :
    (define-primitive
      (##stdin-fd)
      (use js/node/fs)
      "() => push(0),")

    (define-primitive
      (##stdout-fd)
      (use js/node/fs)
      "() => push(1),")

    (define-primitive 
      (##get-fd-input-file filename)
      (use js/node/fs scm2str)
      "prim1((x) => {console.log('cannot open file in web mode'); return FALSE;}),")

    (define-primitive
      (##get-fd-output-file filename)
      (use js/node/fs scm2str)
      "prim1((c) => {console.log('cannot open output file in web mode'); return FALSE;}),")

    (define-primitive
      (##read-char-fd fd)
      (use js/node/fs)
      "getchar,")

    (define-primitive
      (##write-char-fd ch fd)
      (use js/node/fs)
      "prim2((fd, ch) => {putchar(ch); return TRUE;}),")

    (define-primitive
      (##close-input-fd fd)
      (use js/node/fs)
      "prim1((_)=> console.err('cannot close input port in web mode')),"))

  (begin
    ;; without web enabled
    (define-primitive
      (##stdin-fd)
      (use js/node/fs)
      "() => push(0),")
    
    (define-primitive
      (##stdout-fd)
      (use js/node/fs)
      "() => push(1),")
    
    (define-primitive 
      (##get-fd-input-file filename)
      (use js/node/fs scm2str)
      "prim1(filename => {try{return fs.openSync(scm2str(filename), 'r')}catch{return FALSE}}),")
    
    (define-primitive
      (##get-fd-output-file filename)
      (use js/node/fs scm2str)
      "prim1(filename => {try{return fs.openSync(scm2str(filename), 'w')}catch{return FALSE}}),")
    
    (define-primitive
      (##read-char-fd fd)
      (use js/node/fs)
      "prim1(fd => {
      let buf=Buffer.alloc(1); 
      let ch=fs.readSync(fd, buf) === 0 ? NIL : buf[0]; 
      return ch;
      }),")
    
    (define-primitive
      (##write-char-fd ch fd)
      (use js/node/fs)
      "prim2((fd, ch) => {
      return fs.writeSync(fd, String.fromCodePoint(ch), null, 'utf8');})
      ,")
    
    (define-primitive
      (##close-input-fd fd)
      (use js/node/fs)
      "prim1(fd => fs.closeSync(fd)),")))

(define (##close-output-fd port) (##close-input-fd port))

