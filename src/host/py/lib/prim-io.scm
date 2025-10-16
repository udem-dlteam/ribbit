(define-primitive
  (%%stdin-fd)
  (use py/io) 
  "lambda: push(sys.stdin),")

(define-primitive
  (%%stdout-fd)
  (use py/io) 
  "lambda: push(sys.stdout),")

(define-primitive 
  (%%get-fd-input-file filename)
  (use py/io scm2str)
  "prim1(lambda filename: open(file, mode='r') if os.path.exists(file:=scm2str(filename)) else FALSE),")

(define-primitive
  (%%get-fd-output-file filename)
  (use py/io scm2str)
  "prim1(lambda filename: open(scm2str(filename), mode='w')),")

(define-primitive
  (%%read-char-fd fd)
  (use py/io)
  "prim1(lambda fd: ord(ch) if (ch:=fd.read(1)) else NIL),")

(define-primitive
  (%%write-char-fd ch fd)
  (use py/io)
  "prim2(lambda fd, ch: [fd.write(chr(ch)), fd.flush()]),")

(define-primitive
  (%%close-input-fd fd)
  (use py/io)
  "prim1(lambda fd: fd.close()),")

(define-feature 
  %%close-output-fd
  (use %%close-input-fd))

(define (%%close-output-fd port) (%%close-input-fd port))
