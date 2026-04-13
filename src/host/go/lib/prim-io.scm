(define-primitive
  (%%stdin-fd)
  (use go/os)
  "{push(os.Stdin)}")

(define-primitive
  (%%stdout-fd)
  (use go/os)
  "{push(os.Stdout)}")

(define-primitive
  (%%get-fd-input-file filename)
  (use go/os scm2str)
  "filenameScheme := pop()
  filename := scm2str(filenameScheme)
  if file, err := os.Open(filename); err == nil {
    push(file)
  } else {
    push(FALSE)
  }")

(define %%get-fd-output-file %%get-fd-input-file)

(define-feature %%read-fd-output-file (use %%get-fd-input-file))

(define-primitive
  (%%read-char-fd fd)
  (use py/io)
  "
  fd := pop()
  b1 := make([]byte, 1)
  if n, err := fd.Read(b1); err == nil && n > 0 {
    push(tagNum(int(b1[0])))
  } else {
    push(NIL)
  }
  ")

(define-primitive
  (%%write-char-fd ch fd)
  (use py/io)
  "
  fd := pop()
  ch := pop()
  b1 := []byte{byte(ch)}
  if _, err := fd.Write(b1); err != nil {
    panic(err)
  }
  fd.Sync()
")

(define-primitive
  (%%close-input-fd fd)
  (use py/io)
  "
  fd := pop()
  if err := fd.Close(); err != nil {
    panic(err)
  }
  ")

(define-feature
  %%close-output-fd
  (use %%close-input-fd))

(define (%%close-output-fd port) (%%close-input-fd port))
