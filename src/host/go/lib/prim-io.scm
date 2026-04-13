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
    push(tagNum(file.Fd())) // push file descriptor as a number
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
  // Second argument is not the name of the file but debugging infos
  file:= os.newFile(fd, \"\")
  b1 := make([]byte, 1)
  if n, err := file.Read(b1); err == nil && n > 0 {
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
  // Second argument is not the name of the file but debugging infos
  file := os.newFile(fd, \"\")
  ch := pop()
  b1 := []byte{byte(ch)}
  if _, err := file.Write(b1); err != nil {
    panic(err)
  }
  file.Sync()
")

; Do not close file descriptors accessed with .Fd(). They are closed by the GC
; and, according to the doc, must not be closed manually. (see docs)
(define %%close-input-fd %%id)
(define %%close-output-fd %%id)

;(define-primitive
;  (%%close-input-fd fd)
;  (use py/io)
;  "
;  ")

;(define-feature
;  %%close-output-fd
;  (use %%close-input-fd))

;(define (%%close-output-fd port) (%%close-input-fd port))
