(define-primitive

  (%%stdin-fd)
  (use go/os go/syscall)
  "{
    push(tagNum((int)(os.Stdin.Fd())))
  }")

(define-primitive
  (%%stdout-fd)
  (use go/os go/syscall)
  "{
  push(tagNum((int)(os.Stdout.Fd())))
  }")

(define-primitive
  (%%get-fd-input-file filename)
  (use go/os scm2str)
  "
  filenameScheme := pop()
  filename := scm2str(filenameScheme)

  if fd, err := syscall.Open(filename, syscall.O_RDONLY, 0644); err == nil {
    push(tagNum(fd)) // push file descriptor as a number
  } else {
    push(FALSE)
  }")


(define-primitive
  (%%get-fd-output-file filename)
  (use go/os scm2str)
  "filenameScheme := pop()
  filename := scm2str(filenameScheme)
  if fd, err := syscall.Open(filename, syscall.O_RDWR | syscall.O_APPEND | syscall.O_CREAT, 0644); err == nil {
    push(tagNum(fd)) // push file descriptor as a number
  } else {
    push(FALSE)
  }")


(define-primitive
  (%%read-char-fd fd)
  (use go/os go/syscall)
  "
  fd := pop().Value()
  b1 := make([]byte, 1)
  if n, err := syscall.Read(fd, b1); err == nil && n > 0 {
    push(tagNum(int(b1[0])))
  } else {
    push(NIL)
  }
  ")

(define-primitive
  (%%write-char-fd ch fd)
  (use go/os go/syscall)
  "
  fd := pop().Value()
  ch := pop().Value()
  b1 := []byte{byte(ch)}
  if _, err := syscall.Write(fd, b1); err != nil {
    panic(err)
  }
  push(TRUE)
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
