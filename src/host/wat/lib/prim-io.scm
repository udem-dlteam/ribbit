(define-primitive
  (##stdin-fd)
  (use wat/io)
  "(call $push->stack (call $tag-num (i32.const 0)) (global.get $#pair))")

(define-primitive
  (##stdout-fd)
  (use wat/io)
  "(call $push->stack (call $tag-num (i32.const 1)) (global.get $#pair))")

(define-primitive
  (##get-fd-input-file filename)
  (use wat/io scm2str)
  "(call $push->stack
        (call $tag-num (call $openFile
                             (call $rib2str* (call $pop<-stack))
                             (i32.const 0)))
        (global.get $#pair))")

(define-primitive
  (##get-fd-output-file filename)
  (use wat/io scm2str)
  "(call $push->stack 
        (call $tag-num (call $openFile
                             (call $rib2str* (call $pop<-stack))
                             (i32.const 1)))
        (global.get $#pair))")

(define-primitive
  (##read-char-fd fd)
  (use wat/io)
  "(local.set $temp0 (call $readChar (call $untag-num (call $pop<-stack))))
   (call $push->stack
         (if (result i32) (i32.eq (local.get $temp0) (i32.const -1))
           (then (global.get $NIL*))
           (else (call $tag-num (local.get $temp0))))
         (global.get $#pair))")

(define-primitive
  (##write-char-fd ch fd)
  (use wat/io)
  "(local.set $temp0 (call $untag-num (call $pop<-stack)))
   (call $writeChar (local.get $temp0) (call $untag-num (call $pop<-stack)))
   (call $push->stack (global.get $TRUE*) (global.get $#pair))")

(define-primitive
  (##close-input-fd fd)
  (use wat/io)
  "(call $closeFd (call $untag-num (call $pop<-stack)))")

;;(call $push->stack (global.get $NIL*) (global.get $#pair))
(define (##close-output-fd port) (##close-input-fd port))


(if-feature
  (or (not quiet) (not hide-frog))
  (define (welcome-msg)
     (display 
"               ____________________
               |                    |
               | Welcome to Ribbit! |
    ,-.        |                    |
    \\_/        | - Wasp             |
  <(|||:}  --- |____________________|
    / \\
    `-^")))
