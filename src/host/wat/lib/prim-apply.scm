(define-primitive
  (%%apply f args)
  "
  (local.set $temp1 (call $pop<-stack)) ;; arg
  (local.set $temp0 (call $pop<-stack))
  (call $store-temp1 (local.get $temp0)) ;; save from GC
  (local.set $temp0 (i32.const 0)) ;; num_args

  (block $break
    (loop $continue
      (br_if $break (i32.eq (local.get $temp1) (global.get $NIL*)))
      (call $push->stack (local.get $temp1) (global.get $#pair))
      (local.set $temp1 (call $field0 (global.get $stack*)))
      (call $field0-set! (global.get $stack*) (call $field0 (local.get $temp1)))
      (local.set $temp1 (call $field1 (local.get $temp1)))
      (local.set $temp0 (i32.add (local.get $temp0) (i32.const 1)))
      (br $continue)))

  (call $push->stack (call $tag-num (local.get $temp0)) (global.get $#pair))
  (return (call $load-temp1))
  ")
