(module
  ;; @@(feature wat/wasi
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (func $logInteger (param i32))
  (func $logChar (param i32))
  ;; )@@

  ;; @@(feature (or wat/js (not wat/wasi))
  ;; @@(feature debug
  (import "import" "logInteger" (func $logInteger (param i32)))
  ;; )@@
  (import "import" "logChar" (func $logChar (param i32)))
  (import "import" "getChar" (func $getChar (result i32)))
  (import "import" "exit" (func $exit (param i32)))
  ;; @@(feature wat/io
  (import "import" "openFile" (func $openFile (param i32) (param i32) (result i32)))
  (import "import" "readChar" (func $readChar (param i32) (result i32)))
  (import "import" "writeChar" (func $writeChar (param i32 i32)))
  (import "import" "closeFd" (func $closeFd (param i32)))
  ;; )@@
  ;; )@@

  ;; Multi-memory
  (memory $shareholder 2)  ;; Small temp heap for operations
  (global $shareholder-next-free (mut i32) (i32.const 1000))

  ;;; Returns
  (func $alloc-shareholder (param $size i32) (result i32)
    (local $addr* i32)
    (local.set $addr* (global.get $shareholder-next-free))
    (global.set $shareholder-next-free
                (i32.add (local.get $size)
                         (global.get $shareholder-next-free)))
    (local.get $addr*))

  ;; Store the bytecode in a string
  ;; @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
  ;; @@(feature debug
  (data (i32.const 1000) ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y\00")
  ;; Pointer to the input chars
  (global $input_cursor* (mut i32) (i32.const 1000))
  ;; )@@
  ;; @@(feature (not debug)
  (data (i32.const 2000) ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y")
  ;; Pointer to the input chars
  (global $input_cursor* (mut i32) (i32.const 2000))
  ;; )@@
  ;; )@@

  ;;; <UTILS>
  (func $+1 (param i32) (result i32)
    (i32.add (local.get 0) (i32.const 1)))

  (func $+2 (param i32) (result i32)
    (i32.add (local.get 0) (i32.const 2)))
  
  (func $+3 (param i32) (result i32)
    (i32.add (local.get 0) (i32.const 3)))

  ;;; </UTILS>

  ;; Read the symbol table utility function
  (func $get_byte (result i32)
    (local $old_cursor i32)
    (local.set $old_cursor (global.get $input_cursor*))
    (global.set $input_cursor* 
                (i32.add (global.get $input_cursor*) (i32.const 1)))
    (i32.load8_u (memory $shareholder) (local.get $old_cursor)))

  ;; Instruction graph utility function
  (func $get_code (result i32)
    (local $byte i32)
    (local.set $byte (i32.sub (call $get_byte) (i32.const 35)))
    (if (result i32) (i32.lt_s (local.get $byte) (i32.const 0))
      (then
        (i32.const 57))
      (else
        (local.get $byte))))

  ;; Short vs long instuction encoding
  (func $get_int (param $n i32) (result i32)
    (local $code i32)
    (local.set $code (call $get_code))
    (local.set $n (i32.mul (local.get $n) (i32.const 46)))
    (if (result i32) (i32.lt_s (local.get $code) (i32.const 46))
      (then
        (i32.add (local.get $code) (local.get $n)))
      (else
        (call $get_int 
              (i32.sub 
                (i32.add (local.get $code) (local.get $n))
                (i32.const 46))))))

  (global $op-jump/call i32 (i32.const 20))
  (global $op-get i32 (i32.const 30))
  (global $op-set i32 (i32.const 0))
  (global $op-const i32 (i32.const 10))
  (global $op-closure-const i32 (i32.const 11))
  (global $op-if i32 (i32.const 4))

  (func $get-op (param $op-idx i32) (result i32)
    (if (i32.eqz (local.get $op-idx))
      (then (return (global.get $op-jump/call))))

    (if (i32.eq (local.get $op-idx) (i32.const 1))
      (then (return (global.get $op-get))))

    (if (i32.eq (local.get $op-idx) (i32.const 2))
      (then (return (global.get $op-set))))

    (if (i32.eq (local.get $op-idx) (i32.const 3))
      (then (return (global.get $op-const))))

    (if (i32.eq (local.get $op-idx) (i32.const 4))
      (then (return (global.get $op-closure-const))))

    (if (i32.eq (local.get $op-idx) (i32.const 5))
      (then (return (global.get $op-if))))

    (call $logChar (i32.const 68))
    (unreachable))

  ;;; 64 * (64 * 1024) = 4,194,304 bytes
  (memory $ribeep 64)    ;; Heap specific for ribs

  (global $alloc (mut i32) (i32.const 0))
  (global $heap-bot i32 (i32.const 0))
  ;;; 64 * (64 * 1024) / 2 = 4,194,304 / 2 = 2,097,152 bytes
  ;;; Allow space for only whole ribs, else the gc cuts right through a rib
  ;;(global $heap-mid i32 (i32.const 4_194_288));; 2_097_144))
  ;;(global $alloc-limit (mut i32) (i32.const 4_194_288)) ;; 4_194_288));;  

  (global $heap-mid i32 (i32.const 2_097_144))
  (global $alloc-limit (mut i32) (i32.const 2_097_144))

  (global $scan* (mut i32) (i32.const 0))

  ;; @@(feature (not no-gc)
  (func $copy (param $obj* i32) (result i32)
    ;;(local $obj* i32)
    (local $f0 i32)
    (local $copy i32)

    ;;(local.set $obj* (i32.load (memory $ribeep) (global.get $scan*)))
    ;;(local.set $obj* (global.get $scan*))
    (local.set $copy (local.get $obj*))

    (if (call $rib*? (local.get $obj*))
      (then
        (local.set $f0 (call $field0 (local.get $obj*)))
        ;;; stack* is our broken heart value
        (if (i32.eq (local.get $f0) (i32.const -1))
          (then
            (local.set $copy (call $field1 (local.get $obj*))))
          (else
            (local.set $copy (global.get $alloc))
            (call $field0-set! (local.get $obj*) (i32.const -1))
            (drop (call $alloc-val (local.get $f0)))
            (drop (call $alloc-val (call $field1 (local.get $obj*))))
            (drop (call $alloc-val (call $field2 (local.get $obj*))))
            (call $field1-set! (local.get $obj*) (local.get $copy))))
        ;;(if (local.get $store?)
        ;;  (then 
        ;;    (i32.store (memory $ribeep) 
        ;;               (global.get $scan*)
        ;;               (local.get $copy))))
        ;;(global.set $scan* (local.get $copy))
        ))

    (global.set $scan* (i32.add (global.get $scan*) (i32.const 4)))
    (local.get $copy))

  (func $gc
    (local $to-space i32)

    ;; @@(feature debug-gc (use debug)
    (local $from-space i32)
    (local.set $from-space 
               (if (result i32) (i32.eq (global.get $alloc-limit) (global.get $heap-mid))
                 (then (global.get $heap-bot))
                 (else (global.get $heap-mid))))
    (call $logOp (i32.const 71)) ;; --- G:
    (call $logInteger (i32.sub (global.get $alloc) (local.get $from-space)))
    (call $newline)
    (call $logOp (i32.const 77)) ;; --- M:
    (call $print-mem (i32.const 20) (local.get $from-space))
    (call $newline)
    ;; )@@

    (local.set $to-space 
               (if (result i32) (i32.eq (global.get $alloc-limit) (global.get $heap-mid))
                 (then (global.get $heap-mid))
                 (else (global.get $heap-bot))))
    (global.set $alloc-limit (i32.add (local.get $to-space) (global.get $heap-mid)))
    (global.set $alloc (local.get $to-space))

    (global.set $scan* (global.get $stack*))
    (global.set $stack* (call $copy (global.get $stack*)))

    (global.set $scan* (global.get $pc*))
    (global.set $pc* (call $copy (global.get $pc*)))

    (global.set $scan* (global.get $FALSE*))
    (global.set $FALSE* (call $copy (global.get $FALSE*)))

    (global.set $scan* (local.get $to-space))
    (block $break
      (loop $continue
        (br_if $break (i32.eq (global.get $scan*) (global.get $alloc)))
        (i32.store (memory $ribeep)
                   (global.get $scan*)
                   (call $copy (i32.load (memory $ribeep) (global.get $scan*))))
        ;;(if (i32.eqz (i32.rem_u (global.get $scan*) (i32.const 10_000)))
        ;;  (then
        ;;    (call $print-mem (i32.const 20) (local.get $to-space))
        ;;    (call $newline)))

        ;;(call $logInteger (global.get $scan*))
        ;;(call $logChar (i32.const 32))
        ;;(call $logInteger (global.get $alloc))
        ;;(call $newline)
        (br $continue)))

    (global.set $TRUE* (call $field0 (global.get $FALSE*)))
    (global.set $NIL* (call $field1 (global.get $FALSE*)))
    
    ;; @@(feature debug-gc (use debug)
    (call $print-mem (i32.const 20) (local.get $to-space))
    (call $newline)
    (call $newline)
    (call $logOp (i32.const 71)) ;; --- G:
    (call $logInteger (i32.sub (global.get $alloc) (local.get $to-space)))
    (call $newline)
    ;; )@@
    )

  ;; @@(feature debug-gc (use debug)
  (func $print-mem (param $limit i32) (param $start i32)
    (local $i i32)
    (local $ptr i32)
    (local.set $i (i32.const 0))
    (local.set $ptr (local.get $start))

    (block $break 
      (loop $continue 
        (br_if $break (i32.eq (local.get $i) (local.get $limit)))

        (call $logChar (i32.const 40))
        (call $logInteger (i32.load 
                            (memory $ribeep)
                            (local.get $ptr)))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
        (call $logChar (i32.const 32))
        (call $logInteger (i32.load 
                            (memory $ribeep)
                            (local.get $ptr)))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
        (call $logChar (i32.const 32))
        (call $logInteger (i32.load 
                            (memory $ribeep)
                            (local.get $ptr)))
        (local.set $ptr (i32.add (local.get $ptr) (i32.const 4)))
        (call $logChar (i32.const 41))
        (call $logChar (i32.const 32))

        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $continue))))
  ;; )@@
  ;; )@@


  (global $tagged-num-0 i32 (i32.const 1)) ;; (0 << 1) | 1

  (global $#pair i32 (i32.const 1)) ;; (0 << 1) | 1 ;; pre-shifted
  (global $#closure i32 (i32.const 3)) ;; (1 << 1) | 1 ;; pre-shifted
  (global $#symbol i32 (i32.const 5)) ;; (2 << 1) | 1 ;; pre-shiften
  (global $#string i32 (i32.const 7)) ;; (3 << 1) | 1 ;; pre-shifted
  (global $#singleton i32 (i32.const 11)) ;; (5 << 1) | 1 ;; pre-shifted

  ;; Addresses already known
  (global $FALSE* (mut i32) (i32.const 0))  ;; address 0 in the memory ribeep
  (global $TRUE* (mut i32) (i32.const 12))  ;; address 3x4 in the memory ribeep
  (global $NIL* (mut i32) (i32.const 24))   ;; address 3x4 + 3x4 in the memory ribeep

  ;; Addresses already known
  ;;(global $TEMP1* i32 (i32.const 12))  ;; (field0 $TRUE)

  (func $store-temp1 (param $val i32)
    (call $field0-set! (global.get $TRUE*) (local.get $val)))

  (func $load-temp1 (result i32)
    (call $field0 (global.get $TRUE*)))

  ;;(global $TEMP2* i32 (i32.const 16))  ;; (field1 $TRUE)
  ;;(global $TEMP3* i32 (i32.const 24))  ;; (field0 $NIL)
  ;;(global $TEMP4* i32 (i32.const 28))  ;; (field1 $NIL)
  ;; TAG(0) == (0 << 1 | 1) == 1

  (global $symbol-table* (mut i32) (i32.const 1)) ;; global but not root (tagged-num-0)
  
  ;; pre-allocate 3 ribs :=> #f, #t, ()
  ;; note : false can be used as the root for the gc
  (func $init-globals
    (call $alloc-rib ;; FALSE
          (i32.const 0)
          (i32.const 0)
          (global.get $#singleton))
    (drop)
    
    (call $alloc-rib ;; TRUE
          (global.get $tagged-num-0)
          (global.get $tagged-num-0)
          (global.get $#singleton))
    (drop)

    (call $alloc-rib ;; NIL
          (global.get $tagged-num-0)
          (global.get $tagged-num-0)
          (global.get $#singleton)) ;; symbol tag
    (drop)

    (call $field0-set! (global.get $FALSE*) (global.get $TRUE*))
    (call $field1-set! (global.get $FALSE*) (global.get $NIL*)))

  ;; Assumes pair or nil!
  ;; returns length of the list
  (func $lst-lenght (param $lst* i32) (result i32)
    (local $size i32)
    (local.set $size (i32.const 0))
    (block $break
      (loop $continue
        (br_if $break (i32.eq (local.get $lst*) (global.get $NIL*)))
        (local.set $size (i32.add (local.get $size) (i32.const 1)))
        (local.set $lst* (call $field1 (local.get $lst*)))
        (br $continue))) ;; assumes list, check prior to call
    (call $tag-num (local.get $size))) ;; return size of the list

  ;;; ptrs in $ribeep
  (global $stack* (mut i32) (i32.const 1)) ;; $stack* = tagged-num-0
  (global $pc* (mut i32) (i32.const 1))

  (export "memory" (memory $shareholder))

  ;; tag num (num) :=> ((obj)(num) << 1 )| 1)
  ;; addresses are always even
  (func $tag-num (param $num i32) (result i32)
    (i32.or (i32.shl (local.get $num) (i32.const 1)) (i32.const 1)))

  ;; untag num  (x) :=> ((x) >> 1)
  ;; check actuall value
  (func $untag-num (param $tagged-num i32) (result i32)
    (i32.shr_s (local.get $tagged-num) (i32.const 1)))

  ;; is num :=> (x&1)
  (func $num? (param $tagged-obj i32) (result i32)
    (i32.and (local.get $tagged-obj) (i32.const 1)))

  ;; is rib :=> (!is num(x))
  (func $rib*? (param $tagged-obj i32) (result i32)
    (i32.eqz (call $num? (local.get $tagged-obj))))

  ;; Get Value field 0 := [*,_,_]
  (func $field0 (param $rib* i32) (result i32)
    (i32.load (memory $ribeep) (local.get $rib*)))

  ;; Get Value field 1 := [_,*,_]
  (func $field1 (param $rib* i32) (result i32)
    (i32.load (memory $ribeep) 
              (i32.add 
                (i32.const 4) ;; 1 x 4 bytes
                (local.get $rib*))))

  ;; Get Value field 2 := [_,_,*]
  (func $field2 (param $rib* i32) (result i32)
    (i32.load (memory $ribeep)
              (i32.add (i32.const 8) ;; 2 x 4 bytes
                       (local.get $rib*))))

  ;; Set field 0 - (set value @ mem integer or short encoding?)
  (func $field0-set! (param $rib* i32) (param $val i32)
    (i32.store (memory $ribeep) (local.get $rib*) (local.get $val)))

  ;; Set field 1 - (set value @ mem (+ rib 4) (val))
  (func $field1-set! (param $rib* i32) (param $val i32)
    (i32.store (memory $ribeep) ;; store in [_,*,_]
               (i32.add 
                 (local.get $rib*) 
                 (i32.const 4))
               (local.get $val))) ;; value here

  ;; Set field 2 - (set value @ mem ((+ rib 8)(value))
  (func $field2-set! (param $rib* i32) (param $val i32)
    (i32.store (memory $ribeep) ;; store in [_,_,*]
               (i32.add 
                 (local.get $rib*)
                 (i32.const 8))
               (local.get $val))) ;; value here

  (func $check-gc 
    (if (i32.eq (global.get $alloc) (global.get $alloc-limit))
      (then
        (call $gc) ;; @@(feature (not no-gc))@@
        (nop) ;; @@(feature no-gc)@@
        )))

  ;; Allocate memory from stack to value
  ;; alloc == next free address
  (func $alloc-val (param $val i32) (result i32)
    (local $val* i32)
    (local.set $val* (global.get $alloc))
    (i32.store (memory $ribeep) (local.get $val*) (local.get $val)) ;; Allocate from the rib specific memheap
    (global.set $alloc (i32.add (global.get $alloc) (i32.const 4))) ;; Alloc int
    (local.get $val*))

  ;; Rib containing: [field 0, field 1, field 2]
  ;; Address that we know is followed by 3 integers
  (func $alloc-rib (param i32 i32 i32) (result i32)
    (local $rib* i32)
    (local $old-stack* i32)
    (call $push->stack (local.get 0) (local.get 1))
    (local.set $old-stack* (call $field1 (global.get $stack*)))
    (local.set $rib* (global.get $stack*))

    (call $field1-set! (local.get $rib*) (call $field2 (local.get $rib*)))
    (call $field2-set! (local.get $rib*) (local.get 2))

    (global.set $stack* (local.get $old-stack*))
    (local.get $rib*))

  (func $alloc-rib2 (param i32 i32 i32) (result i32)
    (local $rib* i32)
    (local $old-stack* i32)
    (call $push->stack (local.get 0) (local.get 2))
    (local.set $old-stack* (call $field1 (global.get $stack*)))
    (local.set $rib* (global.get $stack*))

    (call $field1-set! (local.get $rib*) (local.get 1))

    (global.set $stack* (local.get $old-stack*))
    (local.get $rib*)) ;; one rib

  ;; push value where [_,_,field 2] is the stack
  ;; use to push to stack
  (func $push->stack (param $field0 i32) (param $field2 i32)
    (local $rib* i32)
    (local.set $rib* (call $alloc-val (local.get $field0)))
    (drop (call $alloc-val (global.get $stack*)))
    (drop (call $alloc-val (local.get $field2)))
    (global.set $stack* (local.get $rib*))
    (call $check-gc))

  ;; pop = () => { let x = stack[0]; stack = stack[1]; return x; };
  ;; use to pop from stack
  (func $pop<-stack (result i32)
    (local $x i32)
    (local.set $x (call $field0 (global.get $stack*)))
    (global.set $stack* (call $field1 (global.get $stack*)))
    (local.get $x))

  (func $lst-ref (param $lst* i32) (param $num i32) (result i32)
    (call $field0 (call $list-tail (local.get $lst*) (local.get $num))))

  (func $sym-ref (param $num i32) (result i32)
    (call $lst-ref (global.get $symbol-table*) (local.get $num)))

  ;; Create a symbol
  (func $make-sym (param $sym-name* i32) (result i32)
    ;; Locals
    (local $lst* i32)
    (local $sym* i32)
    (local $root* i32)

    ;; Set lst, sym, and root

    ;; *lst = alloc-rib()
    (local.set $lst* 
      (call $alloc-rib 
        (local.get $sym-name*) ;; field0 := [sym-name*,_,_]
        (call $lst-lenght (local.get $sym-name*)) ;; field1 := [sym-name*,lst-len,_]
        (global.get $#string))) ;; field2 := [sym-name*,lst-len,#string]

    ;; *sym = alloc-rib()
    (local.set $sym* 
      (call $alloc-rib 
        (global.get $FALSE*) ;; field0
        (local.get $lst*) ;; field1
        (global.get $#symbol))) ;; field2

    ;; *root = alloc-rib()
    (local.set $root* 
      (call $alloc-rib 
        (local.get $sym*) ;; field0
        (global.get $symbol-table*) ;; field1
        (global.get $#pair))) ;;field2

    ;; return root
    (local.get $root*))
        

  (func $set-global! (param $rib* i32)
    (call $field0-set! (call $field0 (global.get $symbol-table*))
          (local.get $rib*))
    (global.set $symbol-table* (call $field1 (global.get $symbol-table*))))

  ;; Build the symbol-table
  (func $build-sym-table
    ;; locals
    (local $num i32)
    (local $accum i32)
    (local $char i32)

    ;; init num = get_int(0)
    (local.set $num (call $get_int (i32.const 0)))

    ;; while num > 0, loop
    (loop $while
      (if
        (i32.gt_s ;; if num > 0
          (local.get $num)
          (i32.const 0))
        (then
          ;; num--
          (local.set $num
            (i32.sub (local.get $num) (i32.const 1)))
          ;; Update symbol table
          (global.set $symbol-table*
            (call $make-sym (global.get $NIL*)))

          (br $while)))) ;; loop

    ;; init accum = NIL
    (local.set $accum (global.get $NIL*))

    ;; Parse chars
    (block $break
      (loop $whileTrue
        ;; Get the next char
        (local.set $char (call $get_byte))

        ;; if char == ',' make symbol accum
        (if
          (i32.eq (local.get $char) (i32.const 44)) ;; 44 = ','
          (then
            ;; symbol-table = make-sym(accum)
            (global.set $symbol-table* 
              (call $make-sym (local.get $accum)))
            (local.set $accum (global.get $NIL*)) ;; accum = NIL (reset)
            (br $whileTrue))) ;; continue

        ;; if char == ';' break out of loop!
        (br_if $break
          (i32.eq (local.get $char) (i32.const 59))) ;; 59 == ';'

        ;; Update accum
        (local.set $accum 
          (call $alloc-rib 
                (call $tag-num (local.get $char))
                (local.get $accum)
                (global.get $#pair)))

        ;; loop until break
        (br $whileTrue)))

    ;; Update symbol table
    (global.set $symbol-table*
      (call $make-sym (local.get $accum))));; End of build-sym-table

  ;; Rib Graph Decoder
  (func $rib-graph-decode
    ;; we will refer to the array using the get-op
    (local $obj i32)
    (local $d i32)
    (local $op i32)
    (local $num i32)
    (local $char* i32)
    
    (block $exit 
      (loop $continue ;; while 1
        (local.set $num (call $get_code)) ;; num = get_code()
        (local.set $op (i32.const -1)) ;; op - -1
        (local.set $obj (local.get $num)) ;; obj = num

        ;; Determine the operation and adjust obj
        (block $inner-exit 
          (loop $inner-continue
            ;; while (obj > 2 + (d = get-op(++op)))
            (local.set $op (call $+1 (local.get $op))) ;; op += 1
            (local.set $d (call $get-op (local.get $op))) ;; d = get-op(op)

            (br_if $inner-exit 
              (i32.le_s (local.get $obj) (call $+2 (local.get $d)))) ;; obj > 2 + d, if false, break

            ;; obj -= d+ 3
            (local.set $obj (i32.sub (local.get $obj) (call $+3 (local.get $d))))
            (br $inner-continue)))

        ;; Decoding step
        (if (i32.gt_s (local.get $num) (i32.const 90))
          (then
            ;; if x > 90, op = IF instruction
            (local.set $op (global.get $op-if))
            (local.set $obj (call $pop<-stack)))
          (else
            ;; if !op || op == NIL
            ;; (if (i32.eq (local.get $op) (global.get $NIL*))
            (if (i32.eqz (local.get $op))
              (then
                (call $push->stack 
                      (global.get $tagged-num-0) 
                      (global.get $tagged-num-0))))

            ;; if obj >= d
            (local.set 
              $obj 
              (if (result i32) (i32.ge_s (local.get $obj) (local.get $d)) ;; if obj >= d
                (then
                  (if (result i32) (i32.eq (local.get $obj) (local.get $d)) ;; if obj == d
                    (then 
                      (call $tag-num 
                            (call $get_int (i32.const 0)))) ;; tag-num(get_int(0))
                    (else
                      ;; sym-ref(get_int(obj - d - 1))
                      (call $sym-ref 
                            (call $get_int
                                  (i32.sub 
                                    (i32.sub (local.get $obj) (local.get $d))
                                    (i32.const 1)))))))
                (else 
                  (if (result i32) (i32.lt_s (local.get $op) (i32.const 3)) ;; if op < 3
                    (then 
                      (call $sym-ref (local.get $obj))) ;; n = sym-ref(n)
                    (else 
                      (call $tag-num (local.get $obj))))))) ;; n = tag-num(n)

            ;; if op > 4
            (if (i32.gt_s (local.get $op) (i32.const 4))
              (then
                (local.set $obj
                  (call $alloc-rib 
                    (call $alloc-rib2
                      (local.get $obj)
                      (global.get $tagged-num-0)
                      (call $pop<-stack))
                    (global.get $NIL*)
                    (global.get $#closure)))
                (br_if 
                  $exit
                  (i32.eq (global.get $stack*) (global.get $tagged-num-0))) ;; stack == tagged-num-0
                (local.set $op (i32.const 3))) ;; INSTR_CONST
              ;; else if op > 0
              (else 
                (if (i32.gt_s (local.get $op) (i32.const 0))
                  (then 
                    (local.set $op (i32.sub (local.get $op) (i32.const 1)))) ;; op--
                  (else
                    (local.set $op (i32.const 0)))))))) ;; op = 0


        (local.set $char*
          (call $alloc-rib
            (call $tag-num (local.get $op))
            (local.get $obj)
            (call $field0 (global.get $stack*))))

        (call $field0-set!
          (global.get $stack*) 
          (local.get $char*))

        (br $continue))) ;; Top of stack = tag-rib(char*)

    (global.set $pc* 
      (call $field2 
            (call $field0 (local.get $obj))))) ;; Update program counter

  ;; @@(feature bool2scm 
  (func $bool2scm (param $val i32) (result i32)
    (if (result i32) (local.get $val)
      (then (global.get $TRUE*))
      (else (global.get $FALSE*))))
  ;; )@@

  (func $call-primitive (param $prim-idx i32) (result i32)
    (local $temp0 i32)
    (local $temp1 i32)
    ;; @@(primitives (gen "(if (result i32) (i32.eq (i32.const " index ") (local.get $prim-idx)) (then " body " (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop)")
    (if (result i32) (i32.eq (i32.const 0) (local.get $prim-idx)) (then ;; @@(primitive (%%rib a b c)
        (local.set $temp0 (call $pop<-stack))
        (local.set $temp1 (call $pop<-stack))
        (call $push->stack 
              (call $alloc-rib (call $pop<-stack) (local.get $temp1) (local.get $temp0)) 
              (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop)
    ;; )@@

    (if (result i32) (i32.eq (i32.const 1) (local.get $prim-idx)) (then ;; @@(primitive (%%id x)
        ;;nop
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 2) (local.get $prim-idx)) (then ;; @@(primitive (%%arg1 x y)
        (drop (call $pop<-stack))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 3) (local.get $prim-idx)) (then ;; @@(primitive (%%arg2 x y)
        (call $push->stack (call $pop<-stack) (drop (call $pop<-stack)) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 4) (local.get $prim-idx)) (then ;; @@(primitive (%%close rib)
        (local.set $temp0 (call $alloc-rib
                                (call $field0 (call $field0 (global.get $stack*)))
                                (call $field1 (global.get $stack*))
                                (global.get $#closure)))
        (call $field0-set! 
              (global.get $stack*)
              (local.get $temp0))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 5) (local.get $prim-idx)) (then ;; @@(primitive (%%rib? rib) (use bool2scm)
        (call $push->stack (call $bool2scm (call $rib*? (call $pop<-stack))) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 6) (local.get $prim-idx)) (then ;; @@(primitive (%%field0 rib) 
        (call $push->stack (call $field0 (call $pop<-stack)) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 7) (local.get $prim-idx)) (then ;; @@(primitive (%%field1 rib)
        (call $push->stack (call $field1 (call $pop<-stack)) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 8) (local.get $prim-idx)) (then ;; @@(primitive (%%field2 rib)
        (call $push->stack (call $field2 (call $pop<-stack)) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 9) (local.get $prim-idx)) (then ;; @@(primitive (%%field0-set! rib x) 
        (local.set $temp1 (call $pop<-stack))
        (local.set $temp0 (call $pop<-stack))
        (call $field0-set! (local.get $temp0) (local.get $temp1))
        (call $push->stack (local.get $temp1) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 10) (local.get $prim-idx)) (then ;; @@(primitive (%%field1-set! rib x) 
        (local.set $temp1 (call $pop<-stack))
        (local.set $temp0 (call $pop<-stack))
        (call $field1-set! (local.get $temp0) (local.get $temp1))
        (call $push->stack (local.get $temp1) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 11) (local.get $prim-idx)) (then ;; @@(primitive (%%field2-set! rib x) 
        (local.set $temp1 (call $pop<-stack))
        (local.set $temp0 (call $pop<-stack))
        (call $field2-set! (local.get $temp0) (local.get $temp1))
        (call $push->stack (local.get $temp1) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 12) (local.get $prim-idx)) (then ;; @@(primitive (%%eqv? x y) (use bool2scm)
        (call $push->stack 
              (call $bool2scm 
                    (i32.eq (call $pop<-stack) (call $pop<-stack)))
              (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 13) (local.get $prim-idx)) (then ;; @@(primitive (%%< x y) (use bool2scm)
        (local.set $temp1 (call $pop<-stack))
        (local.set $temp0 (call $pop<-stack))
        (call $push->stack (call $bool2scm (i32.lt_s (local.get $temp0) (local.get $temp1))) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 14) (local.get $prim-idx)) (then ;; @@(primitive (%%+ x y)
        (call $push->stack (i32.sub (i32.add (call $pop<-stack) (call $pop<-stack)) (i32.const 1)) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 15) (local.get $prim-idx)) (then ;; @@(primitive (%%- x y)
        (local.set $temp1 (call $pop<-stack))
        (local.set $temp0 (call $pop<-stack))
        (call $push->stack (i32.add (i32.sub (local.get $temp0) (local.get $temp1)) (i32.const 1)) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 16) (local.get $prim-idx)) (then ;; @@(primitive (%%* x y)
        (call $push->stack 
              (call $tag-num (i32.mul (call $untag-num (call $pop<-stack)) 
                                      (call $untag-num (call $pop<-stack))))
              (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 17) (local.get $prim-idx)) (then ;; @@(primitive (%%quotient x y)
        (local.set $temp1 (call $pop<-stack))
        (local.set $temp0 (call $pop<-stack))
        (call $push->stack 
              (call $tag-num (i32.div_s (call $untag-num (local.get $temp0))
                                        (call $untag-num (local.get $temp1))))
              (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 18) (local.get $prim-idx)) (then ;; @@(primitive (%%getchar)
        (call $push->stack (call $tag-num (call $getChar)) (global.get $#pair))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 19) (local.get $prim-idx)) (then ;; @@(primitive (%%putchar c)
        (call $logChar (call $untag-num (call $field0 (global.get $stack*))))
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@

    (if (result i32) (i32.eq (i32.const 20) (local.get $prim-idx)) (then ;; @@(primitive (%%exit n)
        (call $exit (call $untag-num (call $pop<-stack))) ;; Sets a trap and ends the program
        (return (global.get $tagged-num-0))) (else (i32.const 0))) (drop) 
    ;; )@@
    ;; )@@
    (call $logChar (i32.const 65))
    (unreachable)
    )


  (func $list-tail (param $lst* i32) (param $i i32) (result i32)
    (if (result i32) (i32.eqz (local.get $i))
      (then 
        (local.get $lst*))
      (else
        (call $list-tail 
              (call $field1 (local.get $lst*))
              (i32.sub (local.get $i) (i32.const 1))))))

  (func $get-opnd (param $o i32) (result i32)
    (call $field0 
          (if (result i32) (call $num? (local.get $o))
            (then
              (call $list-tail (global.get $stack*) (call $untag-num (local.get $o))))
            (else
              (local.get $o)))))

  (func $get-cont (result i32)
    (local $s* i32)
    (local.set $s* (global.get $stack*))
    (block $break
     (loop $continue
       (br_if $break (i32.ne (call $field2 (local.get $s*)) (global.get $#pair)))
       (local.set $s* (call $field1 (local.get $s*)))
       (br $continue)))
    (local.get $s*))

  (func $advance-pc
    (global.set $pc* (call $field2 (global.get $pc*))))


  (func $jump/call (param $proc* i32) (param $jump? i32) (param $instr i32)
    (local $s2* i32)
    (local $c2* i32)
    (local $nparams i32)
    (local $i i32)
    (local $k* i32)
    (local $nargs i32) ;; @@(feature arity-check)@@
    (local $variadic? i32) ;; @@(feature arity-check)@@

    ;; @@(feature debug-trace (use debug)
    (call $logInstr (if (result i32) (local.get $jump?) 
                      (then (i32.const 106))
                      (else (i32.const 99))))
    ;; )@@

    (loop $continue
      (if (call $num? (call $field0 (local.get $proc*)))
        (then ;; primitive call
          (drop (call $pop<-stack)) ;; @@(feature (and arity-check (not prim-no-arity)))@@
          (local.set $proc* (call $call-primitive (call $untag-num (call $field0 (local.get $proc*)))))
          (br_if $continue (call $rib*? (local.get $proc*)))
          (if (local.get $jump?)
            (then
              (global.set $pc* (call $get-cont))
              (call $field1-set! (global.get $stack*) (call $field0 (global.get $pc*)))))
          (call $advance-pc))
        (else ;; non-primitive call
          (local.set $nargs (call $untag-num (call $pop<-stack))) ;; @@(feature arity-check)@@
          (local.set $s2* (call $alloc-rib 
                                (global.get $tagged-num-0)
                                (local.get $proc*)
                                (global.get $#pair)))
          (local.set $proc* (call $field1 (local.get $s2*)))
          (call $field0-set! (global.get $pc*) (call $field0 (local.get $proc*)))

          ;; we store the value for nparams_vari in nparams
          ;; to save a variable
          (local.set $nparams ;; nparams_vari = (caar proc)
                     (call $untag-num 
                           (call $field0 
                                 (call $field0 
                                       (local.get $proc*)))))
          ;; @@(feature arity-check
          (local.set $variadic?  ;; variadic? = nparams_vari & 1
                     (i32.and (local.get $nparams) (i32.const 1)))
          ;; )@@

          (local.set $nparams ;; nparams = nparams_vari >> 1
                     (i32.shr_u (local.get $nparams) (i32.const 1)))

          ;; @@(feature arity-check
          (if (if (result i32) (local.get $variadic?) 
                (then (i32.gt_s (local.get $nparams) (local.get $nargs)))
                (else (i32.ne (local.get $nparams) (local.get $nargs))))
            (then 
              ;; Unexpected number of arguments nargs
              ;; @@(feature (or debug-err debug-trace) (use debug)
              (call $show-operand (call $field1 (global.get $pc*)))
              ;; )@@
              (call $logChar (i32.const 66))
              (unreachable)))
          ;; )@@

          ;; @@(feature rest-param (use arity-check)
          (local.set $nargs (i32.sub (local.get $nargs) (local.get $nparams)))
          (if (local.get $variadic?)
            (then 
              (local.set $c2* (global.get $NIL*)) ;; rest = NIL
              (local.set $i (i32.const 0))
              (block $for-break
                (loop $for-continue
                  (br_if $for-break (i32.ge_s (local.get $i) (local.get $nargs)))
                  (call $store-temp1 (local.get $s2*))
                  (local.set $c2* (call $alloc-rib 
                                        (call $pop<-stack) 
                                        (local.get $c2*)
                                        (global.get $#pair)))
                  (local.set $s2* (call $load-temp1))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $for-continue)))
              (local.set $s2* (call $alloc-rib 
                                    (local.get $c2*)
                                    (local.get $s2*)
                                    (global.get $#pair)))))
          ;; )@@

          (local.set $i (i32.const 0))
          (block $for-break
             (loop $for-continue
               (br_if $for-break (i32.ge_s (local.get $i) (local.get $nparams)))
               (local.set $s2* (call $alloc-rib 
                                     (call $pop<-stack)
                                     (local.get $s2*)
                                     (global.get $#pair)))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $for-continue)))

          (local.set $nparams (i32.add (local.get $nparams) (local.get $variadic?))) ;; @@(feature arity-check)@@

          (local.set $c2* (call $list-tail (local.get $s2*) (local.get $nparams)))

          (if (local.get $jump?)
            (then 
              (local.set $k* (call $get-cont))
              (call $field0-set! (local.get $c2*) (call $field0 (local.get $k*)))
              (call $field2-set! (local.get $c2*) (call $field2 (local.get $k*))))
            (else 
              (call $field0-set! (local.get $c2*) (global.get $stack*))
              (call $field2-set! (local.get $c2*) (call $field2 (global.get $pc*)))))

          (global.set $stack* (local.get $s2*))

          ;;; we reuse $i as `new_pc` cause it is unused here and we save a variable
          (local.set $i (call $field0 (global.get $pc*)))
          (call $field0-set! (global.get $pc*) (call $tag-num (local.get $instr)))
          (global.set $pc* (call $field2 (local.get $i)))))))

  ;; @@(feature scm2str
  (func $rib2str* (param $rib*#string i32) (result i32)
    (local $str-size i32)
    (local $str* i32)
    (local $next* i32)
    (local.set $str-size (call $field1 (local.get $rib*#string)))
    (local.set $str* (call $alloc-shareholder (call $+1 (local.get $str-size))))
    (local.set $next* (local.get $str*))
    (local.set $rib*#string (call $field0 (local.get $rib*#string)))

    (block $break 
      (loop $continue
        (br_if $break (i32.eq (local.get $rib*#string) (global.get $NIL*)))
        (i32.store8 (memory $shareholder)
                      (local.get $next*)
                      (call $untag-num 
                            (call $field0 (local.get $rib*#string))))
        (local.set $rib*#string (call $field1 (local.get $rib*#string)))
        (local.set $next* (i32.add (local.get $next*) (i32.const 1)))
        (br $continue)))

    (i32.store8 (memory $shareholder)
                  (i32.add 
                    (local.get $str*)
                    (i32.mul (i32.const 4) (local.get $str-size)))
                  (i32.const 0) ;; '\0'
                  )
    ;; TODO: Add loop to put
    (local.get $str*))
  ;; )@@

  (func $run
    ;; current running instruction
    (local $instr i32)

    ;; run loop
    (block $break
      (loop $continue
        (local.set $instr (call $untag-num (call $field0 (global.get $pc*))))
        (if (i32.eq (local.get $instr) (i32.const 5)) ;; HALT
          (then
            ;;(unreachable) ;; halts
            (br $break)))

        (if (i32.eq (local.get $instr) (i32.const 0)) ;; jump/call
          (then
            (call $jump/call 
                  ;; proc*
                  (call $get-opnd (call $field1 (global.get $pc*)))
                  ;; jump?
                  (i32.eq (call $field2 (global.get $pc*)) (global.get $tagged-num-0))
                  (local.get $instr))
            (br $continue)))

        (if (i32.eq (local.get $instr) (i32.const 1)) ;; set
          (then
            ;; @@(feature debug-trace (use debug)
            (call $logInstr (i32.const 115)) ;; --- s: 
            ;; )@@
            (if (call $num? (call $field1 (global.get $pc*)))
              (then 
                (call $field0-set! 
                  (call $list-tail (global.get $stack*) (call $untag-num (call $field1 (global.get $pc*))))
                  (call $field0 (global.get $stack*))))
              (else 
                (call $field0-set! 
                  (call $field1 (global.get $pc*))
                  (call $field0 (global.get $stack*)))))
            (global.set $stack* (call $field1 (global.get $stack*)))
            (call $advance-pc)
            (br $continue)))
        
        (if (i32.eq (local.get $instr) (i32.const 2)) ;; get
          (then
            ;; @@(feature debug-trace (use debug)
            (call $logInstr (i32.const 103)) ;; --- g: 
            ;; )@@
            (call $push->stack 
                  (call $get-opnd (call $field1 (global.get $pc*)))
                  (global.get $#pair))
            (call $advance-pc)
            (br $continue)))
        
        (if (i32.eq (local.get $instr) (i32.const 3)) ;; const
          (then
            ;; @@(feature debug-trace (use debug)
            (call $logInstr (i32.const 67)) ;; --- C: 
            ;; )@@
            (call $push->stack 
                  (call $field1 (global.get $pc*))
                  (global.get $#pair))
            (call $advance-pc)
            (br $continue)))
        
        (if (i32.eq (local.get $instr) (i32.const 4)) ;; if
          (then
            ;; @@(feature debug-trace (use debug)
            (call $logInstr (i32.const 105)) ;; --- i: 
            ;; )@@
            (global.set $pc* 
                        (if (result i32) (i32.ne (call $pop<-stack) (global.get $FALSE*))
                             (then (call $field1 (global.get $pc*)))
                             (else (call $field2 (global.get $pc*)))))
            (br $continue)))

        (call $logChar (i32.const 67))
        (unreachable))))

  (func $setup-stack 
    (local $first* i32)

    (call $push->stack 
          (global.get $tagged-num-0)
          (global.get $#pair))
    (call $push->stack 
          (global.get $tagged-num-0)
          (global.get $#pair))

    (local.set $first* (call $field1 (global.get $stack*)))
    (call $field1-set! (global.get $stack*) (global.get $tagged-num-0))
    (call $field2-set! (global.get $stack*) (local.get $first*))
    
    (call $field0-set! (local.get $first*) (call $tag-num (i32.const 5))) ;; HALT instruction
    (call $field1-set! (local.get $first*) (global.get $tagged-num-0))
    (call $field2-set! (local.get $first*) (global.get $#pair)))

  (func $_start
    ;; @@(feature debug
    (global.set $shareholder-next-free (call $str-len (global.get $input_cursor*)))
    ;; )@@

    (call $init-globals)
    (call $build-sym-table)
    (call $rib-graph-decode)

    (call $set-global! (call $alloc-rib 
                             (global.get $tagged-num-0)
                             (global.get $symbol-table*)
                             (global.get $#closure)))

    (call $set-global! (global.get $FALSE*))
    (call $set-global! (global.get $TRUE*))
    (call $set-global! (global.get $NIL*))

    (call $setup-stack)
    (call $run))

  ;; @@(feature debug (use scm2str)
  (func $print (param $str_ptr i32) (param $str_len i32)
    (i32.store (i32.const 0) (local.get $str_ptr))
    (i32.store (i32.const 4) (local.get $str_len))

    (call $logInteger (local.get $str_ptr)))

  (func $println (param $str_ptr i32) (param $str_len i32)
    (call $print (local.get $str_ptr) (local.get $str_len))
    (call $newline))

  (func $newline
    (call $logChar (i32.const 10)))

  (func $print-rib (param $rib* i32) (param $depth i32)
    (if (i32.lt_s (local.get $depth) (i32.const 0))
      (then
        (if (call $rib*? (local.get $rib*))
          (then
            (call $logChar (i32.const 40)) ;; (
            (call $logChar (i32.const 46)) ;; .
            (call $logChar (i32.const 46)) ;; .
            (call $logChar (i32.const 46)) ;; .
            (call $logChar (i32.const 41))) ;; )
          (else
            (call $logChar (i32.const 46))  ;; .
            (call $logChar (i32.const 46))  ;; .
            (call $logChar (i32.const 46))) ;; .
            ))
      (else
        (if (call $rib*? (local.get $rib*))
          (then 
            (if (i32.eq (call $field2 (local.get $rib*)) (global.get $#singleton))
              (then
                (if (i32.eq (local.get $rib*) (global.get $FALSE*))
                  (then 
                    (call $logChar (i32.const 35))   ;; #
                    (call $logChar (i32.const 102)))) ;; f
                (if (i32.eq (local.get $rib*) (global.get $TRUE*))
                  (then 
                    (call $logChar (i32.const 35))   ;; #
                    (call $logChar (i32.const 116)))) ;; t
                (if (i32.eq (local.get $rib*) (global.get $NIL*))
                  (then 
                    (call $logChar (i32.const 39)) ;; '
                    (call $logChar (i32.const 40)) ;; (
                    (call $logChar (i32.const 41)) ;; )
                    )))
              (else
                (call $logChar (i32.const 40)) ;; (
                (call $print-rib 
                      (call $field0 (local.get $rib*)) 
                      (i32.sub (local.get $depth) (i32.const 1)))
                (call $logChar (i32.const 44)) ;; ,
                (call $logChar (i32.const 32)) ;; #\space
                (call $print-rib 
                      (call $field1 (local.get $rib*)) 
                      (i32.sub (local.get $depth) (i32.const 1)))
                (call $logChar (i32.const 44)) ;; ,
                (call $logChar (i32.const 32)) ;; #\space
                (call $print-rib 
                      (call $field2 (local.get $rib*)) 
                      (i32.sub (local.get $depth) (i32.const 1)))
                (call $logChar (i32.const 41)) ;; )
                )))
          (else
            (call $logInteger (call $untag-num (local.get $rib*))))))))

  (func $show-operand (param $obj i32)
    (if (call $num? (local.get $obj))
      (then 
        (call $logChar (i32.const 105)) ;; "i"
        (call $logChar (i32.const 32)) ;; " "
        (call $logInteger (call $untag-num (local.get $obj))))
      (else 
        (if (i32.eq (call $field2 (local.get $obj)) (global.get $#symbol))
          (then
            (call $logChar (i32.const 115)) ;; "s"
            (call $logChar (i32.const 32)) ;; " "
            (call $print-str (call $symrib2str* (local.get $obj))))
          (else 
            (call $logChar (i32.const 40))
            (call $logChar (i32.const 46))
            (call $logChar (i32.const 46))
            (call $logChar (i32.const 46))
            (call $logChar (i32.const 41)))))))

  (func $show-symbol-table
    (local $next* i32)
    (local.set $next* (global.get $symbol-table*))
    (block $break
      (loop $continue
        (br_if $break (i32.eq (local.get $next*) (global.get $tagged-num-0)))
        (call $print-str (call $symrib2str* (call $field0 (local.get $next*))))
        (local.set $next* (call $field1 (local.get $next*)))
        (call $print-rib (local.get $next*) (i32.const 20))
        (call $newline)
        (br $continue))))

  (func $show-stack 
    (local $itr* i32)
    (local.set $itr* (global.get $stack*))
    (if (i32.ne (call $field2 (local.get $itr*)) (global.get $tagged-num-0))
      (then 
        (call $logChar (i32.const 91))
        (call $logChar (i32.const 93)))
      (else 
        (call $logChar (i32.const 91))
        (call $logChar (i32.const 32))

        (block $break
          (br_if $break (i32.ne (call $field2 (local.get $itr*)) (global.get $tagged-num-0)))
          (call $print-rib (call $field0 (local.get $itr*)) (i32.const 1))
          (local.set $itr* (call $field1 (local.get $itr*)))
          (loop $continue 
            (br_if $break (i32.ne (call $field2 (local.get $itr*)) (global.get $tagged-num-0)))
            (call $logChar (i32.const 44))
            (call $logChar (i32.const 32))
            (call $print-rib (call $field0 (local.get $itr*)) (i32.const 1))
            (local.set $itr* (call $field1 (local.get $itr*)))
            (br $continue)))

        (call $logChar (i32.const 32))
        (call $logChar (i32.const 93)))))
        
  (func $print-str (param $str* i32)
    (block $break 
      (loop $continue 
        (br_if $break (i32.eq (i32.load8_u (local.get $str*)) (i32.const 0)))
        (call $logChar (i32.load8_u (memory $shareholder) (local.get $str*)))
        (local.set $str* (i32.add (local.get $str*) (i32.const 1)))
        (br $continue))))

  (func $alert (param i32)
    (call $logChar (i32.const 62)) ;; >
    (call $logChar (i32.const 32))
    (call $logInteger (local.get 0))
    (call $newline))

  (func $logOp (param i32)
    (call $logChar (i32.const 45))  ;; -
    (call $logChar (i32.const 45))  ;; -
    (call $logChar (i32.const 45))  ;; -
    (call $logChar (i32.const 32))  ;; space
    (call $logChar (local.get 0))   ;; arg
    (call $logChar (i32.const 58))  ;; :
    (call $logChar (i32.const 32))) ;; space

  (func $logInstr (param i32)
    (call $logOp (local.get 0))
    (call $show-operand (call $field1 (global.get $pc*)))
    (call $newline)
    (call $show-stack)
    (call $newline))

  (func $info (param i32)
    (call $logInteger (local.get 0))
    (call $newline))

  (func $str-len (param $str* i32) (result i32)
    (local $offset i32)
    (local.set $offset (local.get $str*))
    (block $break
      (loop $continue
        (br_if $break (i32.eqz (i32.load8_u (memory $shareholder) (local.get $str*))))
        (local.set $str* (i32.add (local.get $str*) (i32.const 1)))
        (br $continue)))
    (i32.sub (local.get $str*) (local.get $offset)))


  (func $symrib2str* (param $rib*#symbol i32) (result i32)
    (call $rib2str* (call $field1 (local.get $rib*#symbol))))
  ;; )@@

  (export "_start" (func $_start))
)
