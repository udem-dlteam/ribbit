; @@(replace ");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{" (encode 92)
(def rvm-code ");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{") ;; RVM code that prints HELLO!
; )@@

(def debug-mode false)
; Use this macro to add debug code
; e.g. (debug (define debug-variable (vector 1 2 3)))
(defmacro debug [& body]
  `(if debug-mode (do ~@body)))

(def pair-type      0)
(def procedure-type 1)
(def symbol-type    2)
(def string-type    3)
(def vector-type    4)
(def singleton-type 5)

(defn _rib [x y z] (apply vector (map ref [x y z])))
(defn _rib? [x] (vector? x))
(defn _field0 [x] (deref (get x 0)))
(defn _field1 [x] (deref (get x 1)))
(defn _field2 [x] (deref (get x 2)))
(defn _field0-set! [x y]  (dosync (ref-set (get x 0) y)))
(defn _field1-set! [x y]  (dosync (ref-set (get x 1) y)))
(defn _field2-set! [x y]  (dosync (ref-set (get x 2) y)))

(defn is-instance? [t]
  #(and (_rib? %) (= (_field2 %) t)))

(def _pair? (is-instance? pair-type))

(defn _cons [car cdr] (_rib car cdr pair-type))
(defn _car [pair] (_field0 pair))
(defn _cdr [pair] (_field1 pair))
(defn _set-car! [pair x] (_field0-set! pair x))

(defn _length [lst]
  (if (_pair? lst)
    (+ 1 (_length (_cdr lst)))
    0))

; This was named _list-tail in the Scheme implementation
(defn _list-drop [lst i]
  (if (< 0 i)
    (_list-drop (_cdr lst) (- i 1))
    lst))

(defn _list-head [lst i rest]
  (if (< 0 i)
    (_cons (_car lst) (_list-head (_cdr lst) (- i 1) rest))
    rest))

; f is a function that takes two args
; 1. accumulator
; 2. element
; and returns an updated accumulator
(defn _fold-left [f acc lst]
  (if (_pair? lst)
    (_fold-left f
                (f acc (_car lst))
                (_cdr lst))
    acc))

(def _false (_rib 0 0 singleton-type))
(def _true  (_rib 0 0 singleton-type))
(def _nil   (_rib 0 0 singleton-type))

; Converts a list of chars to a string rib
(defn _list->string [lst] (_rib lst (_length lst) string-type))
; Converts a string rib to a symbol rib
(defn _string->uninterned-symbol [s] (_rib _false s symbol-type))

(defn _reverse-lst [lst]
  (_fold-left
    (fn [acc elem] (_cons elem acc))
    _nil
    lst))

(def pos (ref 0))

;; Call this function to get the next-instr byte from the input
(def get-byte
 (fn []
   (let [idx (deref pos)]
     (dosync (ref-set pos (+ 1 idx)))
     (int (get rvm-code idx)))))

;; Debug print for ribs
(debug
 (def tracing (ref false))
 (def step-count (ref 0))
 (def start-tracing (ref 0))
 (def next-stamp (ref 0))

  ; Prints a debug representation of a rib
  ; Works on both objects and instructions.
 (defn show [obj]
   (if-not (_rib? obj)
     (print obj) ; Print the int directly
     (let [obj-type (_field2 obj)] ; Handle the object based on its type
       (if (= obj-type vector-type)
         (do (print "#") (show (_field0 obj))) ; Extra work for vectors
         (case obj-type
            ; Pair
           0 (do
               (print "(")
               (show (_field0 obj))
               (let [obj
                     (loop [n 1
                            obj (_field1 obj)]
                       (if (and (_rib? obj) (= (_field2 obj) 0))
                         (if (> n 4) ; If list is too long, stop printing
                           (do
                             (print " ...")
                             _nil)
                           (do ; Otherwise recursively handle cdr
                             (print " ")
                             (show (_field0 obj))
                             (recur (+ n 1) (_field1 obj))))
                         obj))]
                 (when-not (= obj _nil)
                   (print " . ")
                   (show obj))
                 (print ")")))
            ; Proc
           1 (if (_rib? (_field0 obj))
               (do
                 (print "#<procedure nparams=")
                 (print (_field0 (_field0 obj)))
                 (print ">"))
               (do ; We have an int that points to a primitive
                 (print "#<primitive ")
                 (print (_field0 obj))
                 (print ">")))
            ; Symbol type
           2 (let [obj (_field1 obj)] ; Get name of symbol
               (if (and (_rib? obj)
                        (= (_field2 obj) 3)  ; Check if we have a string
                        (> (_field1 obj) 0)) ; that is non-empty
                (loop [obj (_field0 obj)] ; Iterate through list of chars
                  (when (and (_rib? obj) (= (_field2 obj) 0)) ; Ensure we have a pair
                    (print (char (_field0 obj)))
                    (recur (_field1 obj))))
                (do
                  (print "#<symbol ")
                  (show obj)
                  (print ">"))))
            ; String type
           3 (do
               (print "\"")
               (loop [obj (_field0 obj)] ; Loop through chars of a string
                 (if (and (_rib? obj) (= (_field2 obj) 0)) ; Ensure that we have a pair
                   (let [c (_field0 obj)]
                     (case c
                       10 (print "\\n")
                       13 (print "\\r")
                       9 (print "\\t")
                       92 (print "\\\\")
                       34 (print "\\\"")
                       (print (char c)))
                     (recur (_field1 obj)))
                   (print "\""))))
            ; Singleton
           5 (cond
               (= obj _false) (print "#f")
               (= obj _true) (print "#t")
               (= obj _nil) (print "()")
               :else (do
                       (print "[")
                       (show (_field0 obj))
                       (print ",")
                       (show (_field1 obj))
                       (print ",")
                       (show (_field2 obj))
                       (print "]")))
            ; Vector
           4 (do
               (print "[")
               (show (_field0 obj))
               (print ",")
               (show (_field1 obj))
               (print ",")
               (show (_field2 obj))
               (print "]"))

            ; Default - Instruction
           (do
             (print "[I: ")
             (show (_field0 obj))
             (print ",")
             (show (_field1 obj))
             (print ",")
             (show (_field2 obj))
             (print "]")))))))

 (defn trace-instruction [instr-name opnd]
   (print instr-name)
   (when opnd
     (print " ")
     (show opnd))
   (newline))

 (defn start-step [stack]
   (dosync (ref-set step-count (+ 1 (deref step-count))))
   (when (>= (deref step-count) (deref start-tracing))
     (dosync (ref-set tracing true)))
   (if-not (deref tracing)
     (when (>= (deref step-count) (deref next-stamp))
       (dosync (ref-set next-stamp (Math/floor (+ (* (deref next-stamp) 1.01) 1))))
       (print "@")
       (print (deref step-count))
       (newline))
     (do
       (print "@")
       (print (deref step-count))
       (print " STACK = (")
       (loop [s stack
              sep ""]
         (if (and (_rib? s) (= (_field2 s) pair-type))
           (do
             (print sep)
             (show (_field0 s))
             (recur (_field1 s) " "))
           (do
             (print ")")
             (newline))))))))

;; Decode start

; Half of encoding base (92)
(def half-eb (/ 92 2))

(defn get-code []
  (let [x (- (get-byte) 35)]
    (if (< x 0) 57 x)))

; Returns something between 0-91 (inclusive)
; n is the accumulation of the int by calculated by using the
; higher powers.
;
; Interpret the code as a base-46 integer, by doing the following:
; 1. If the code is between 0-45, use this as the coefficient for
;    the 0th power, OR
; 2. If the code is between 46-91, calculate code % 46, use this as
;    the coefficient for the current power of n and make a recursive
;    call until we get a code between 0-45.
; Check Section 2.11 of the Ribbit paper for more details.
(defn get-int
  [n] (let [x (get-code)
            y (* n half-eb)]
        (if (< x half-eb)
          (+ y x)
          (get-int (+ y (- x half-eb))))))

; Consumes start of the input to build an
; initial symbol table (behaves as a linked list)
(defn build-symtbl []
  (letfn
    ; Adds list of chars to symtbl by treating symtbl
   [(add-symbol [cs symtbl]
      (_cons (_string->uninterned-symbol (_list->string cs))
             symtbl))
    (loop1 [n symtbl]
      (if (< 0 n)
        (loop1 (- n 1) (add-symbol _nil symtbl)) ; Add n empty lists
        (letfn [(loop2 [symtbl]
                  (letfn [(loop3 [cs]
                            (let [x (get-byte)]
                              (if (= x 44) ; #\, separates symbols
                                (loop2 (add-symbol cs symtbl))
                                (if (= x 59) ; #\; terminates symbol list
                                  (add-symbol cs symtbl)
                                  (loop3 (_cons x cs))))))]
                    (loop3 _nil)))]
          (loop2 symtbl))))]
   (loop1 (get-int 0) _nil)))

(defn decode []
  (let [symtbl (build-symtbl)]
      ; Get nth symbol from the symbol table
    (defn sym [n]
      (_car (_list-drop symtbl n)))

    ; Creates a new instruction with the given op and opnd
    ; and places it on the top of the stack, then continue
    ; decoding with new stack. This instruction has the
    ; previous head of the stack as the next instruction.

    (defn decode-loop [stack]
      (loop [stack stack]
        (defn get-n [op n]
          ; Short encodings
          ; Check Figure 7 of Ribbit paper for values
          (let [d (get (vector 20 30 0 10 11 4) op)]
            (if (< (+ 2 d) n)
              (get-n (+ op 1) (- n (+ d 3))) ; On top of the short encodings, we have 3
                                             ; extra codes to provide extra info
              (vector op d n))))

       ; Short encodings
        (let [x (get-code)
              [op d n] (get-n 0 x)]
          (if (< 90 x) ; x == 91 -> If instruction
            (let [op 4 ; 4 is the code for If instructions
                  opnd (_car stack)
                  stack (_cdr stack)]
              (_set-car! stack (_rib op opnd (_car stack)))
              (recur stack))
            (let [stack (if (= op 0) (_cons 0 stack) stack) ; Add 0 to stack for jump (tailcall)
                  opnd (if (< n d)
                          ; TODO: Why operand should be a symbol for jmp, call, set??
                          ; Shouldn't it be for jmp and call only according to Section 2.11??
                         (if (< op 3) (sym n) n)
                         (if (= n d) ; The d'th value indicates if the index is
                                     ; a variable length base-46 integer.
                           (get-int 0)
                           (sym (get-int (- (- n d) 1)))))]
              (if (< 4 op) ; Check if we have a closure
                (let [proc (_rib
                            (_rib
                             opnd ; Num params
                             0 ; Just 0, check Figure 5 of Ribbit paper
                             (_car stack)) ; Linked list of instructions of the proc
                            _nil ; Empty env
                            procedure-type)
                      stack (_cdr stack)]
                   ; If the stack contains more stuff, add a const containing the
                   ; proc to the stack and keep decoding, otherwise return the proc
                  (if (_rib? stack)
                    (do
                      ;; const containing the proc
                      (_set-car! stack (_rib 3 proc (_car stack)))
                      (recur stack))
                    proc))
                 ; Give the ribs the right op code, jump and
                 ; call have the same code. Check Figure 3
                 ; of Ribbit paper for the respective codes.
                (do
                  (_set-car! stack (_rib (if (< 0 op) (- op 1) 0) opnd (_car stack)))
                  (recur stack))))))))

    (let [main-proc (decode-loop 0)]
      ;; Set predefined globals (always 4 first in the symbol table)
      (defn set-global [symtbl v]
        (_field0-set! (_car symtbl) v)
        (_cdr symtbl))
      (reduce set-global
              symtbl
              (vector (_rib 0 symtbl procedure-type) ; Create proc with 0th primitive - rib
                      _false
                      _true
                      _nil))
      (debug
       (print "Symtbl: ")
       (show symtbl)
       (newline))
      main-proc)))

;; Decode end

(defn get-cont [stack]
  (loop [stack stack]
    (if (_rib? (_field2 stack))
      stack
      (recur (_cdr stack)))))

(defn get-var [stack opnd]
  (_field0 (if (_rib? opnd)
             opnd
             (_list-drop stack opnd))))

(defn set-var [stack opnd v]
  (_field0-set!
    (if (_rib? opnd)
        opnd
        (_list-drop stack opnd))
    v))

(defn fatal [msg]
  (println msg)
  (System/exit 1))

(defn prim0 [f]
  (fn [stack _pc] (_cons (f) stack)))

(defn prim1 [f]
  (fn [stack _pc]
     (let [x (_car stack)
           stack (_cdr stack)]
       (_cons (f x) stack))))

(defn prim2 [f]
  (fn [stack _pc]
     (let [y (_car stack)
           stack (_cdr stack)
           x (_car stack)
           stack (_cdr stack)]
       (_cons (f x y) stack))))

(defn prim3 [f]
  (fn [stack _pc]
     (let [z (_car stack)
           stack (_cdr stack)
           y (_car stack)
           stack (_cdr stack)
           x (_car stack)
           stack (_cdr stack)]
       (_cons (f x y z) stack))))

(defn bool [x]
  (if x _true _false))

(defn readchar []
    (.read *in*))

(def primitives
  (vector 
    ;; @@(primitives (gen body)
    (prim3 _rib)         ;; @@(primitive (%%rib a b c))@@
    (prim1 (fn [x] x))   ;; @@(primitive (%%id x))@@
    (fn [stack _pc] (_cdr stack))  ;; @@(primitive (%%arg1 x y))@@
    (prim2 (fn [_ x] x)) ;; @@(primitive (%%arg2 x y))@@

    ;; @@(primitive (%%close rib)
    (fn [stack _pc]
      (let [x (_car stack)
            stack (_cdr stack)]
        (_cons (_rib (_field0 x) stack procedure-type) stack)))
    ;; )@@

    (prim1 (fn [x] (bool (_rib? x)))) ;; @@(primitive (%%rib? rib))@@
    (prim1 _field0) ;; @@(primitive (%%field0 rib))@@
    (prim1 _field1) ;; @@(primitive (%%field1 rib))@@
    (prim1 _field2) ;; @@(primitive (%%field2 rib))@@
    (prim2 (fn [x y] (_field0-set! x y) y)) ;; @@(primitive (%%field0-set! rib x))@@
    (prim2 (fn [x y] (_field1-set! x y) y)) ;; @@(primitive (%%field1-set! rib x))@@
    (prim2 (fn [x y] (_field2-set! x y) y)) ;; @@(primitive (%%field2-set! rib x))@@
    (prim2 (fn [x y] (bool (= x y)))) ;; @@(primitive (%%eqv? x y))@@
    (prim2 (fn [x y] (bool (< x y)))) ;; @@(primitive (%%< a b))@@
    (prim2 +) ;; @@(primitive (%%+ a b))@@
    (prim2 -) ;; @@(primitive (%%- a b))@@
    (prim2 *) ;; @@(primitive (%%* a b))@@
    (prim2 quot) ;; @@(primitive (%%quotient a b))@@

    ;; @@(primitive (%%getchar)
    (prim0 (fn [] ;; 18
             (if (< (deref pos) (count rvm-code))
               (get-byte)
               (let [c (readchar)]
                 c))))
    ;; )@@

    ;; @@(primitive (%%putchar x)
    (prim1 (fn [x] ;; 19
             (print (char x))
             (flush) ; Ensure that print goes out immediately
             x))
    ;; )@@ 

    ;; @@(primitive (%%exit x)
    (prim1 (fn [x] ;; 20
             (flush) ; Ensure that error messages go out
             (System/exit x)))
    ;; )@@
    ;; )@@
    ))

(defn run [pc stack]
  (defn gather-args [nargs new-stack stack]
    (if (< 0 nargs)
      (gather-args
       (- nargs 1)
       (_cons (_car stack) new-stack)
       (_cdr stack))
      [new-stack stack]))
  (loop [pc pc
         stack stack]
    (debug (start-step stack))
    (let [instr (_field0 pc)
          opnd (_field1 pc)
          next-instr (_field2 pc)]
       ;; jump/call
      (case instr
        0 (do
            (debug
              (when tracing
                (trace-instruction (if (= 0 next-instr) "jump" "call") opnd)))
            (let [proc (get-var stack opnd)
                  code (_field0 proc)
                  ; @@(feature arity-check
                  nargs (_car stack)
                  stack (_cdr stack)
                  ; )@@
                 ]
                 (if (_rib? code)
                    ;; calling a lambda
                   (let [new-cont (_rib 0 proc 0)
                         ; Num params expected by function
                         nparams (bit-shift-right (_field0 code) 1)
                         ; Does the function allow variadic args?
                         is-variadic (= (bit-and (_field0 code) 1) 1) ; Check if last bit = 1
                         rest-args 0
                         ;; @@(feature rest-param (use arity-check)
                         rest-args (- nargs nparams)
                         ;; )@@
                         ] 
                          
                       ;; @@(feature arity-check
                       (cond
                         (and (not is-variadic) (not (= nargs nparams)))
                           (fatal (format "lambda called with %d args, expected %d"
                                          nargs nparams))
                         (and is-variadic (< nargs nparams))
                           (fatal (format "lambda called with %d args, expected at least %d"
                                          nargs nparams)))
                       ;; )@@

                       (let [
                             [new-stack stack] [new-cont stack]
                             ;; @@(feature rest-param (use arity-check)
                             [new-stack stack] (if is-variadic
                                                  ; Pop first N args from old stack and add them
                                                  ; to the new stack
                                                  [(_cons
                                                     (_reverse-lst (_list-head stack
                                                                               rest-args _nil))
                                                     new-cont)
                                                   (_list-drop stack rest-args)]
                                                  [new-stack stack])
                             ;; )@@
                             [new-stack stack] (gather-args
                                                 nparams new-stack stack)]
                         (if (_rib? next-instr) ;; non-tail call?
                           (do
                             (_field0-set! new-cont stack)
                             (_field2-set! new-cont next-instr))
                           (let [k (get-cont stack)]
                             (_field0-set! new-cont (_field0 k))
                             (_field2-set! new-cont (_field2 k))))
                         (recur (_field2 code) new-stack)))

                   ;; calling a primitive
                   (let [stack ((get primitives code) stack pc)]
                     (recur (if (_rib? next-instr) ;; next-instr is non-tail call?
                                next-instr
                                (let [cont (get-cont stack)]
                                  (_field1-set! stack (_field0 cont))
                                  (_field2 cont)))
                          stack)))))
        ;; set
        1 (do
            (debug (when tracing (trace-instruction "set" opnd)))
            (set-var stack opnd (_car stack))
            (recur next-instr (_cdr stack)))

        ;; get
        2 (do
            (debug (when tracing (trace-instruction "get" opnd)))
            (recur next-instr (_cons (get-var stack opnd) stack)))

        ;; const
        3 (do
            (debug (when tracing (trace-instruction "const" opnd)))
            (recur next-instr (_cons opnd stack)))

        ;; if
        4 (do
            (debug (when tracing (trace-instruction "if" opnd)))
            (recur (if (= (_car stack) _false)
                    next-instr
                    opnd)
                 (_cdr stack)))

        ;; halt
        (do
            (debug (when tracing (trace-instruction "halt" opnd)))
           nil)))))

(let [x (decode)]
  (run (_field2 (_field0 x)) ;; instruction stream of main procedure
       (_rib 0 0 (_rib 5 0 0)))) ;; primordial continuation = halt
