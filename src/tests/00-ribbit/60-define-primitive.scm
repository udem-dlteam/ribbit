(cond-expand

  ((host py)
   (define-primitive (square a)
     "lambda: push(pop() ** 2),"))

  ((host js)
   (define-primitive (square a)
     "() => push(pop() ** 2),"))

  ((host c)
   (define-primitive (square a)
     "{PRIM1();push2(TAG_NUM((NUM(x) * NUM(x))), PAIR_TAG);break;}"))

  ((host scm)
   (define-primitive (square a)
     "(prim1 (lambda (x) (* x x)))"))

  ((host hs)
   (define-primitive (square a)
     " , prim1 (pure . (\\case RibInt x -> RibInt (x * x); _ -> ribFalse))\n"))

  ((host lua)
   (define-primitive (square a)
     "prim1(function(x) return x * x end),"))

  ((host ml)
   (define-primitive (square a)
     "prim1 (fun x -> match x with | Integer x -> Integer (x * x) | _ -> invalid_arg \"square argument must be Integer\");"))


  ((host sh)
   (define-primitive (square a)
     "eval _C=\\$_X$_S
      _C=$((_C*_C))
      _PUSH
      ;;"))

   ((host asm)
    (define-feature square
      (prims "
prim_square: 
%if FIX_TAG != 0
\tdec LAST_ARG
%endif
\tshr LAST_ARG, 1
\timul LAST_ARG, LAST_ARG
\tshl LAST_ARG, 1
%if FIX_TAG != 0
\tinc LAST_ARG
%endif
\tNBARGS(1)
\tret"))

    (define-primitive (square a)
      "\tdd   prim_square\n"))
   ((host pro)
      (define-primitive (square a) "_) :-
  narg_check(1, Stack, Stack0, square),
  pop(Stack0, X, Stack1),
  N is X * X,
  push(Stack1, N, NewStack).
"))

   ((host clj)
      (define-primitive (square a)
        "(prim1 (fn [x] (* x x)))\n"))

  ((host zig)
   (define-primitive (square a)
     "const val: RibField = self.stackPop();
try self.stackPush(num(val.num * val.num));
},\n"))

   (else
     (define-primitive (square a)
      "ERROR"))) ;; Will make test fail !

(##putchar (square 6))
(##putchar 10)

;;;run: -l max -f+ arity-check
;;;run: -l empty -f+ arity-check
;;;fancy-compiler
;;;expected:
;;;$
