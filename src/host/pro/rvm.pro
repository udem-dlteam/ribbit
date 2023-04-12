reserved_atoms([nil, code, init, main], []).


% @@(replace ");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{" (encode 92)
rvm_code(");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{"). % RVM code that prints HELLO!
% )@@

% type(+TypeLit, -TypeVal).
% type(pair, 0).
% type(procedure, 1).
% type(symbol, 2).
% type(string, 3).
% type(vector, 4).
% type(singleton, 5).


% special(+Short, -Value)
special(nil, rib(nil, 0, 0, 5)).
special(true, rib(true, 0, 0, 5)).
special(false, rib(false, 0, 0, 5)).

rib(_, _, _, _).
rib_(A, B, C, rib(0, A, B, C)).

field_0_set(Rib, Val) :-
  setarg(2, Rib, Val).

field_1_set(Rib, Val) :-
  setarg(3, Rib, Val).

field_2_set(Rib, Val) :-
  setarg(4, Rib, Val).

set(rib(_, _, _, _), Val, Val).

field_0(Rib, A) :- arg(2, Rib, A).
field_1(Rib, B) :- arg(3, Rib, B).
field_2(Rib, C) :- arg(4, Rib, C).

cons(A, B, Rib) :- 
  rib_(A, B, 0, Rib).

is_rib(rib(_, _, _, _)).

% get_byte(-FirstByte)
get_byte_(Code) :-
  g_read(code, [Code | Tail]),
  g_link(code, Tail).

% get_code_(-Code)
get_code_(AdjustedCode) :-
  g_read(code, [Code | Tail]),
  Decoded is Code - 35,
  (Decoded < 0 ->
    AdjustedCode is 57;
    AdjustedCode = Decoded
  ),
  g_link(code, Tail).

% get_int_(+Acc, -N)
get_int_(Acc, N) :-
  get_code_(Code),
  Y is Acc * 46,
  (Code < 46 ->
    N is Code + Y;
    NewN is Y + Code - 46,
    get_int_(NewN, N)
  ).

list_tail(Rib, 0, Rib) :- !.
list_tail(Rib, N, NRib) :-
  N1 is N - 1,
  field_1(Rib, Cdr),
  list_tail(Cdr, N1, NRib).

%init_symtbl(+N, -SymTable)
init_symtbl(N, SymTable) :-
  special(nil, NIL),
  init_symtbl(N, NIL, SymTable).

% init_symtbl(+N, +Partial, -SymTable)
init_symtbl(N, Partial, SymTable) :-
  (
    N @> 0 ->
      N1 is N - 1,
      special(false, FALSE),
      special(nil, NIL),
      rib_(NIL, 0, 3, Rib1),
      rib_(FALSE, Rib1, 2, Rib2),
      cons(Rib2, Partial, NewPartial),
      init_symtbl(N1, NewPartial, SymTable);
      SymTable = Partial
  ).

% build_symtbl(+Codes, +SymTable, +Acc, +N, -NewSymTable, -NewCodes)
build_symtbl(SymTable, Acc, N, NewSymTable) :-
  get_byte_(Code),
  (
    Code = 59 ->
      special(false, FALSE),
      rib_(Acc, N, 3, Rib1),
      rib_(FALSE, Rib1, 2, Rib2),
      cons(Rib2, SymTable, NewSymTable);
      (
        Code = 44 ->
        special(nil, NIL),
        special(false, FALSE),
        rib_(Acc, N, 3, Rib1),
        rib_(FALSE, Rib1, 2, Rib2),
        cons(Rib2, SymTable, SymTable1),
        build_symtbl(SymTable1, NIL, 0, NewSymTable);
        N1 is N + 1,
        integer(N1),
        cons(Code, Acc, NewAcc),
        build_symtbl(SymTable, NewAcc, N1, NewSymTable)
      )
  ).
  

% add_instr(+Op, +Opnd, +Stack)
add_instr(Op, Opnd, Stack) :-
  field_0(Stack, Car),
  rib_(Op, Opnd, Car, Rib),
  field_0_set(Stack, Rib).

% get_sym(+SymTable, +Index, -Sym)
get_sym(SymTable, N, Sym) :-
  list_tail(SymTable, N, Rib),
  field_0(Rib, Sym).

% decode_op(+Op, -DecodedOp)
% maps an integer to an operator
decode_op(Op, DecodedOp) :- nth0(Op, [20, 30, 0, 10, 11, 4], DecodedOp).

decode_op_loop(N, Op, NewOp, NewN, NewD) :-
  decode_op(Op, D),
  (
    D + 2 < N ->
      Op1 is Op + 1,
      N1 is N - D - 3,
      decode_op_loop(N1, Op1, NewOp, NewN, NewD);
      NewN = N,
      NewOp = Op,
      NewD = D
  ).

decode(SymTable, Instructions) :-
  decode(SymTable, 0, Instructions).

% decode_loop(+Codes, +SymTable, +Stack, -Instructions, -NewCodes)
% main decode loop (translation of the .scm version)
decode(SymTable, Stack, Instructions) :-
  get_code_(X),
  decode_op_loop(X, 0, Op, N, D),
  (
    90 < X ->
      field_0(Stack, Car),
      field_1(Stack, Cdr),
      add_instr(4, Car, Cdr),
      decode(SymTable, Cdr, Instructions);
      (
        Op == 0 ->
          cons(0, Stack, Stack1);
          Stack1 = Stack
      ),
      (
        N < D ->
          (
            Op < 3 ->
              get_sym(SymTable, N, Opnd);
              Opnd = N
          );
          (
            N = D ->
              get_int_(0, Opnd);
              N1 is N - D - 1,
              get_int_(N1, I),
              get_sym(SymTable, I, Opnd)
          )
      ),
      (
        4 < Op ->
          special(nil, NIL),
          field_0(Stack1, Car1),
          rib_(Opnd, 0, Car1, Rib1),
          rib_(Rib1, NIL, 1, Proc),
          field_1(Stack1, Stack2),
          (
            is_rib(Stack2) ->
              add_instr(3, Proc, Stack2),
              decode(SymTable, Stack2, Instructions);
              Instructions = Proc
          );
          (
            0 < Op ->
              Op1 is Op - 1;
              Op1 = 0
          ),
          add_instr(Op1, Opnd, Stack1),
          decode(SymTable, Stack1, Instructions)
      )
  ).

set_global(SymTable, Val, NewSymTable) :-
  field_0(SymTable, Car),
  field_1(SymTable, Cdr),
  field_0_set(Car, Val),
  set(SymTable, Cdr, NewSymTable).

finalize_decode(T0, NewSymTable) :-
  rib_(0, T0, 1, Rib0),
  set_global(T0, Rib0, T1),
  special(false, FALSE),
  set_global(T1, FALSE, T2),
  special(true, TRUE),
  set_global(T2, TRUE, T3),
  special(nil, NIL),
  set_global(T3, NIL, NewSymTable).

% pop(+Stack, -Rib, -NewStack)
pop(Stack, Rib, NewStack) :-
  field_0(Stack, Rib),
  field_1(Stack, NewStack).

% push(+Stack, +Rib, -NewStack)
push(Stack, Rib, NewStack) :-
  cons(Rib, Stack, NewStack).

get_cont(Stack, Cont) :-
  field_2(Stack, Field2),
  (
    is_rib(Field2) ->
      Cont=Stack;
      field_1(Stack, Cdr),
      get_cont(Cdr, Cont)
  ).

get_var(Stack, Opnd, Var) :-
  (
    is_rib(Opnd) ->
      field_0(Opnd, Var);
      % writeln(debug_out, Opnd),
      list_tail(Stack, Opnd, Rib),
      field_0(Rib, Var)
  ).

set_var(Stack, Opnd, Val) :-
  (
    is_rib(Opnd) ->
      field_0_set(Opnd, Val);
      list_tail(Stack, Opnd, Rib),
      field_0_set(Rib, Val)
  ).

% @@(feature arity-check
narg_check(Needed, Stack, Stack1, _) :-
  pop(Stack, Needed, Stack1), !.

narg_check(Needed, Stack, _, Prim) :-
  pop(Stack, Given, _),
  atomic_list_concat([Given, ' expected to have ', Needed, ' arguments in ', Prim], Message),
  domain_error(Message, Given), !.
% )@@

narg_check(_, Stack, Stack, _) :- !.

% @@(primitives (gen "primitive(" index ", Stack, NewStack, _) :- \n" body)
primitive(0, Stack, NewStack, _) :- % @@(primitive (rib a b c)
  narg_check(3, Stack, Stack0, rib),
  pop(Stack0, Z, Stack1),
  pop(Stack1, Y, Stack2),
  pop(Stack2, X, Stack3),
  rib_(X, Y, Z, Rib),
  push(Stack3, Rib, NewStack). % )@@

primitive(1, Stack, NewStack, _) :- % @@(primitive (id x)
  narg_check(1, Stack, Stack0, id),
  pop(Stack0, X, Stack1),
  push(Stack1, X, NewStack). % )@@

primitive(2, Stack, NewStack, _) :- % @@(primitive (arg1 x y)
  narg_check(2, Stack, Stack0, arg1),
  pop(Stack0, _, NewStack). % )@@

primitive(3, Stack, NewStack, _) :- % @@(primitive (arg2 x y)
  narg_check(2, Stack, Stack0, arg2),
  pop(Stack0, Y, Stack1),
  pop(Stack1, _, Stack2),
  push(Stack2, Y, NewStack). % )@@

primitive(4, Stack, NewStack, _) :- % @@(primitive (close rib)
  narg_check(_, Stack, Stack0, close),
  pop(Stack0, X, Stack1),
  field_0(X, Car),
  rib_(Car, Stack1, 1, Rib),
  push(Stack1, Rib, NewStack). % )@@

primitive(5, Stack, NewStack, _) :- % @@(primitive (rib? x)
  narg_check(1, Stack, Stack0, 'rib?'),
  pop(Stack0, X, Stack1),
  special(true, TRUE),
  special(false, FALSE),
  (
    is_rib(X) ->
      push(Stack1, TRUE, NewStack);
      push(Stack1, FALSE, NewStack)
    ). % )@@

primitive(6, Stack, NewStack, _) :- % @@(primitive (field0 x)
  narg_check(1, Stack, Stack0, field0),
  pop(Stack0, X, Stack1),
  field_0(X, Field0),
  push(Stack1, Field0, NewStack). % )@@

primitive(7, Stack, NewStack, _) :- % @@(primitive (field1 x)
  narg_check(1, Stack, Stack0, field1),
  pop(Stack0, X, Stack1),
  field_1(X, Field1),
  push(Stack1, Field1, NewStack). % )@@

primitive(8, Stack, NewStack, _) :- % @@(primitive (field2 x)
  narg_check(1, Stack, Stack0, field2),
  pop(Stack0, X, Stack1),
  field_2(X, Field2),
  push(Stack1, Field2, NewStack). % )@@

primitive(9, Stack, NewStack, _) :- % @@(primitive (field0-set! rib x)
  narg_check(2, Stack, Stack0, 'field0-set!'),
  pop(Stack0, Y, Stack1),
  pop(Stack1, X, Stack2),
  field_0_set(X, Y),
  push(Stack2, Y, NewStack). % )@@

primitive(10, Stack, NewStack, _) :- % @@(primitive (field1-set! rib x)
  narg_check(2, Stack, Stack0, 'field1-set!'),
  pop(Stack0, Y, Stack1),
  pop(Stack1, X, Stack2),
  field_1_set(X, Y),
  push(Stack2, Y, NewStack). % )@@

primitive(11, Stack, NewStack, _) :- % @@(primitive (field2-set! rib x)
  narg_check(2, Stack, Stack0, 'field2-set!'),
  pop(Stack0, Y, Stack1),
  pop(Stack1, X, Stack2),
  field_2_set(X, Y),
  push(Stack2, Y, NewStack). % )@@

primitive(12, Stack, NewStack, _) :- % @@(primitive (eqv? x y)
  narg_check(2, Stack, Stack0, 'eqv?'),
  pop(Stack0, Y, Stack1),
  pop(Stack1, X, Stack2),
  special(true, TRUE),
  special(false, FALSE),
  (
    X = Y ->
      push(Stack2, TRUE, NewStack);
      push(Stack2, FALSE, NewStack)
  ). % )@@

primitive(13, Stack, NewStack, _) :- % @@(primitive (< a b)
  narg_check(2, Stack, Stack0, '<'),
  pop(Stack0, Y, Stack1),
  pop(Stack1, X, Stack2),
  special(true, TRUE),
  special(false, FALSE),
  (
    X < Y ->
      push(Stack2, TRUE, NewStack);
      push(Stack2, FALSE, NewStack)
  ). % )@@

primitive(14, Stack, NewStack, _) :- % @@(primitive (+ a b)
  narg_check(2, Stack, Stack0, '+'),
  pop(Stack0, Y, Stack1),
  pop(Stack1, X, Stack2),
  N is X + Y,
  push(Stack2, N, NewStack). % )@@

primitive(15, Stack, NewStack, _) :- % @@(primitive (- a b)
  narg_check(2, Stack, Stack0, '-'),
  pop(Stack0, Y, Stack1),
  pop(Stack1, X, Stack2),
  N is X - Y,
  push(Stack2, N, NewStack). % )@@

primitive(16, Stack, NewStack, _) :- % @@(primitive (* a b)
  narg_check(2, Stack, Stack0, '*'),
  pop(Stack0, Y, Stack1),
  pop(Stack1, X, Stack2),
  N is X * Y,
  push(Stack2, N, NewStack). % )@@

primitive(17, Stack, NewStack, _) :- % @@(primitive (quotient a b)
  narg_check(2, Stack, Stack0, quotient),
  pop(Stack0, Y, Stack1),
  pop(Stack1, X, Stack2),
  N is X // Y,
  push(Stack2, N, NewStack). % )@@

primitive(18, Stack, NewStack, _) :- %% @@(primitive (getchar)
  narg_check(0, Stack, Stack0, getchar),
  get_code(Char),
  push(Stack0, Char, NewStack). % )@@

primitive(19, Stack, NewStack, _) :- % @@(primitive (putchar c)
  narg_check(1, Stack, Stack0, putchar),
  pop(Stack0, X, Stack1),
  put_code(X),
  push(Stack1, X, NewStack). % )@@

primitive(20, Stack, NewStack, _) :- % @@(primitive (exit a)
  narg_check(1, Stack, Stack0, exit),
  pop(Stack0, X, NewStack),
  halt(X). % )@@


primitive(21, Stack, NewStack, _) :- % @@(primitive (list . l) (use variadic)
  pop(Stack, Nargs, Stack1),
  pop_n(Nargs, Stack1, Args, Stack2),
  reverse(Args, Args1),
  list_(Args1, List),
  push(Stack2, List, NewStack). % )@@

primitive(22, Stack, NewStack) :- % @@(primitive (square a)
  narg_check(1, Stack, Stack0, square),
  pop(Stack0, X, Stack1),
  N is X * X,
  push(Stack1, N, NewStack). % )@@
% )@@

  



list_([], NIL) :- special(nil, NIL).
list_([X | Tail], List) :-
  list_(Tail, TailList),
  cons(X, TailList, List).

pop_n(0, Stack, [], Stack) :- !.
pop_n(Nargs, Stack, [X | Args], Stack2) :-
  N1 is Nargs - 1,
  pop(Stack, X, Stack1),
  pop_n(N1, Stack1, Args, Stack2).

run([], []) :- halt(0), !.
run(Pc, Stack) :-
  field_0(Pc, Instr),
  field_1(Pc, Opnd),
  field_2(Pc, Next),
  case_instr(Instr, Opnd, Next, Pc, Stack, Pc1, Stack1),
  run(Pc1, Stack1).

case_instr(0, Opnd, Next, Pc, Stack, Pc1, Stack1) :-
  get_var(Stack, Opnd, Proc),
  field_0(Proc, Code),
  (
    is_rib(Code) ->
      %% calling lambda
      cons(0, Proc, NewCont),
      field_0(Code, NargsEncoded),
      Nargs is NargsEncoded // 2,
      (
        (NargsEncoded mod 2) =:= 1 ->
          pop(Stack, NPassed, Stack_1),
          Nargs =< NPassed,
          Nargs2 is Nargs + 1,
          N is NPassed - Nargs,
          pop_n(N, Stack_1, Rest, Stack_2),
          reverse(Rest, Rest0),
          list_(Rest0, RestRib),
          push(Stack_2, RestRib, Stack_3)
          ; Nargs2 = Nargs,
          narg_check(Nargs2, Stack, Stack_3, lambda_call)
      ),
      lambda_call_arg_loop(Nargs2, NewCont, Stack_3, NewStack0, Stack0),
      lambda_call_loop(NewCont, Next, Code, NewStack0, Stack0, Pc1, Stack1)
      %% calling primitive
      ;
      primitive(Code, Stack, Stack1, Pc),
      field_2(Pc, Next1),
      (
        is_rib(Next1) ->
          % non-tail call
          Pc1 = Next1;
          get_cont(Stack1, Cont),
          field_0(Cont, ContCar),
          field_1_set(Stack1, ContCar),
          field_2(Cont, Pc1)
      )
  ), !.

%% set
case_instr(1, Opnd, Next, _, Stack, Next, Cdr) :-
  field_0(Stack, Car),
  set_var(Stack, Opnd, Car),
  field_1(Stack, Cdr), !.

%% get
case_instr(2, Opnd, Next, _, Stack, Next, Stack1) :-
  get_var(Stack, Opnd, Var),
  cons(Var, Stack, Stack1), !.

%% const
case_instr(3, Opnd, Next, _, Stack, Next, Stack1) :-
  cons(Opnd, Stack, Stack1), !.

%% if
case_instr(4, Opnd, Next, _, Stack, Pc1, Cdr) :-
  field_0(Stack, Car),
  field_1(Stack, Cdr),
  (
    special(false, Car) ->
      Pc1 = Next;
      Pc1 = Opnd
  ), !.

%% halt
case_instr(_, _, _, _, _, [], []) :- halt(0), !.

lambda_call_arg_loop(0, NewStack, Stack, NewStack, Stack) :- !.
lambda_call_arg_loop(Nargs, NewStack, Stack, NewStackF, StackF) :-
  N1 is Nargs - 1,
  pop(Stack, Car, Stack1),
  push(NewStack, Car, NewStack1),
  lambda_call_arg_loop(N1, NewStack1, Stack1, NewStackF, StackF).

lambda_call_loop(NewCont, Next, Code, NewStack, Stack, Pc, NewStack) :-
  (
    is_rib(Next) ->
      % non-tail call
      field_0_set(NewCont, Stack),
      field_2_set(NewCont, Next);
      get_cont(Stack, K),
      field_0(K, Car),
      field_0_set(NewCont, Car),
      field_2(K, Field2),
      field_2_set(NewCont, Field2)
  ),
  field_2(Code, Pc).


g_link(A, V) :-
  b_setval(A, V).

g_expand(A) :-
  b_getval(A, ARef),
  functor(ARef, B, Arity),
  NewArity is Arity * 2,
  functor(F, B, NewArity),
  ARef =.. AList,
  F =.. FList,
  length(L, Arity),
  append([_ | L], E, FList),
  append(AList, E, NewList),
  NewA =.. NewList,
  g_link(A, NewA).

g_read(A, V) :-
  b_getval(A, V).

g_inc(A, V) :-
  g_read(A, Aref),
  V is Aref + 1,
  g_link(A, V).


init :-
  rvm_code(A),
  atom_codes(A, C),
  g_link(code, C).

main :-
  init,
  get_int_(0, N),
  init_symtbl(N, T0),
  special(nil, NIL),
  build_symtbl(T0, NIL, 0, T),
  decode(T, Instructions),
  finalize_decode(T, _),
  field_0(Instructions, Pc1),
  field_2(Pc1, Pc),
  rib_(5, 0, 0, Rib1),
  rib_(0, 0, Rib1, Stack),
  % gspy(run),
  % gspy(primitive/4),
  % gspy(list_tail),
  % open("debug.out.rvm", write, _, [alias(debug_out), close_on_abort(true)]),
  (run(Pc, Stack) -> halt ; halt).

:- initialization((main; halt)). % start program

