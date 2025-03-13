(if-feature c/gc/es

  ;; ref count or even-shiloach gc
  (define-primitive
    (##apply f args)
    (use es-apply)
    "{
  PRIM2();
  set_rank(NIL, 1);
  TEMP4 = x; // protect x
  add_ref(NIL, x, 1);
  int num_args = 0;
  obj arg = TAG_RIB(y);
  while (arg != NIL) {
    push(arg);
    arg = CAR(stack);
    SET_CAR(stack, CAR(arg));
    arg = TAG_RIB(CDR(arg));
    num_args++;
  }
  push(TAG_NUM(num_args));
  DEC_PRIM2();
  return TAG_RIB(x);
}")

  ;; mark-and-sweep or stop-and-copy gc
  (define-primitive
    (##apply f args)
      "{
  PRIM2();
  TEMP1 = x; // save x for the gc
  int num_args = 0;
  obj arg = TAG_RIB(y);
  while (arg != NIL) {
    push2(arg, PAIR_TAG); // make sure the arg doesn't get GC'd
    arg = CAR(stack);
    CAR(stack) = CAR(arg);
    arg = TAG_RIB(CDR(arg));
    num_args++;
  }
  push2(TAG_NUM(num_args), PAIR_TAG);
  x = TEMP1; // retrive x from possibly GC'd
  return TAG_RIB(x);
}"))
