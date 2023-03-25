# La version python :

primitives = [
 # @@(primitives (gen body)@@
 prim3(lambda z,y,x:[x,y,z]),                                            # @@(primitive (rib a b c))@@
 prim1(lambda x:x),                                                      # @@(primitive (id x))@@
 pop,                                                                    # @@(primitive (arg1 x y))@@
 arg2,                                                                   # @@(primitive (arg2 x y))@@
 close,                                                                  # @@(primitive (close rib))@@
 prim1(lambda x:to_bool(is_rib(x))),                                     # @@(primitive (rib? rib))@@
 prim1(lambda x:x[0]),                                                   # @@(primitive (field0 rib))@@
 prim1(lambda x:x[1]),                                                   # @@(primitive (field1 rib))@@
 prim1(lambda x:x[2]),                                                   # @@(primitive (field2 rib))@@
 prim2(f0s),                                                             # @@(primitive (field0-set! rib x))@@
 prim2(f1s),                                                             # @@(primitive (field1-set! rib x))@@
 prim2(f2s),                                                             # @@(primitive (field2-set! rib x))@@
 prim2(lambda y,x:to_bool(x is y if is_rib(x) or is_rib(y) else x==y)),  # @@(primitive (eqv? x y))@@
 prim2(lambda y,x:to_bool(x<y)),                                         # @@(primitive (< a b))@@
 prim2(lambda y,x:x+y),                                                  # @@(primitive (+ a b))@@
 prim2(lambda y,x:x-y),                                                  # @@(primitive (- a b))@@
 prim2(lambda y,x:x*y),                                                  # @@(primitive (* a b))@@
 prim2(lambda y,x:int(x/y)),                                             # @@(primitive (quotient a b))@@
 getchar,                                                                # @@(primitive (getchar))@@
 prim1(putchar),                                                         # @@(primitive (putchar c))@@
 prim1(exit),                                                            # @@(primitive (exit a))@@
 # @@)@@
]
