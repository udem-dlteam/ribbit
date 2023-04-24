# Practical guide for VMs markup

## Prelude: understanding the annotation system

The annotation system was explained in demonstration. It consists of adding markers that give additional information to the compiler in order to know:
1. location of primitives (and order)
2. how to change the initial character string (bytecode)
3. the pieces of code to add or remove in order to activate certain functionalities

The following virtual machines have already been annotated, you can rely on these to see how the system is performing.

These annotations allow the compiler to:
- rearrange the primitives
- remove unnecessary primitives
- enable/disable features

Note that each annotation starts with `@@(` and ends with `)@@` (much like the s-expressions of scheme. The first symbol of the annotation corresponds to its type. For example, here is a simple annotation of type `replace`:

```
# @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
input=");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" # RVM code that prints HELLO!
# )@@
```

You can follow the following steps to make the annotations of your RVM. Feel free to contact me on github or by email if you have any questions.

Email: leonard.oest.oleary@umontreal.ca

github: leo-ard

## Step 1: Changing the number of arguments

The integer that corresponded to the numbers of arguments has been changed slightly. Now the last bit determines if the function is variadic or not (0 or 1). A function is variadic if it accepts a variable number of parameters. Thus, a 4 corresponds to a function which takes 2 arguments and which is not variadic and a 5 corresponds to a function which takes 2 arguments with variadic arguments.
```
0 -> 000 -> no argument, the function does not take a variadic argument
1 -> 001 -> no arguments, variadic arguments are accepted
2 -> 010 -> 1 argument, the function does not take a variadic argument
3 -> 011 -> 1 argument, variadic arguments are accepted
4 -> 100 -> 2 arguments, the function does not take a variadic argument,
etc...
```

To make the change, you can either divide the result by 2, or shift one bit to the right. [Here is the change I made for rvm.js](https://github.com/udem-dlteam/ribbit/blob/8dfb16f1cd0168a97c4bf2fab7a46bc5ec19fe94/src/host/js/rvm.js#L372):
```js
let nparams = c[0] >> 1;
```


## Step 2: The *replace* annotation

For your `rvm` to support the replacement of the character string containing the *bytecode*, you must add the *replace* instruction. The *replace* annotation takes 2 arguments, the string to replace and the content with which to do it. For example, for `js`, we have:
```js
input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"; // @@(replace ");'u?> vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92))@@
```

Here, we replace `);'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y` (without the `"`) by `(encode 92 )` which returns the bytecode of our program encoded with 92 possible codes per character (normal encoding).

To test, you can do:

```
gsi rsc.scm -t <your target> -l empty tests/00-empty.scm -o <file-of-your-choice>
```

and look at the contents of `<file-of-your-choice>` to see if the string has changed. It should now be equal to: `#di,,,,;'i$!':lkl{` (which corresponds to the *bytecode* of an empty program).

## Step 3: Annotate Primitives

You can now annotate primitives. This is done by adding the `primitive` instructions, and inside that annotation, multiple `primitive` annotations for each one you have in your code. The `primitives` annotation will replace all the code it contains with whatever primitives the compiler deems useful. The `(gen ...)` instruction tells the compiler *how* to generate each primitive. For example, here is the `c` annotation:

```c
void prim(int no) {
  switch (no) {
      // @@(primitives (gen "case " index ":" body)
      case 0: // @@(primitive (rib a b c)
      {
        obj new_rib = TAG_RIB(alloc_rib(NUM_0, NUM_0, NUM_0));
        PRIM3();
        CAR(new_rib) = x;
        CDR(new_rib) = y;
        TAG(new_rib) = z;
        push2(new_rib, PAIR_TAG);
        break;

      } // )@@
      case 1: // @@(primitive (id x)
      {
        PRIM1();
        push2(x, PAIR_TAG);
        break;
      } // )@@
      case 2: // @@(primitive (arg1 x y)
      {
        pop();
        break;
      } // )@@
      ...
  } // fr: fin du switch / en: end of switch
  // )@@
} // fr: fin de la fonction prim / en: end of function prim
```

You can see each annotation in the comments. To understand what the `(gen "case " index ":" body)` statement does, one must first understand the concept of `head` and `body`. Each annotation contains a `head` and a `body`. The first line of the annotation definition corresponds to the `head`. The rest of the lines correspond to the body. For example, for the annotation `(primitive (id x))`, the `head` is:
```
       box 2: // @@(primitive (arg1 x y)
```
and the `body` is:
```
       {
         pop();
         break;
       } // )@@
```
The instruction `(gen "case " index ":" body)` tells us that to generate a primitive, we must do: "case " + <the index of the primitive> + ":" + <the body of the primitive>. If we concatenate these elements, it gives us the right result!

Let's look at another example, that of `py`:

```
primitives = [
 # @@(primitives (gen body)
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
 # )@@
]
```

Here, the annotations are on the same line. In this case, the `head` and the `body` of the primitive will be the same, which is the line it is on. This is why we can simply write `(gen body)` for the primitive generation.

Now it's your turn ! You can annotate primitives. Here are all the primitives and their signature that need to be defined in your rvm. The name of the primitive is very important!

```
(rib a b c)
(id x)
(arg1 x y)
(arg2 x y)
(close rib)
(rib? rib)
(field0 rib)
(field1 rib)
(field2 rib)
(field0-set! rib x)
(field1-set! rib x)
(field2-set! rib x)
(eqv? x y)
(< a b)
(+ a b)
(- a b)
(* a b)
(quotient a b)
(getchar)
(putchar c)
(exit a)
```

To test your addition of primitives, you can run the following command:
```
gsi rsc.scm -t <your-language> -l empty tests/01-putchar.scm -o <output-file>
```
Or :
- <your-language> is your language (e.g. `js`)
- <output-file> is a location of your choice

You can then observe the contents of <output-file>. You should see only the `rib`, `putchar`, `close`, `arg2`, `arg1`, `id` primitives.

## Step 4: Feature `arity-check`

Now that your `rvm` contains the annotations of the primitives and the initial string, we can add the *feature* `arity-check` to our rvm. When the `arity-check` feature is enabled, the compiler will generate a *bytecode* that pushes the number of arguments on the stack before each function call. We must therefore remove this number of arguments from the stack and keep it in a variable. Thereafter, we must add a guard that checks if the number of arguments is adequate according to the signature of the function. All additions relating to the feature arity check should be the `@@(feature arity-check ...)@@` annotations in order to be able to remove this code if ever `arity-check` is not activated.

```js
case 0: // jump/call
    if (debug) { console.log((pc[2]===0 ? "--- jump " : "--- call ") + show_opnd(o)); show_stack(); } //debug
    o = get_opnd(o)[0];
    // @@(feature arity-check
    let nargs=pop();
    // )@@
    let c = o[0];

    if (is_rib(c)) {
        let c2 = [0,o,0];
        let s2 = c2;

        let nparams = c[0] >> 1;
        // @@(feature arity-check
        // Ici, c[0] & 1 correspond à si la fonction accepte des arguments variadics ou non
        if (c[0] & 1 ? nparams > nargs : nparams != nargs){
            console.log("*** Unexpected number of arguments nargs:", nargs, " nparams:", nparams, "variadics:", c[0]&1);
            halt();
        }
        // )@@
```

You can now test your code with the following commands:
```
# Without arity-check
gsi rsc.scm -t <your-target> -l max tests/36-fact.scm -o <output>
<your-interpreter> <output>

# With arity-check
gsi rsc.scm -t <your-target> -l max -f+ arity-check tests/36-fact.scm -o <output>
<your-interpreter> <output>
```

## Step 5: Changing the HELLO!

Now that your program supports the number of variadic arguments by default, you need to change the *bytecode* that displays "HELLO!" so that your file works on its own. To do this, you can simply replace `);'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y` with:
```
);'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{
```

Don't forget to do this in the *replace* annotation as well.

To test, you can just run your `rvm` normally and you should see `HELLO!`.


## Step 6: Test everything

To test everything, you can do:

```bash
# Test the "pipeline" compiler
HOST="<your-target>" make check

# Test the "fancy" compiler
HOST="<your-target>" make check-fancy
```

On `check-fancy` the `37-variadics.scm` test will not pass, since you haven't implemented the rests arguments. It's normal, don't worry about it!

See the statement of work to test the bootstrap
