# Practical guide for VMs markup / Guide pratique pour l'annotation de VMs

## Prelude : comprendre le système d'annotation

Le système d'annotation a été expliqué en démonstration. Il consiste à rajouter des marqueurs qui donnent de l'information supplémentaire au compilateur afin de connaitre : 
1. l'emplacement des primitives (et l'ordre)
2. comment changer la chaine de caractère initiale (bytecode)
3. les morceaux de code à ajouter ou retirer afin d'activer certaines fonctionnalités

Les machines virtuelles suivantes ont déjà été annotées, vous pouvez vous fier sur ceux-ci afin de voir comment le système fonctionne.

Ces annotations permettent au compilateur de :
- réarranger les primitives
- retirer les primitives qui ne sont pas nécessaires
- activer/désactiver les fonctionnalités

À noté que chaque annotation commence par `@@(` et fini par `)@@` (un peut comme les s-expressions de scheme. Le premier symbole de l'annotation correspond à son type. Par exemple, voici une annotation simple de type `replace` : 

```
# @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
input=");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" # RVM code that prints HELLO!
# )@@
```

Vous pouvez suivre les étapes suivantes pour faire les annotations de votre RVM. N'hésitez pas à me contacter sur github ou par courriel si vous avez des questions. 

Courriel : leonard.oest.oleary@umontreal.ca

github : leo-ard

## Étape 1 : Changement du nombre d'arguments

L'entier qui correspondait aux nombres d'arguments a été légèrement changé.  Maintenant, le dernier bit détermine si la fonction est variadique ou non (0 ou 1). Une fonction est variadique si elle accepte un nombre variable de paramètres.  Ainsi, un 4 correspond à une fonction qui prend 2 arguments et qui n'est pas variadique et un 5 correspond à une fonction qui prend 2 arguments avec des arguments variadiques.
```
0 -> 000 -> aucun argument, la fonction ne prend pas d'argument variadique
1 -> 001 -> aucun argument, les arguments variadiques sont acceptés
2 -> 010 -> 1 argument, la fonction ne prend pas d'argument variadique
3 -> 011 -> 1 argument, les arguments variadiques sont acceptés
4 -> 100 -> 2 arguments, la fonction ne prend pas d'argument variadique,
etc...
```

Pour faire le changement, vous pouvez soit diviser le résultat par 2, ou faire un décalage d'un bit vers la droite. [Voici le changement que j'ai fait pour rvm.js](https://github.com/udem-dlteam/ribbit/blob/8dfb16f1cd0168a97c4bf2fab7a46bc5ec19fe94/src/host/js/rvm.js#L372) :  
```js
let nparams = c[0] >> 1; 
```


## Étape 2 : L'annotation *replace*

Pour que votre `rvm` supporte le remplacement de la chaine de caractères contenant le *bytecode*, il faut rajouter l'instruction *replace*. L'annotation *replace* prend 2 arguments, soit la chaine de caractères à remplacer et le contenu avec laquelle le faire. Par exemple, pour `js`, on a : 
```js
input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"; // @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92))@@
```

Ici, on remplace `);'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y` (sans les `"`) par `(encode 92)` qui nous retourne le bytecode de notre programme encodé avec 92 code possible par caractère (encodage normal).

Pour tester, vous pouvez faire :

```
gsi rsc.scm -t <votre target> -l empty tests/00-empty.scm -o <fichier-de-votre-choix>
```

et regarder le contenu de `<fichier-de-votre-choix>` pour voir si la chaine de caractère s'est bel et bien modifiée. Elle devrait maintenant être égal à : `#di,,,,;'i$!':lkl{` (qui correspond au *bytecode* d'un programme vide).

## Étape 3: Annotation des primitives

Vous pouvez maintenant annoter les primitives. Pour ce faire, il faut ajouter les instructions `primitives`, et à l'intérieur de cette annotation, plusieurs annotations `primitive` pour chacune que vous avez dans votre code. L'annotation `primitives` va remplacer tout le code qu'elle contient par les primitives que le compilateur juge utiles. L'instruction `(gen ...)` permet de dire au compilateur *comment* générer chaque primitive. Par exemple, voici l'annotation en `c` :

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
  } // fin du switch
  // )@@
} // fin de la fonction prim
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
  prim3(lambda z,y,x:[x,y,z]), # @@(primitive (rib a b c))@@
  prim1(lambda x:x), # @@(primitive (id x))@@
  pop, # @@(primitive (arg1 x y))@@
  arg2, # @@(primitive (arg2 x y))@@
  close, # @@(primitive (close rib))@@
  prim1(lambda x:to_bool(is_rib(x))), # @@(primitive (rib? rib))@@
  prim1(lambda x:x[0]), # @@(primitive (field0 rib))@@
  prim1(lambda x:x[1]), # @@(primitive (field1 rib))@@
  prim1(lambda x:x[2]), # @@(primitive (field2 rib))@@
  prim2(f0s), # @@(primitive (field0-set! rib x))@@
  prim2(f1s), # @@(primitive (field1-set! rib x))@@
  prim2(f2s), # @@(primitive (field2-set! rib x))@@
  prim2(lambda y,x:to_bool(x is y if is_rib(x) or is_rib(y) else x==y)), # @@(primitive (eqv? x y))@@
  prim2(lambda y,x:to_bool(x<y)), # @@(primitive (< a b))@@
  prim2(lambda y,x:x+y), # @@(primitive (+ a b))@@
  prim2(lambda y,x:x-y), # @@(primitive (- a b))@@
  prim2(lambda y,x:x*y), # @@(primitive (* a b))@@
  prim2(lambda y,x:int(x/y)), # @@(primitive (quotient a b))@@
  getchar, # @@(primitive (getchar))@@
  prim1(putchar), # @@(primitive (putchar c))@@
  prim1(exit), # @@(primitive (exit a))@@
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
(field0-set!rib x)
(field1-set!ribx)
(field2-set!rib x)
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
box 0: // jump/call
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
         // Here, c[0] & 1 corresponds to whether the function accepts variadic arguments or not
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
