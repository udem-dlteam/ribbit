# Practical guide for VMs markup / Guide pratique pour l'annotation de VMs

## Prelude : comprendre le système d'annotation

Le système d'annotation a été expliqué en démonstration. Il consiste à rajouter des marqueurs qui donnent de l'information supplémentaire au compilateur afin de connaitre : 
1. l'emplacement des primitives (et l'ordre)
2. comment changer la string initial (bytecode)
3. les morceaux de code à ajouter ou retirer afin d'activer certaines fonctionnalités

Les vms suivantes ont déjà été annotées, vous pouvez vous fier sur ceux-ci afin de voir comment le système fonctionne.

Ces annotations permettent au compilateur de :
- réarranger les primitives
- retirer les primitives qui ne sont pas nécessaires
- activer/désactiver les fonctionnalités

À noté que chaque annotation commence par `@@(` et fini par `)@@` (un peut comme les s-expressions de scheme. Le premier symbole que l'annotation contient correspond à son type. Par exemple, voici une annotation simple de type `replace` : 

```
# @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
input=");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" # RVM code that prints HELLO!
# )@@
```

Vous pouvez suivre les étapes suivantes pour faire les annotations de votre RVM. N'hésitez pas à me contacter sur github ou par courriel si vous avez des questions. 
Courriel : leonard.oest.oleary@umontreal.ca
github : leo-ard

## Étape 1 : Changement du nombre d'arguments

L'entier qui correspondait aux nombres d'arguments a été légèrement changé.  Maintenant, le dernier bit détermine si la fonction est *variadic* ou non (0 ou 1).  Ainsi, un 4 correspond à une fonction qui prend 2 arguments et qui n'est pas *variadic* et un 5 correspond à une fonction qui prend 2 arguments avec des arguments variadics.
```
0 -> 000 -> aucun argument, la fonction ne prend pas d'argument variadic
1 -> 001 -> aucun argument, les arguments variadics sont acceptés
2 -> 010 -> 1 argument, la fonction ne prend pas d'argument variadic
3 -> 011 -> 1 argument, les arguments variadics sont acceptés
4 -> 100 -> 2 arguments, la fonction ne prend pas d'argument variadic,
etc...
```

Pour faire le changement, vous pouvez soit diviser le résultat par 2, ou faire un bitshift vers la droite. Voici le changement que j'ai fait pour rvm.js.

## Étape 2 : Annotation du replace

Pour que votre `rvm` supporte le remplacement de la string contenant le bytecode, il faut rajouter l'instruction `replace`. Replace prend 2 arguments, soit la string à remplacé et le contenu avec laquelle le faire. Par exemple, pour `js`, on a : 
```js
input = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"; // @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92))@@
```

Ici, on remplace `);'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y` (sans les `"`) par `(encode 92)` qui nous retourne le bytecode de notre programme encodé sur 92 bits (encodage normal).

Pour tester, vous pouvez faire :

```
gsi rsc.scm -t <votre target> -l empty tests/00-empty.scm -o <fichier-de-votre-choix>
```

et regarder le contenu de `<fichier-de-votre-choix>` pour voir si la string s'est bel et bien modifié. Elle devrait maintenant être égal à : `#di,,,,;'i$!':lkl{` (qui correspond au bytecode d'un programme vide).

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

On peut voir chaque annotation dans les commentaires. Pour comprendre ce que l'instruction `(gen "case " index ":" body)` fait, il faut d'abord comprendre le concept de `head` et `body`. Chaque annotation contient un `head` et un `body`. La première ligne de la définition de l'annotation correspond au `head`. Le reste des lignes correspond au body. Par exemple, pour l'annotation `(primitive (id x))`, le` head` est : 
```
      case 2: // @@(primitive (arg1 x y)
```
et le `body` est : 
```
      {
        pop();
        break;
      } // )@@
```
L'instruction `(gen "case " index ":" body)` nous indique que pour générer une primitive, il faut faire : "case " + <l'index de la primitive> + ":" + <le body de la primitive>. Si on concatène ces éléments, ça nous donne le bon résultat !

Regardons un autre exemple, celui de `py` : 

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

Ici, les annotations sont sur la même ligne. Dans ce cas, le `head` et le `body` de la primitive va être le même, soit la ligne sur laquelle elle est. C'est pourquoi on peut simplement écrire `(gen body)` pour la génération de primitive.

Maintenant, à vous de jouer ! Vous pouvez annoter les primitives. Voici toutes les primitives et leur signature qui doivent être définies dans votre rvm. Le nom de la primitive est très important ! 
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

Pour tester votre ajout des primitives, vous pouvez exécuter la commande suivante : 
```
gsi rsc.scm -t <votre-langage> -l empty tests/01-putchar.scm -o <output-file>
```
ou : 
- <votre-langage> est votre language (par exemple, `js`)
- <output-file> est un emplacement de votre choix

Vous pouvez ensuite observer le contenu de <output-file>. Vous devriez voir que seulement les primitives `rib`, `putchar`, `close`, `arg2`, `arg1`, `id` sont présente.

## Étape 4 : Feature `arity-check`

Maintenant que votre `rvm` contient les annotations des primitives et de la string initial, on peut ajouter le feature `arity-check` à notre rvm. Lorsque que le feature `arity-check` est activé, le compilateur va générer un bytecode qui push le nombre d'arguments sur la pile avant chaque appel de fonction. Il faut donc `pop()` ce nombre d'arguments et le conserver dans une variable. Par la suite, il faut rajouter une garde qui regarde si le nombre d'arguments est adéquoit selon la signature de la fonction. Le `pop` et le check de la signature doivent être dans des annotations `@@(feature arity-check ...)@@` afin de pouvoir retirer ce code si jamais `arity-check` n'est pas activé. 

```js
case 0: // jump/call
    if (debug) { console.log((pc[2]===0 ? "--- jump " : "--- call ") + show_opnd(o)); show_stack(); } //debug
    o = get_opnd(o)[0];
    // @@(feature arity-check
    // Sauvegarde le nombre d'arguments avec lequel on a appelé la fonction
    let ncall=pop();
    // )@@
    let c = o[0];

    if (is_rib(c)) {
        let c2 = [0,o,0];
        let s2 = c2;
        
        // Un bitshift doit être fait dans tous les cas (voir étape 2)
        let nargs = c[0] >> 1; 
        // @@(feature arity-check
        // Est-ce que la fonction supporte des arguments variadics ?
        let vari = c[0] & 1;
        if ((!vari && nargs != ncall) || (vari && nargs > ncall)){
            // On afficher une erreur et on crash si oui
            console.log("*** Unexpected number of arguments ncall:", ncall, " nargs:", nargs, "variadics:", vari);
            halt();
        }
        // )@@
```

Vous pouvez maintenant tester votre code avec les commandes suivantes : 
```
# Sans arity-check 
gsi rsc.scm -t <votre-target> -l max tests/36-fact.scm -o <output>
<votre-interpreteur> <output>

# Avec arity-check 
gsi rsc.scm -t <votre-target> -l max -f+ arity-check tests/36-fact.scm -o <output>
<votre-interpreteur> <output>
```

## Étape 5 : Changement de la string HELLO!

Maintenant que votre programme supporte par défaut le nombre d'arguments variadics, il faut changer le bytecode qui affiche "HELLO!" afin que votre fichier fonctionne en tout seul. Pour ce faire, vous pouvez simplement remplacer `);'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y` par :
```
);'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{
```

N'oubliez pas de le faire aussi dans l'annotation *replace*. 

Pour tester, vous pouvez simplement exécuter votre `rvm` normalement et vous devriez voir `HELLO!`.


## Étape 6: Tout tester

Pour tout tester, vous pouvez faire : 

```bash 
# Test le compilateur "pipeline"
HOST="<votre-target>" make check

# Test le compilateur "fancy"
HOST="<votre-target>" make check-fancy
```

Voir l'énoncé du travail pour tester le bootstrap

