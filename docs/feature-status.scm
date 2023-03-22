# Features

## General Syntax

> Feature definition should be put in comments, but it is not required.

Multiline:

- Start: `@@(`
- End: `)@@`

Inline:

- `@@(`...`)@@`


## Legend
- parentheses `()` mean literal parentheses. Ex: `(use)` means `(use)`
- in `<...>`, replace by what the `...` mean. Ex: `<var>` means you need to put a variable there.
- in `[...]` means the `...` is optional
- `+` means one or more times
- in `{...}`, replace by source code in the language where the feature is defined.
  Ex: `{HEAD} @@(feature <name> {BODY})@@` could be
```c
foobar(); // @@(feature boolean
obj boolean(bool x) { return x ? CAR(FALSE) : FALSE; }
// )@@
```
Here:
- `{HEAD}` is `foobar(); // @@(feature boolean\n`
- `{BODY}` is `obj boolean(bool x) { return x ? CAR(FALSE) : FALSE; } \n// )@@\n`
### Note
When feature (or primitive) definition are on the same line (`{BODY}` is empty), the value of `{BODY}` is
set to the value of `{HEAD}`. (`{HEAD}` == `{BODY}`)
Ex:
```js
prim1((x) => x) //  @@(primitive (id x))@@
```
Here, the `{HEAD}` is `prim1((x) => x) // @@(primitive (id x))@@\n`. Because the definition is a one-liner, the value
of `{BODY}` is set to the value of `{HEAD}`.

## Meta-annotation constructs

### Features 

#### `{HEAD} @@(feature <condition> [(use <otherFeature>+)] [{BODY}])@@` 

The feature annotation locates a feature to be used.
- `condition` : a condition under which the `{BODY}` code is expanded 
  - Ex: `{HEAD} @@(feature boolean [(use <otherFeature>+)] [{BODY}])@@` means _include this feature
    if `(eq? boolean #t)`_

  - Ex: `{HEAD} @@(feature (and boolean getchar) [(use <otherFeature>+)] [{BODY}])@@` means _include this
    feature
    if `(and (eq? boolean #t) (eq? getchar #t))`_

> `(use <feature>)` would become the same (semantically) as `(set! <feature> #t)`
- `use` : Specify dependencies of this feature, meaning features to be activated if this one is (like primitives).


### Primitives

#### `{HEAD} @@(primitives (gen <expr>+) {BODY})@@`

The primitives annotation specify the location of all the primitives. Here `gen` is evaluated to generate each primitive's
code base on the expressions given (it evaluates them similarly to `string append`). The `{BODY}` must contain `primitive`
annotations to specify the location of individual primitive.
 - `gen` :  `expr`s must be string literals or one of the special values:
   - `index`: final primitive index (the one decided after the filtering of unused primitives)
   - `body`: the value of `{BODY}`
   - `head`: the value of `{HEAD}`


### Primitive

#### `{HEAD} @@(primitive <signature> [(use <feature>+)] [{BODY}])@@`
This `primitive` annotation specify the location of a sigle primitive. It must be inside a `primitives` annotation (see above)
- `signature` : A name and arguments as an sexp (for example `(rib a b c)`)
- `use` : A list of primitive or feature that is used by this primitive.


### Replace
#### `{HEAD} @@(replace <symbol> <expr> [{BODY}])@@`
The `replace` command, replaces all instances of `symbol` in the `{BODY}` by the value returned by `expr`:
    - `symbol` must be the text to replace as a symbol or string.
    - `expr` must be a valid scheme expression that returns a string or one of the special values (or a mix of both):
        - `source`: the compiled source code
        - `line`: the current line in the file when the `replace` is evaluated.
        - ... Maybe others?
          <br>
          <br>
    - Ex: `__SOURCE__` will be replaced by the compiled source code:
      ```c
      char *input = "__SOURCE__"; // @@(replace __SOURCE__ source)@@
      ```


### Location

#### `@@(location <name>)@@`

This `location` annotation defined a location in the code where the features can live


## Macro-available contructs

#### `(define-primitive <signature> [(use <feature>+)] <code>)`
This `define-primitive` macro lets the programmer extend the host by extending the primitives. 
- `signature` : A name and arguments as an sexp (for example `(rib a b c)`)
- `use` : A list of primitive or feature that is used by this primitive.
- `code` : Code of the primitive

### Feature

#### `(define-feature <condition> [(use <feature>+)] <location-code-pair>+)`

This `define-feature` macro lets the programmer add primitive at compile-time. 
 - `condition` is a condition under which the feature is added (made of used primitives). 
 - `use` lists the numer of features (or primitives) needed by this one
 - `location-code-pair` pair of location with their respective code. For example (targeting `js`) : 

 ```scheme
(define-feature rib_eater (use rib_to_any)
  (decl "collected_ribs = [];
         function eat_rib(r) { collected_ribs.push(rib_to_any(r)); }")
  (start "console.log('Rib eater is activated');")
  (end "console.log('Here are my lovely eaten ribs (miam) : ', collected_ribs)"))
 ```

