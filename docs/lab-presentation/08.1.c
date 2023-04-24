// FR:
// Un dernier aspect : 
//  - Les features

//On veut développer un système qui permet de montré la dépendance de code:

// @@(feature boolean@@ < --- on annote le feature ici


// EN:
// One last aspect:
// - Features

//We developed a system that lets us show dependency between features.


// @@(feature boolean@@ < --- fr: on annote le feature ici / en: we annoate the feature here
obj boolean(bool x) { return x ? CAR(FALSE) : FALSE; }
// @@)@@

void prim(int no) {
  switch (no) {
  // @@(primitives (gen "\ncase " index ":" body ) @@

  case 0: // @@(primitive (rib a b c) @@
  {
    obj new_rib = TAG_RIB(alloc_rib(NUM_0, NUM_0, NUM_0));
    PRIM3();

    ...

    obj y = CDR(stack);
    TOS = TAG_RIB(alloc_rib(x, y, CLOSURE_TAG));
    break;
  } //@@)@@
  case 5: // @@(primitive (rib? rib) (uses boolean)@@  <--- fr: On utilise "boolean" ici / en: we utilise "boolean" here
  {
    PRIM1();
    push2(boolean(IS_RIB(x)), PAIR_TAG);
    break;
  } //@@)@@

...
