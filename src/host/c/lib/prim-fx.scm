;;; File: prim-fx.scm

;; Basic arithmetic operations (+, *, -, and quotient) are performed here whenever
;; possible, i.e. when both arguments are fixnums and the operation doesn't result
;; in an overflow.

;; See Hacker's Delight by Henry Warren, section 2-13 (Overflow Detection).


;; (define-feature int_size ;; FIXME use this instead of assuming 4 bytes int size?
;;   (decl
;;    "#define INT_SIZE sizeof(int);"))


(define-primitive (##+? x y)
  " { // ##+?
   PRIM2();
   if (IS_NUM(x) && IS_NUM(y)){ // fixnums? 
     int a = NUM(x);
     int b = NUM(y);
     int z = ~(a ^ b) & (1 << 31); 
     push2(((z & ~(((a ^ z) + b) ^ b)) == 0) ? TAG_NUM(a + b) : FALSE, PAIR_TAG);
     break;
   }
   push2(FALSE, PAIR_TAG);
   break;
 }")


(define-primitive (##*? x y)
  " { // ##*?
   PRIM2();
   if (IS_NUM(x) && IS_NUM(y)){ // fixnums?
     int a = NUM(x);
     int b = NUM(y);
     int z = a * b;
     push2(((((b < 0) & (a == 0x80000000))) | ((b != 0) && (z/b != a))) ? FALSE : TAG_NUM(z), PAIR_TAG);
     break;
   }
   push2(FALSE, PAIR_TAG);
   break;
 }")


(define-primitive (##-? x y)
  " { // ##-?
   PRIM2();
   if (IS_NUM(x) && IS_NUM(y)){ // fixnums? 
     int a = NUM(x);
     int b = NUM(y);
     int z = (a ^ b) & (1 << 31); 
     push2(((z & (((a ^ z) - b) ^ b)) == 0) ? TAG_NUM(a - b) : FALSE, PAIR_TAG);
     break;
   }
   push2(FALSE, PAIR_TAG);
   break;
 }")


(define-primitive (##quotient? x y)
  " { // ##quotient? and ##/?
   PRIM2();
   if (IS_NUM(x) && IS_NUM(y)){ // fixnums? 
     int a = NUM(x);
     int b = NUM(y);
     push2(((b == 0) | ((a == (1 << 31) && (b == -1)))) ? FALSE: TAG_NUM(a / b), PAIR_TAG);
     break;
   }
   push2(FALSE, PAIR_TAG);
   break;
 }")

