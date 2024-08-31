;;; File: prim-fl.scm

;;==============================================================================

;; Utilities


;;------------------------------------------------------------------------------

;; conversions

(define-primitive (##fl-32-sign rib)
  "{
    obj florib = pop();
    obj sign = TAG_NUM((NUM(CAR(florib)) & 256) >> 8);
    push2(sign, PAIR_TAG);
    break;
   }")

(define-primitive (##fl-32-exponent rib)
  "{
    obj florib = pop();
    obj exp = TAG_NUM(NUM(CAR(florib)) & 255);
    push2(exp, PAIR_TAG);
    break;
   }")

(define ##fl-32-mantissa ##field1)

(define (scm2flonum-32 sign exponent mantissa)
  (##scm2flonum-32 sign exponent (bn-decode mantissa)))

(define-primitive (##scm2flonum-32 sign exp mantissa)
  ;; builds a florib with the host specific encoding
  "{
    obj mantissa = pop();
    obj exp = pop();
    obj sign = pop();
    obj field_0 = TAG_NUM((NUM(sign) << 8) + NUM(exp));
    obj florib = TAG_RIB(alloc_rib(field_0, mantissa, TAG_NUM(8)));
    push2(florib, PAIR_TAG);
    break;
  }")


;; IEEE representation in C

(define-feature float_union
  (decl
   "union
    {
      int32_t ieee;
      float f;
    } float_a, float_b, res;"))


;; Conversions 

(define-feature florib2ieee
  (decl
   "int32_t florib2ieee(rib* florib){
      // sign and exponent + mantissa
      return (UNTAG(CAR(florib)) << 23) + UNTAG(CDR(florib));
    }"))

(define-feature ieee2florib
  (decl
   "obj ieee2florib(int32_t ieee){
      int field_0 = (ieee & 4286578688) >> 23; // sign and exponent
      int field_1 = ieee & 8388607; // mantissa 

      return TAG_RIB(alloc_rib(TAG_NUM(field_0), TAG_NUM(field_1), TAG_NUM(8)));
    }"))

     
;;==============================================================================

;; Numerical operations

;; Numerical procedures that must be defined here are essentially Ribbit's
;; primitives:
;; ##fl=
;; ##fl<
;; ##fl+
;; ##fl*
;; ##fl-
;; ##fl/ (instead of quotient)


(define-primitive (##fl= a b)
  (use float_union florib2ieee ieee2florib)
  "{
    obj b = pop();
    obj a = pop();

    float_a.ieee = florib2ieee(RIB(a));
    float_b.ieee = florib2ieee(RIB(b));

    push2(float_a.f == float_b.f ? TRUE : FALSE, PAIR_TAG);
    break;
  }")

(define-primitive (##fl< a b)
  (use float_union florib2ieee ieee2florib)
  "{
    obj b = pop();
    obj a = pop();

    float_a.ieee = florib2ieee(RIB(a));
    float_b.ieee = florib2ieee(RIB(b));

    push2(float_a.f < float_b.f ? TRUE : FALSE, PAIR_TAG);
    break;
  }")


(define-primitive (##fl+ a b)
  (use float_union florib2ieee ieee2florib)
  "{
    obj b = pop();
    obj a = pop();

    float_a.ieee = florib2ieee(RIB(a));
    float_b.ieee = florib2ieee(RIB(b));

    res.f = float_a.f + float_b.f;

    obj florib = ieee2florib(res.ieee);

    push2(florib, PAIR_TAG);
    break;
  }")


(define-primitive (##fl* a b)
  (use float_union florib2ieee ieee2florib)
  "{
    obj b = pop();
    obj a = pop();

    float_a.ieee = florib2ieee(RIB(a));
    float_b.ieee = florib2ieee(RIB(b));

    res.f = float_a.f * float_b.f;

    obj florib = ieee2florib(res.ieee);

    push2(florib, PAIR_TAG);
    break;
  }")


(define-primitive (##fl- a b)
  (use float_union florib2ieee ieee2florib)
  "{
    obj b = pop();
    obj a = pop();

    float_a.ieee = florib2ieee(RIB(a));
    float_b.ieee = florib2ieee(RIB(b));

    res.f = float_a.f - float_b.f;

    obj florib = ieee2florib(res.ieee);

    push2(florib, PAIR_TAG);
    break;
  }")


(define-primitive (##fl/ a b)
  (use float_union florib2ieee ieee2florib)
  "{
    obj b = pop();
    obj a = pop();

    float_a.ieee = florib2ieee(RIB(a));
    float_b.ieee = florib2ieee(RIB(b));

    res.f = float_a.f / float_b.f;

    obj florib = ieee2florib(res.ieee);

    push2(florib, PAIR_TAG);
    break;
  }")


