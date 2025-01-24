(define-primitive (gc_check)
                  (use debug/clean-ribs)
                  
  "{
    gc(true);
    push2(TAG_NUM(0), PAIR_TAG);
    break;
   }")

(define-primitive (reset_d_count)
  "{
    d_count = 0;
    push2(TAG_NUM(0), PAIR_TAG);
    break;
}")

(define-primitive (print_d_count)
  "{
    printf(\"***D_COUNT=%d\\n\", d_count);
    push2(TAG_NUM(0), PAIR_TAG);
    break;
}")

(define-primitive (viz_heap x) 
      (use scm2str debug/rib-viz)
  "{
    PRIM1();
    char* s = scm2str(x);
    DEC_PRIM1();
    viz_heap(s);
    push2(TAG_NUM(0), PAIR_TAG);
}")

