;; Library to test the AGC.
;;
;; Allows the visualization of the heap, forcing a gc pass and count the number of 


(define-primitive (gc_check)
                  (use debug/clean-ribs)
  "{
    gc();
    push(TAG_NUM(0));
    break;
   }")

(define-primitive (reset_d_count)
  "{
    d_count = 0;
    push(TAG_NUM(0));
    break;
}")

(define-primitive (print_d_count)
  "{
    printf(\"***D_COUNT=%d\\n\", d_count);
    push(TAG_NUM(0));
    break;
}")

(define-primitive (viz_heap x) 
      (use scm2str debug/rib-viz)
  "{
    PRIM1();
    char* s = scm2str(x);
    DEC_PRIM1();
    viz_heap(s);
    push(TAG_NUM(0));
}")

