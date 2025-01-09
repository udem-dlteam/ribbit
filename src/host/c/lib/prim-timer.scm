;;; File: prim-timer.scm
;; Implement the timer primitives. Timer primitives are used
;; with the time library (see src/lib/time.scm)

;; Add c global definitions to keep track of time
(define-feature c/include-clock
  ((import "#include <time.h>\n")))

(define-feature c/timer/globals
  (use c/include-clock)
  ((decl "
clock_t time_before;
clock_t time_difference;
")))

(define-feature c/timer/gc-globals
  (use c/include-clock)
  ((decl "
clock_t time_gc_before;
clock_t time_gc_accumulated;
int should_clock_gc = 0;
int gc_invocations = 0;
")
   (gc-start "
if(should_clock_gc == 1) {gc_invocations++;time_gc_before=clock();}
")
   (gc-end "
if(should_clock_gc == 1) {time_gc_accumulated += clock()-time_gc_before; gc_invocations++;}
")))


;; Starts a new timer
(define-primitive (##timer-start)
  (use c/timer/globals c/timer/gc-globals)
  "{
  should_clock_gc = 1;
  gc_invocations = 0;
  time_gc_accumulated = 0;
  time_before = clock();
  push2(TAG_NUM(0), PAIR_TAG);
  break;
  }")

;; Ends the timer
(define-primitive (##timer-end)
  (use c/time/globals c/time/gc-globals)
  "{
time_difference = clock() - time_before;
should_clock_gc = 0;
push2(TAG_NUM(0), PAIR_TAG);
break;
}")

;; Display a timer in the format '2.623414 seconds (0.028148 seconds in GC, 3630 invocations)'
(define-primitive (##timer-display)
  (use c/time/globals c/time/gc-globals)
  "{
printf(\"%.6f seconds (%.6f seconds in GC, %d invocations)\\n\", ((double)time_difference) / CLOCKS_PER_SEC, ((double)time_gc_accumulated) / CLOCKS_PER_SEC, gc_invocations);
push2(TAG_NUM(0), PAIR_TAG);
break;
}")

