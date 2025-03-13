;;; File: prim-timer.scm
;; Implement the timer primitives. Timer primitives are used
;; with the time library (see src/lib/time.scm)

;; Add c global definitions to keep track of time
(define-feature c/include-sys/time.h
  ((import "#include <sys/time.h>\n")))

(define-feature c/timer/globals
  (use c/include-sys/time.h)
  ((decl "
struct timeval time_before;
struct timeval time_after;
")))

(define-feature c/timer/gc-globals
  (use c/include-sys/time.h)
  ((decl "
struct timeval time_gc_before;
struct timeval time_gc_after;
long long time_gc_accumulated;
int should_clock_gc = 0;
int gc_invocations = 0;
")
   (gc-start "
if(should_clock_gc == 1) {
  gc_invocations++;
  if(gettimeofday(&time_gc_before, 0) != 0) {
    printf(\"***error while grabbing time...\");
    exit(1);
  };
}
")
   (gc-end "
if(should_clock_gc == 1) {
  if(gettimeofday(&time_gc_after, 0) != 0){
    printf(\"***Error while grabbing time...\");
    exit(1);
  }
  time_gc_accumulated += (time_gc_after.tv_sec-time_gc_before.tv_sec)*1000000LL + time_gc_after.tv_usec-time_gc_before.tv_usec;
  gc_invocations++;
}
")))


;; Starts a new timer
(define-primitive (##timer-start)
  (use c/timer/globals c/timer/gc-globals)
  "{
  should_clock_gc = 1;
  gc_invocations = 0;
  time_gc_accumulated = 0;
  if(gettimeofday(&time_before, 0) != 0) {
    printf(\"***error while grabbing time...\");
    exit(1);
  };
  push(TAG_NUM(0));
  break;
  }")

;; Ends the timer
(define-primitive (##timer-end)
  (use c/time/globals c/time/gc-globals)
  "{
  if(gettimeofday(&time_after, 0) != 0) {
    printf(\"***error while grabbing time...\");
    exit(1);
  };

  should_clock_gc = 0;
  push(TAG_NUM(0));
  break;
}")

;; Display a timer in the format '2.623414 seconds (0.028148 seconds in GC, 3630 invocations)'
(define-primitive (##timer-display)
  (use c/time/globals c/time/gc-globals)
  "{
long long time_difference_ns = (time_after.tv_sec-time_before.tv_sec)*1000000LL + time_after.tv_usec-time_before.tv_usec;
printf(
  \"%.6f seconds (%.6f seconds in GC, %d invocations)\\n\",
  ((double)time_difference_ns) / 1000000,
  ((double)time_gc_accumulated) / 1000000, gc_invocations
);
push(TAG_NUM(0));
break;
}")

