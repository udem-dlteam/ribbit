;;; File: prim-timer.scm
;; Implement the timer primitives. Timer primitives are used
;; with the time library (see src/lib/time.scm)

;; Add c global definitions to keep track of time

(define-feature c/time/profiling
  (use c/include-stdio.h c/include-stdlib.h)
  ((decl
"
#if __has_builtin(__builtin_ia32_rdtscp)
#define USE___builtin_ia32_rdtscp
#endif

#if __has_builtin(__builtin_readcyclecounter)
#define USE___builtin_readcyclecounter
#endif

#ifdef USE___builtin_ia32_rdtscp
#ifdef __GNUC__

#define GET_CYCLECOUNT()\
({ int64_t temp; __asm__ __volatile__ (\"rdtsc\\n\\tshl $32,%%rdx\\n\\tor %%rdx,%%rax\\n\\tmov %%rax,%0\\n\":\"=r\"(temp)::\"%rax\",\"%rbx\",\"%rcx\",\"%rdx\"); temp; })

#endif
#endif

#ifndef GET_CYCLECOUNT
#ifdef USE___builtin_readcyclecounter

#define GET_CYCLECOUNT() \
__builtin_readcyclecounter ()

#endif
#endif

#ifndef GET_CYCLECOUNT
#define GET_CYCLECOUNT() exit(1);
#endif
"
)))


(define-feature c/time/profiling-decl
  (use c/include-stdint.h)
  ((decl "
int64_t __profiling_total_start = 0;
int64_t __profiling_total_total = 0;

#ifdef __profiling_debug
int64_t __profiling_total_started = 0;

enum {
  __profiling_nothing,
  __profiling_add_ref,
  __profiling_remove_ref,
  __profiling_drop,
  __profiling_catch,
  __profiling_collect,
};

int __profiling_status = __profiling_nothing;
#endif

int64_t __profiling_start = 0;
int64_t __temp = 0;
int64_t __profiling_add_ref_total = 0;
int64_t __profiling_remove_ref_total = 0;
int64_t __profiling_drop_total = 0;
int64_t __profiling_catch_total = 0;
int64_t __profiling_collect_total = 0;

int64_t __profiling_gc_total = 0;

")))

(define-feature c/time/profiling-start-end
  (use c/time/profiling-decl)
  ((profiling-total-start "
__profiling_start = 0;
__temp = 0;
__profiling_add_ref_total = 0;
__profiling_remove_ref_total = 0;
__profiling_drop_total = 0;
__profiling_catch_total = 0;
__profiling_collect_total = 0;
int64_t __profiling_gc_total = 0;
__profiling_total_start = GET_CYCLECOUNT();
")
  (profiling-total-end "
__profiling_total_total += GET_CYCLECOUNT() - __profiling_total_start;
")))

(define-feature c/time/add-profiling-locations
  (use c/time/profiling-decl)

  ((profiling-start "__profiling_start = GET_CYCLECOUNT();")
   (profiling-start-remove-ref "
#ifdef __profiling_debug
if (__profiling_status != __profiling_nothing) {
  printf(\"***Error in profiling: start-remove-ref...\\n\");
  exit(1);
}
__profiling_status = __profiling_remove_ref;
#endif
// @@(location profiling-start)@@
")
   (profiling-start-add-ref "
#ifdef __profiling_debug
if (__profiling_status != __profiling_nothing) {
  printf(\"***Error in profiling: start-add-ref %d...\\n\", __profiling_status);
  exit(1);
}
__profiling_status = __profiling_add_ref;
#endif
// @@(location profiling-start)@@
")
    (profiling-start-drop "
#ifdef __profiling_debug
if (__profiling_status != __profiling_nothing) {
  printf(\"***Error in profiling: start-drop...%d\\n\", __profiling_status);
  exit(1);
}
__profiling_status = __profiling_drop;
#endif
// @@(location profiling-start)@@
")
    (profiling-drop->catch "
#ifdef __profiling_debug
if (__profiling_status != __profiling_drop) {
  printf(\"***Error in profiling: drop->catch...\\n\");
  exit(1);
}
__profiling_status = __profiling_catch;
#endif
__temp = GET_CYCLECOUNT();
__profiling_drop_total += __temp - __profiling_start;
__profiling_start = __temp;
")

    (profiling-remove-ref->drop "
#ifdef __profiling_debug
if (__profiling_status != __profiling_remove_ref) {
  printf(\"***Error in profiling: remove-ref->drop...\\n\");
  exit(1);
}
__profiling_status = __profiling_drop;
#endif
__temp = GET_CYCLECOUNT();
__profiling_remove_ref_total += __temp - __profiling_start;
__profiling_start = __temp;
")

    (profiling-catch->collect "
#ifdef __profiling_debug
if (__profiling_status != __profiling_catch) {
  printf(\"***Error in profiling: catch->collect...\\n\");
  exit(1);
}
__profiling_status = __profiling_collect;
#endif
__temp = GET_CYCLECOUNT();
__profiling_catch_total += __temp - __profiling_start;
__profiling_start = __temp;
")

(profiling-stop-collect "
#ifdef __profiling_debug
if (__profiling_status != __profiling_collect) {
  printf(\"***Error in profiling: stop-collect...\\n\");
  exit(1);
}
__profiling_status = __profiling_nothing;
#endif
__profiling_collect_total += GET_CYCLECOUNT() - __profiling_start;
")

(profiling-stop-remove-ref "
#ifdef __profiling_debug
if (__profiling_status != __profiling_remove_ref) {
  printf(\"***Error in profiling: remove_ref...\\n\");
  exit(1);
}
__profiling_status = __profiling_nothing;
#endif
__profiling_remove_ref_total += GET_CYCLECOUNT() - __profiling_start;
")
(profiling-gc-total-end "__profiling_gc_total += GET_CYCLECOUNT() - __profiling_start;")
))

(define-feature c/gc/profile-gc
  (use 
    c/time/add-profiling-locations
    c/time/profiling-start-end
    c/time/profiling


    ))

;catch, adopt, rerank, collect



;;; TIME LIBRARY
;;;

(define-feature c/include-sys/time.h
  ((import "#include <sys/time.h>\n")))

(define-feature c/include-stdint.h
  ((import "#include <stdint.h>\n")))

(define-feature c/timer/globals
  (use c/include-sys/time.h)
  ((decl "
struct timeval time_before;
struct timeval time_after;
")))

(define-feature c/timer/gc-globals
  (use c/include-sys/time.h c/timer/gc-start-end)
  ((decl "
struct timeval time_gc_before;
struct timeval time_gc_after;
long long time_gc_accumulated = 0;
int gc_invocations = 0;
int gc_timer_started = 0;
")
   ))


(define-feature c/timer/gc-start-end
  (use c/timer/globals)
  ((gc-start "
gc_invocations++;
if (gc_timer_started == 1) {
  printf(\"***Error: gc timer was already started...\");
  exit(1);
}
gc_timer_started = 1;
if(gettimeofday(&time_gc_before, 0) != 0) {
  printf(\"***error while grabbing time...\");
  exit(1);
};

// @@(location profiling-start)@@
")
   (gc-end "
// @@(location profiling-gc-total-end)@@

if (gc_timer_started == 0) {
  printf(\"***Error: gc timer was not started...\");
  exit(1);
}
if(gettimeofday(&time_gc_after, 0) != 0){
  printf(\"***Error while grabbing time...\");
  exit(1);
}
long long time_difference_ns = (time_gc_after.tv_sec-time_gc_before.tv_sec)*1000000LL + time_gc_after.tv_usec-time_gc_before.tv_usec;
time_gc_accumulated += time_difference_ns;
gc_invocations++;
gc_timer_started = 0;
")))

;; Starts a new timer
(define-primitive (##timer-start)
  (use c/timer/globals c/timer/gc-globals)
  "{
  if(gettimeofday(&time_before, 0) != 0) {
    printf(\"***error while grabbing time...\");
    exit(1);
  };

  // @@(location profiling-total-start)@@

  push(TAG_NUM(0));
  break;
  }")

;; Ends the timer
(define-primitive (##timer-end)
  (use c/time/globals c/time/gc-globals)
  "{

  // @@(location profiling-total-end)@@

  if(gettimeofday(&time_after, 0) != 0) {
    printf(\"***error while grabbing time...\");
    exit(1);
  };

  push(TAG_NUM(0));
  break;
}")

;;;; Display a timer in the format '2.623414 seconds (0.028148 seconds in GC, 3630 invocations)'
;;(define-primitive (##timer-display)
;;  (use c/time/globals c/time/gc-globals)
;;  "{
;;long long time_difference_ns = (time_after.tv_sec-time_before.tv_sec)*1000000LL + time_after.tv_usec-time_before.tv_usec;
;;printf(
;;  \"%.6f seconds (%.6f seconds in GC, %d invocations)\\n\",
;;  ((double)time_difference_ns) / 1000000,
;;  ((double)time_gc_accumulated) / 1000000, gc_invocations
;;);
;;push(TAG_NUM(0));
;;break;
;;}")
  
  (define-primitive (##timer-display)
    (use c/time/globals c/time/gc-globals c/time/profiling-decl)
    "{
  long long time_difference_ns = (time_after.tv_sec-time_before.tv_sec)*1000000LL + time_after.tv_usec-time_before.tv_usec;
  printf(
    \"%.6f seconds (%.6f seconds in GC, %d invocations, %ld total cycles, %ld gc cycles(%.3f), %ld drop cycles (%.3f), %ld catch cycles(%.3f), %ld collect cycles(%.3f), %ld remove-ref cycles(%.3f))\\n\",
    ((double)time_difference_ns) / 1000000,
    ((double)time_gc_accumulated) / 1000000, 
    gc_invocations,
    __profiling_total_total,
    __profiling_gc_total,
    ((float)__profiling_gc_total /(float)__profiling_total_total),
    __profiling_drop_total,
    ((float)__profiling_drop_total/(float)__profiling_total_total),
    __profiling_catch_total,
    ((float)__profiling_catch_total/(float)__profiling_total_total),
    __profiling_collect_total,
    ((float)__profiling_collect_total/(float)__profiling_total_total),
    __profiling_remove_ref_total,
    ((float)__profiling_remove_ref_total/(float)__profiling_total_total)
    );
  push(TAG_NUM(0));
  break;
  }")
