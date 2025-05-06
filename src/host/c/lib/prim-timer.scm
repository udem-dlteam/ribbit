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

#define GET_CYCLECOUNT() \
({ int64_t temp; __asm__ __volatile__ (\"rdtscp\\n\\tshl $32,%%rdx\\n\\tor %%rdx,%%rax\\n\\tmov %%rax,%0\\n\\tcpuid\\n\":\"=r\"(temp)::\"%rax\",\"%rbx\",\"%rcx\",\"%rdx\"); temp; })

#endif
#endif

#ifndef GET_CPUCYCLECOUNT
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

(define-feature c/time/profiling-drop
  (use c/time/profiling c/time/profiling-decl)
  (
   (profiling-drop-start
     "
if (should_clock_gc) {
  if (__profiling_drop_started == 1) {
    printf(\"***Error: profiling drop was already started...\");
    exit(1);
  }
  __profiling_drop_started = 1;
  __profiling_drop_start = GET_CYCLECOUNT();
}
")
   (profiling-drop-end
     "
if (should_clock_gc) {
  if (__profiling_drop_started == 0) {
    printf(\"***Error: profiling drop was not started...\");
    exit(1);
  }
  __profiling_drop_started = 0;
  __profiling_drop_total += GET_CYCLECOUNT() - __profiling_drop_start;
}
")))

(define-feature c/time/profiling-decl
  (use c/include-stdint.h)
  ((decl "
int64_t __profiling_total_start = 0;
int64_t __profiling_total_total = 0;
int64_t __profiling_total_started = 0;

int64_t __profiling_catch_start = 0;
int64_t __profiling_catch_total = 0;
int64_t __profiling_catch_started = 0;

int64_t __profiling_drop_start = 0;
int64_t __profiling_drop_total = 0;
int64_t __profiling_drop_started = 0;

int64_t __profiling_rerank_start = 0;
int64_t __profiling_rerank_total = 0;
int64_t __profiling_rerank_started = 0;

int64_t __profiling_adopt_start = 0;
int64_t __profiling_adopt_total = 0;
int64_t __profiling_adopt_started = 0;

int64_t __profiling_collect_start = 0;
int64_t __profiling_collect_total = 0;
int64_t __profiling_collect_started = 0;
")))


(define-feature c/time/profiling-total
  (use c/time/profiling c/include-stdint.h c/time/profiling-decl)
  ((profiling-total-start
     "
if (__profiling_total_started == 1) {
  printf(\"***Error: profiling total was already started...\");
  exit(1);
}
__profiling_total_started = 1;
__profiling_total_start = GET_CYCLECOUNT();
")
   (profiling-total-end
     "
if (__profiling_total_started == 0) {
  printf(\"***Error: profiling total was not started...\");
  exit(1);
}
__profiling_total_started = 0;
__profiling_total_total += GET_CYCLECOUNT() - __profiling_total_start;
")))


(define-feature c/time/profiling-catch
  (use c/time/profiling c/time/profiling-decl)
  ((profiling-catch-start
     "
if (should_clock_gc) {
  if (__profiling_catch_started == 1) {
    printf(\"***Error: profiling catch was already started...\");
    exit(1);
  }
  __profiling_catch_started = 1;
  __profiling_catch_start = GET_CYCLECOUNT();
}
")
   (profiling-catch-end
     "
if (should_clock_gc) {
  if (__profiling_catch_started == 0) {
    printf(\"***Error: profiling catch was not started...\");
    exit(1);
  }
  __profiling_catch_started = 0;
  __profiling_catch_total += GET_CYCLECOUNT() - __profiling_catch_start;
}
")))

(define-feature c/time/profiling-adopt
  (use c/time/profiling c/time/profiling-decl)
  ((profiling-adopt-start
     "
if (should_clock_gc) {
  if (__profiling_adopt_started == 1) {
    printf(\"***Error: profiling adopt was already started...\");
    exit(1);
  }
  __profiling_adopt_started = 1;
  __profiling_adopt_start = GET_CYCLECOUNT();
}
")
   (profiling-adopt-end
     "
if (should_clock_gc) {
  if (__profiling_adopt_started == 0) {
    printf(\"***Error: profiling adopt was not started...\");
    exit(1);
  }
  __profiling_adopt_started = 0;
  __profiling_adopt_total += GET_CYCLECOUNT() - __profiling_adopt_start;
}
")))


(define-feature c/time/profiling-rerank
  (use c/time/profiling c/time/profiling-decl)
  ((profiling-rerank-start
     "
if (should_clock_gc) {
  if (__profiling_rerank_started == 1) {
    printf(\"***Error: profiling rerank was already started...\");
    exit(1);
  }
  __profiling_rerank_started = 1;
  __profiling_rerank_start = GET_CYCLECOUNT();
}
")
   (profiling-rerank-end
     "
if (should_clock_gc) {
  if (__profiling_rerank_started == 0) {
    printf(\"***Error: profiling rerank was not started...\");
    exit(1);
  }
  __profiling_rerank_started = 0;
  __profiling_rerank_total += GET_CYCLECOUNT() - __profiling_rerank_start;
}
")))

(define-feature c/time/profiling-collect
  (use c/time/profiling c/time/profiling-decl)
  ((profiling-collect-start
     "
if (should_clock_gc) {
  if (__profiling_collect_started == 1) {
    printf(\"***Error: profiling collect was already started...\");
    exit(1);
  }
  __profiling_collect_started = 1;
  __profiling_collect_start = GET_CYCLECOUNT();
}
")
   (profiling-collect-end
     "
if (should_clock_gc) {
  if (__profiling_collect_started == 0) {
    printf(\"***Error: profiling collect was not started...\");
    exit(1);
  }
  __profiling_collect_started = 0;
  __profiling_collect_total += GET_CYCLECOUNT() - __profiling_collect_start;
}
")))

(define-feature c/gc/profile-gc
  (use 
    c/time/profiling
    c/time/profiling-drop
    c/time/profiling-total
    c/time/profiling-catch
    c/time/profiling-adopt
    c/time/profiling-rerank
    c/time/profiling-collect

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
int should_clock_gc = 0;
int gc_invocations = 0;
int gc_timer_started = 0;
")
   ))


(define-feature c/timer/gc-start-end
  (use c/timer/globals)
  ((gc-start "
if(should_clock_gc == 1) {
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
}
")
   (gc-end "
if(should_clock_gc == 1) {
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
}
")))

;; Starts a new timer
(define-primitive (##timer-start)
  (use c/timer/globals c/timer/gc-globals)
  "{
  should_clock_gc = 1;
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
  if(gettimeofday(&time_after, 0) != 0) {
    printf(\"***error while grabbing time...\");
    exit(1);
  };

  // @@(location profiling-total-end)@@

  should_clock_gc = 0;
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
    \"%.6f seconds (%.6f seconds in GC, %d invocations, %ld total cycles, %ld drop cycles, %ld catch cycles, %ld adopt cycles, %ld rerank cycles, %ld collect cycles)\\n\",
    ((double)time_difference_ns) / 1000000,
    ((double)time_gc_accumulated) / 1000000, 
    gc_invocations,
    __profiling_total_total,
    __profiling_drop_total,
    __profiling_catch_total,
    __profiling_adopt_total,
    __profiling_rerank_total,
    __profiling_collect_total
    );
  push(TAG_NUM(0));
  break;
  }")
