;;; File: prim-timer.scm
;; Implement the timer primitives. Timer primitives are used
;; with the time library (see src/lib/time.scm)

;; Add c global definitions to keep track of time

(define-feature c/gc/profile-time
  (use c/include-stdint.h c/include-stdio.h c/include-stdlib.h c/include-sys/time.h)
(
(import "
double __profile_total_time = 0.;
double __profile_gc_time = 0.;
int __profile_gc_invocations = 0;
struct timeval __profile_temp_timeval;

int64_t __profile_gc_cycles = 0;
int64_t __profile_drop_cycles = 0;
int64_t __profile_catch_cycles = 0;
int64_t __profile_collect_cycles = 0;
int64_t __profile_remove_edge_cycles = 0;
int64_t __profile_adopt_in_drop_cycles = 0;
int64_t __profile_adopt_in_remove_edge_cycles = 0;
int64_t __profile_adopt_in_unprotect_cycles = 0;
int64_t __profile_total_cycles = 0;
")
(profile-time-start
"
__profile_gc_time = 0.;
__profile_total_time = 0.;
__profile_gc_invocations = 0;
if(gettimeofday(&__profile_temp_timeval, 0) != 0){
  exit(1);
}
__profile_total_time -= (__profile_temp_timeval.tv_sec + __profile_temp_timeval.tv_usec / 1e6);
")
(profile-time-stop "
if(gettimeofday(&__profile_temp_timeval, 0) != 0){
  exit(1);
}
__profile_total_time += (__profile_temp_timeval.tv_sec + __profile_temp_timeval.tv_usec / 1e6);
")
(profile-gc-start "
__profile_gc_invocations++;

if(gettimeofday(&__profile_temp_timeval, 0) != 0){
  exit(1);
}
__profile_gc_time -= (__profile_temp_timeval.tv_sec + __profile_temp_timeval.tv_usec / 1e6);
")
(profile-gc-stop "
if(gettimeofday(&__profile_temp_timeval, 0) != 0){
  exit(1);
}
__profile_gc_time += (__profile_temp_timeval.tv_sec + __profile_temp_timeval.tv_usec / 1e6);
")))

(define-feature c/gc/profile-cycles
  (use c/gc/profile-time)
(
(import "
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
)
(profile-cycles-start "
__profile_gc_cycles = 0;
__profile_drop_cycles = 0;
__profile_catch_cycles = 0;
__profile_collect_cycles = 0;
__profile_remove_edge_cycles = 0;
__profile_adopt_in_drop_cycles = 0;
__profile_adopt_in_remove_edge_cycles = 0;
__profile_adopt_in_unprotect_cycles = 0;
__profile_total_cycles = -GET_CYCLECOUNT();
")
(profile-cycles-stop "__profile_total_cycles += GET_CYCLECOUNT();")

(profile-start-remove-edge "__profile_remove_edge_cycles -= GET_CYCLECOUNT();")
(profile-stop-remove-edge "__profile_remove_edge_cycles += GET_CYCLECOUNT();")
(profile-start-drop "__profile_drop_cycles -= GET_CYCLECOUNT();")
(profile-start-drop-in-remove-edge "__profile_drop_cycles -= tmp_catch_cycles_drop_in_remove_edge_cycles;")
(profile-stop-drop "
int64_t tmp_drop_cycles = GET_CYCLECOUNT();
__profile_drop_cycles += tmp_drop_cycles;
")
(profile-start-catch "
__profile_catch_cycles -= tmp_drop_cycles;
")
(profile-stop-catch "
int64_t tmp_catch_cycles = GET_CYCLECOUNT();
__profile_catch_cycles += tmp_catch_cycles;
")
(profile-start-collect "
__profile_collect_cycles -= tmp_catch_cycles;
")
(profile-stop-collect "__profile_collect_cycles += GET_CYCLECOUNT();")
(profile-start-adopt-in-drop "__profile_adopt_in_drop_cycles -= GET_CYCLECOUNT();")
(profile-stop-adopt-in-drop "__profile_adopt_in_drop_cycles += GET_CYCLECOUNT();")
(profile-start-adopt-in-remove-edge "
int64_t tmp_catch_cycles_drop_in_remove_edge_cycles;
__profile_adopt_in_remove_edge_cycles -= GET_CYCLECOUNT();
")
(profile-stop-adopt-in-remove-edge "
tmp_catch_cycles_drop_in_remove_edge_cycles = GET_CYCLECOUNT();
__profile_adopt_in_remove_edge_cycles += tmp_catch_cycles_drop_in_remove_edge_cycles;
")
(profile-start-adopt-in-unprotect "__profile_adopt_in_unprotect_cycles -= GET_CYCLECOUNT();")
(profile-stop-adopt-in-unprotect "__profile_adopt_in_unprotect_cycles += GET_CYCLECOUNT();")
(profile-start-gc "__profile_gc_cycles -= GET_CYCLECOUNT();")
(profile-stop-gc "__profile_gc_cycles += GET_CYCLECOUNT();")))

;;; TIME LIBRARY
;;;

(define-feature c/include-sys/time.h
  ((import "#include <sys/time.h>\n")))

(define-feature c/include-stdint.h
  ((import "#include <stdint.h>\n")))

;; Starts a new timer
(define-primitive (##timer-start)
  (use c/gc/profile-time)
  "{
  @@(location profile-time-start)@@
  @@(location profile-cycles-start)@@
  push(TAG_NUM(0));
  break;
  }")

;; Ends the timer
(define-primitive (##timer-end)
  (use c/gc/profile-time)
  "{
  @@(location profile-time-stop)@@
  @@(location profile-cycles-stop)@@
  push(TAG_NUM(0));
  break;
}")
  
(define-primitive (##timer-display)
  (use c/gc/profile-time)
  "{
printf(
  \"%.6f seconds (%.6f seconds in GC, %d invocations, %ld total cycles, %ld gc cycles (%.3f), %ld drop cycles (%.3f), %ld catch cycles (%.3f), %ld collect cycles (%.3f), %ld adopt-in-drop cycles (%.3f), %ld adopt-in-remove-edge cycles (%.3f), %ld adopt-in-unprotect cycles (%.3f))\\n\",
  ((double)__profile_total_time),
  ((double)__profile_gc_time), 
  __profile_gc_invocations,
  __profile_total_cycles,
  __profile_gc_cycles,
  ((float)__profile_gc_cycles /(float)__profile_total_cycles),
  __profile_drop_cycles,
  ((float)__profile_drop_cycles/(float)__profile_total_cycles),
  __profile_catch_cycles,
  ((float)__profile_catch_cycles/(float)__profile_total_cycles),
  __profile_collect_cycles,
  ((float)__profile_collect_cycles/(float)__profile_total_cycles),
  __profile_adopt_in_drop_cycles,
  ((float)__profile_adopt_in_drop_cycles/(float)__profile_total_cycles),
  __profile_remove_edge_cycles,
  ((float)__profile_remove_edge_cycles/(float)__profile_total_cycles),
  __profile_adopt_in_unprotect_cycles,
  ((float)__profile_adopt_in_unprotect_cycles/(float)__profile_total_cycles)
  );
push(TAG_NUM(0));
break;
}")
