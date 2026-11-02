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


(define-feature c/include-sys/time.h
  ((import "#include <sys/time.h>\n")))

(define-feature c/include-stdint.h
  ((import "#include <stdint.h>\n")))

;; Starts a new timer
(define-primitive (%%timer-start)
  (use c/timer/globals c/timer/gc-globals)
  "{
  @@(location profile-time-start)@@
  push(TAG_NUM(0));
  break;
  }")

;; Ends the timer
(define-primitive (%%timer-end)
  (use c/time/globals c/time/gc-globals)
  "{
  @@(location profile-time-stop)@@
  push(TAG_NUM(0));
  break;
}")

(define-primitive (%%timer-display)
  (use c/time/globals c/time/gc-globals c/time/profiling-decl)
  "{
printf(
  \"%.6f seconds total (%.6f seconds in GC, %d invocations)\\n\",
  __profile_total_time,
  __profile_gc_time,
  __profile_gc_invocations);
push(TAG_NUM(0));
break;
}")
