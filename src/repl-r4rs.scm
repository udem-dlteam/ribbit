(export
  ;; 6.1
  not
  boolean?

  ;; 6.2
  eqv?
  eq?
  equal?

  ;; 6.3
  pair?
  cons
  car
  cdr
  set-car!
  set-cdr!
  caar
  cadr
  cdar
  cddr
  caaar
  caadr
  cadar
  caddr
  cdaar
  cdadr
  cddar
  cdddr
  caaaar
  caaadr
  caadar
  caaddr
  cadaar
  cadadr
  caddar
  cadddr
  cdaaar
  cdaadr
  cdadar
  cdaddr
  cddaar
  cddadr
  cdddar
  cddddr
  null?
  list?
  list
  length
  append
  reverse
  list-ref
  memv
  memq
  member
  assv
  assq
  assoc

  ;; 6.4
  symbol?
  symbol->string
  string->symbol

  ;; 6.5
  number?
  complex?
  real?
  rational?
  integer?
  exact?
  inexact?
  =
  <
  >
  <=
  >=
  zero?
  positive?
  negative?
  odd?
  even?
  max
  min
  +
  *
  -
  /
  abs
  quotient
  remainder
  modulo
  gcd
  lcm
  floor
  ceiling
  truncate
  round

  number->string
  string->number

  ;; 6.6
  char?
  char=?
  char<?
  char>?
  char<=?
  char>=?
  char-ci=?
  char-ci<?
  char-ci>?
  char-ci<=?
  char-ci>=?
  char-alphabetic?
  char-numeric?
  char-whitespace?
  char-upper-case?
  char-lower-case?
  char->integer
  integer->char
  char-upcase
  char-downcase

  ;;6.7
  string?
  make-string
  string
  string-length
  string-ref
  string-set!
  string=?
  string<?
  string>?
  string<=?
  string>=?
  string-ci=?
  string-ci<?
  string-ci>?
  string-ci<=?
  string-ci>=?
  substring
  string-append
  string->list
  list->string

  ;; 6.8
  vector?
  make-vector
  vector
  vector-length
  vector-ref
  vector-set!
  vector->list
  list->vector

  ;; 6.9
  procedure?
  apply
  map
  for-each
  call/cc

  ;; 6.10
  call-with-input-file
  call-with-output-file
  input-port?
  output-port?
  current-input-port
  current-output-port
  open-input-file
  open-output-file
  close-input-port
  close-output-port
  read
  read-char
  peek-char
  eof-object?
  write
  display
  newline
  write-char
  load
  eval
  ;; error

  quote set! define if lambda quasiquote unquote unquote-splicing

  ;#; ;; support for begin special form
  begin

  ;#; ;; support for single armed let special form
  let

  ;#; ;; support for and special form
  and

  ;#; ;; support for or special form
  or

  ;#; ;; support for cond special form
  cond
  )

(repl)

;;;fancy-compiler
;;;input:(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))(fact 10)
;;;options: -l r4rs
;;;expected:
;;;
;;;              ____________________
;;;             |                    |
;;;             | Welcome to Ribbit! |
;;;             |                    |
;;;    λ        | - Rib the Frog     |
;;;  @...@  --- |____________________|
;;; (-----)
;;;( >___< )
;;;^^ ~~~ ^^
;;;
;;;> 0
;;;> 3628800
;;;> 
