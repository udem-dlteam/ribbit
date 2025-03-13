
(if-feature c/gc/es

  ;; ref count or even-shiloach gc
  (begin

    (define-primitive
      (##stdin-fd)
      (use c/stdio)
      "{
      FILE* file = fdopen(0, \"r\");
      push((long) file | 1);
      break;
    }")

    (define-primitive
      (##stdout-fd)
      (use c/stdio)
      "{
      FILE* file = fdopen(1, \"w\");
      push((long) file | 1);
      break;
    }")

    (define-primitive
      (##get-fd-input-file filename)
      (use c/stdio scm2str)
      "{
  PRIM1();
  char* filename = scm2str(x);
  FILE* file = fopen(filename, \"r\");
  push(file ? (long) file | 1 : FALSE);
  free((void*) filename);
  DEC_PRIM1();
  break;
}")

    (define-primitive
      (##get-fd-output-file filename)
      (use c/stdio scm2str)
      "{
  PRIM1();
  char* filename = scm2str(x);
  FILE* file = fopen(filename, \"w\");
  push((long) file | 1);
  free((void *) filename);
  DEC_PRIM1();
  break;
}")

    (define-primitive
      (##read-char-fd fd)
      (use c/stdio)
      "{
  PRIM1();
  FILE* file = (FILE*) ((long) x ^ 1);
  char buffer[1];
  int bytes_read = fread(buffer, 1, 1, file);
  if (!bytes_read) push(NIL);
  else push(TAG_NUM(buffer[0]));
  DEC_PRIM1();
  break;
}")

    (define-primitive
      (##write-char-fd ch fd)
      (use c/stdio)
      "{
  PRIM2();
  FILE* file = (FILE*) ((long) y ^ 1);
  char buffer[1] = {(char) NUM(x)};
  int success = fwrite(buffer, 1, 1, file);
  if (success != 1) {
  perror(\"Cannot write to file.\");
  }
  fflush(file);
  push(TRUE);
  DEC_PRIM2();
  break;
}")

    (define-primitive
      (##close-input-fd fd)
      (use c/stdio)
      "{
  PRIM1();
  FILE* file = (FILE*) ((long) x ^ 1);
  fclose(file);
  DEC_PRIM1();
  break;
}"))

  ;; mark-and-sweep or stop-and-copy gc
  (begin

    (define-primitive
      (##stdin-fd)
      (use c/stdio)
      "{
      FILE* file = fdopen(0, \"r\");
      push2((long) file | 1, PAIR_TAG);
      break;
    }")

    (define-primitive
      (##stdout-fd)
      (use c/stdio)
      "{
      FILE* file = fdopen(1, \"w\");
      push2((long) file | 1, PAIR_TAG);
      break;
    }")

    (define-primitive
      (##get-fd-input-file filename)
      (use c/stdio scm2str)
      "{
  PRIM1();
  char* filename = scm2str(x);
  FILE* file = fopen(filename, \"r\");
  push2(file ? (long) file | 1 : FALSE, PAIR_TAG);
  free((void*) filename);
  break;
}")

    (define-primitive
      (##get-fd-output-file filename)
      (use c/stdio scm2str)
      "{
  PRIM1();
  char* filename = scm2str(x);
  FILE* file = fopen(filename, \"w\");
  push2((long) file | 1, PAIR_TAG);
  free((void *) filename);
  break;
}")

    (define-primitive
      (##read-char-fd fd)
      (use c/stdio)
      "{
  PRIM1();
  FILE* file = (FILE*) ((long) x ^ 1);
  char buffer[1];
  int bytes_read = fread(buffer, 1, 1, file);
  if (!bytes_read) push2(NIL, PAIR_TAG);
  else push2(TAG_NUM(buffer[0]), PAIR_TAG);
  break;
}")

    (define-primitive
      (##write-char-fd ch fd)
      (use c/stdio)
      "{
  PRIM2();
  FILE* file = (FILE*) ((long) y ^ 1);
  char buffer[1] = {(char) NUM(x)};
  int success = fwrite(buffer, 1, 1, file);
  if (success != 1) {
  perror(\"Cannot write to file.\");
  }
  fflush(file);
  push2(TRUE, PAIR_TAG);
  break;
}")

    (define-primitive
      (##close-input-fd fd)
      (use c/stdio)
      "{
  PRIM1();
  FILE* file = (FILE*) ((long) x ^ 1);
  fclose(file);
  break;
}")))

(define (##close-output-fd port) (##close-input-fd port))
