#include "kernel.h"
static char scanned = 0;

void write(const char *s, size_t len) {
  for (int i = 0; i < len; i++)
    putchar(s[i]);
}

void *memcpy(char *dst, const char *src, size_t n) {
  for (int i = 0; i < n; i++) {
    dst[i] = src[i];
  }
  return dst;
}

int puts(const char *s) {
  while (*s != 0) {
    putchar(*s);
    s++;
  }
  putchar('\n');
  return 1;
}

FILE *fopen(const char *file, const char *mode) {
  errno = 1;
  puts("fopen not implemented");
  return 0;
}
/*int printf(const char *txt, ...) {
  puts("printf is not implemented");
  errno = 1;
  return -1;
}*/

void fclose(FILE *stream) {
  free(stream);
  errno = 0;
  return;
}
int twice = 0;
static size_t fdwrite(char *t, size_t n) {
  if (scanned) {
    putchar('\n');
    scanned = 0;
  }
  write(t, n);
  errno = 0;
  return n;
}

FILE *fdopen(int fd, const char *modes) {
  if (fd == 0 || fd == 1 || fd == 2) {
    return malloc(0x10);
  }
  printf("fdopen not possible on fd %d\n",fd);
  errno = 1;
  return NULL;
}

int fflush(FILE *ptr) {
  errno = 0;
  return 0;
}

void perror(const char *s) {
  puts(s);
  // puts(errors[errno]);
}
size_t fread(void *ptr, size_t size, size_t n, FILE *f) {
  errno = 0;
  return read((char *)ptr, size * n);
}
size_t fwrite(const void *ptr, size_t size, size_t n, FILE *f) {
  fdwrite((char *)ptr, size * n);
  errno = 0;
  return n * size;
}
