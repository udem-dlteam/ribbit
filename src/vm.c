#define TRUE 1
#define FALSE 0

#include "test.h"
#include "decode.h"
#include "io.h"
#include "stdlib.h"

extern int c_read();

extern void gc_test();

int main(void) {

  gc_test();

  return 0;
}
