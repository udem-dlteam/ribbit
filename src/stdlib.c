#include "io.h"
#include "types.h"

void print_i(int num) {
  char buff[10];
  int i = 0;

  do {
    buff[i++] = (char)(num % 10);
    num = (int)(num / 10);
  } while(num != 0);

  while(i) {
    __putc('0' + buff[--i]);
  }
}
