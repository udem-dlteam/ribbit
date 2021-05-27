#define TRUE 1
#define FALSE 0

#include "decode.h"
#include "io.h"

extern int c_read();

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

int main(void) {
  __putc('>');
  while(TRUE) {
    __putc(c_read());
  }
  return 0;
}
