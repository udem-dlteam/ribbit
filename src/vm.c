#define TRUE 1
#define FALSE 0

#include "decode.h"

extern void c_write(int);
extern int c_read();

int main(void) {
  while(TRUE) {
    c_write(c_read());
  }
  return 0;
}
