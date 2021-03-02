#define TRUE 1
#define FALSE 0

extern void c_write(int);
extern int c_read();

int main(void) {
  c_write('H');
  return 0;
}
