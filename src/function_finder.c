#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#define TOTAL_SIZE (512 * 2)

uint8_t *load() {
  FILE* fp = fopen("boot.bin", "rb");

  if(!fp) {
    printf("Boot.bin file not found\n");
    exit(-1);
  }

  uint8_t *ram = malloc(sizeof(uint8_t) * TOTAL_SIZE);
  size_t total = fread(ram, sizeof(uint8_t), TOTAL_SIZE, fp);

  printf("Read %ld bytes of the archive.\n", total);

  fclose(fp);

  return ram;
}

void find_all_rets(uint8_t* ram) {
  uint8_t *r = ram;
  size_t left = TOTAL_SIZE;

  size_t iter = 0;
  int retc = 0;
  while(left--) {
    uint8_t at_cur = *r++;
    uint8_t next = *r;

    if(0xC3 == at_cur) {
      printf("Ret # %d: ret at address %lx\n", retc + 1, 0x7C00 + r - ram -1);
      retc++;
    }

    ++iter;
  }

  printf("%ld bytes analyzed, %d ret found\n", r - ram, retc);
}

int main(int argc, char **argv) {
  uint8_t *ram = load();
  find_all_rets(ram);
  free(ram);
}
