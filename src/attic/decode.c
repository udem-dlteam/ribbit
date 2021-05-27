#include "types.h"

#define TIMES_FOUR(n) ((n) << 2)
#define MAX_OF_MULTIPLE(m) ((1 << (TIMES_FOUR(m))) - 1)

void decode(byte* buff, size_t len) {
  // every op code is encoded in a muliple of 4 bits. If the 
  // maximum value is reached with the current multiple,
  // assume we need a bigger multiple. ie, if for multiple = 1
  // and value = 0xF, then multiple is >= 2.
  // For the following array:
  // byte b[] = {
  //        0xAA,
  //        0xFA,
  //        0xFF,
  //        0xBA
  // };
  // the decoding give out:
  // A,A,AF,BFF,A
  // with all these numbers being in hex, of course.
  size_t bt_c = 0;    // byte cursor
  byte half_c = 1;   // half byte tracker

  while(bt_c < len) {
    u32 decoded = 0;
    byte multiple = 0;

    do {
      byte shift_factor = half_c << 2;
      byte t = buff[bt_c] >> shift_factor; // x >> (n << 2)
      byte half_byte = t & 0xF;
      byte mshift = multiple << 2;
      decoded |= half_byte << mshift;
      multiple++;
      if ((--half_c)) {
        bt_c++;
        half_c = 1;
      }
    } while (decoded == MAX_OF_MULTIPLE(multiple));

    // dispatch(decoded)
  }
}
