#ifndef __UTILS_H
#define __UTILS_H

#define halt()                                                                 \
  do {                                                                         \
    __asm__("hlt");                                                            \
  } while (0)

#endif
