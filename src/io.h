#ifndef __IO_H
#define __IO_H

#define __putc(c)                                                              \
  do {                                                                         \
    asm("movb %0, %%al\n"                                                      \
        "movb $0x0E, %%ah\n"                                                   \
        "int $0x10"                                                            \
        :                                                                      \
        : "g"((char)((c)))                                                     \
        : "ax", "bx");                                                         \
  } while (0)

#endif
