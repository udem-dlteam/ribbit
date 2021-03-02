ENTRY(boot)
SECTIONS {
  . = 0x7C00;
  .text : {
    *(.boot)
    . = 0x1FE;
    SHORT(0xAA55);
  }
}
