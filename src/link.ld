ENTRY(boot)
SECTIONS {
  . = 0x7C00;
  .text : {
    *(.boot)
    *(.text)
    *(.data)
  }

  /* Bootable disk flag */
  . = 0x7DFE;
  .end : {
    . = .;
    SHORT(0xAA55);
  }
}
