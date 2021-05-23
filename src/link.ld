ENTRY(boot)
SECTIONS {
  . = 0x7C00;
  .text : {
    *(.boot)
    *(EXCLUDE_FILE(*mem.o).text* )
    *(EXCLUDE_FILE(*mem.o).data* )
  }

  /* Bootable disk flag */
  . = 0x7DFE;
  .end : {
    . = .;
    SHORT(0xAA55);
  }
  . = 0x7E02;
  second_sector : {
    . = .;
    mem.o (.text)
  }
}
