ENTRY(boot)
SECTIONS {
  . = 0x7C00;
  .first_sector : {
    *(.boot)
    *(EXCLUDE_FILE(*mem.o).text* )
    *(EXCLUDE_FILE(*mem.o).data* )
  }

  . = 0x7DFE;
  .boot_flag : {
    . = .;
    SHORT(0xAA55);
  }
  . = 0x7E00;
  second_sector : {
    . = .;
    mem.o (.text)
  }
}
