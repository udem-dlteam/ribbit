ENTRY(boot)
SECTIONS {
  . = 0x7C00;
  .first_sector : {
    *(.boot)
    *(EXCLUDE_FILE(*vm.o *test.o).text* )
    *(EXCLUDE_FILE(*vm.o *test.o).data* )
  }

  . = 0x7DFE;
  .boot_flag : {
    . = .;
    SHORT(0xAA55);
  }
  . = 0x7E00;
  second_sector : {
    . = .;
    vm.o (.text)
    vm.o (.data)
    mem_test.o (.text)
    mem_test.o (.data)
  }
}
