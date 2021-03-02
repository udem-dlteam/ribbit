# Sector Scheme

build: bin_files

bin_files: boot.bin

boot.bin: boot.o
	ld $*.o -o $*.bin -Ttext 0x7C00 --omagic --entry=boot --oformat binary

boot.o: boot.s
	as --32 -o $*.o -s $*.s

