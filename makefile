# Sector Scheme

build: bin_files

run: bin_files
	qemu-system-i386 boot.bin

clean:
	rm *.bin *.o

bin_files: boot.bin

boot.bin: boot.o
	ld boot.o -o boot.bin -Ttext 0x7C00 --omagic -m elf_i386 --entry=boot --oformat binary

boot.o: boot.s
	as --32 -o $*.o -s $*.s

