# Makefile to Compile the C VM as the smallest possible VM
# Also supports `make compiled` to take `source.scm` and 
# receive an executable `compiled`

UNAME = $(shell uname)
OUT=rVM
INT=vm.o.unstripped
VM_SRC=rvm.c
SRC=$(VM_SRC) makefile

ifeq ($(UNAME), Darwin)
	LD=/usr/local/Cellar/x86_64-elf-binutils/2.36.1/bin/x86_64-elf-ld
	GCC=/usr/local/Cellar/x86_64-elf-gcc/11.1.0/bin/x86_64-elf-gcc
	AS=/usr/local/Cellar/x86_64-elf-binutils/2.36.1/bin/x86_64-elf-as
	OBJC=/usr/local/Cellar/x86_64-elf-binutils/2.36.1/bin/x86_64-elf-objcopy
	NASM=nasm
else
	LD=ld
	GCC=gcc
	AS=as
	OBJC=objcopy
	NASM=nasm
endif

GCC_DEFS = -DNOSTART -DNO_STD
CFLAGS = -m32 -ffunction-sections -fdata-sections -fno-plt -fno-stack-protector \
		 -fno-stack-check -fno-unwind-tables -flto -fno-stack-protector \
		 -fno-stack-check \
		 -Wl,--build-id=none -z norelro -Wl,-z,noseparate-code \
		 -Wl,--no-eh-frame-hdr -no-pie -Wl,--no-ld-generated-unwind-info \
		 -Wl,--hash-style=sysv -Wl,-z,nodynamic-undefined-weak \
		 -Wl,--gc-sections -fno-exceptions \
		 -fno-asynchronous-unwind-tables -fomit-frame-pointer -fno-pic -fno-PIE -fno-PIC -ffast-math \
		 -nostartfiles -nostdlib -s

OBJCARGS = -S --remove-section .comment --remove-section .note --remove-section .hash --remove-section .eh_frame --remove-section .note.gnu.property

COPT_SPACE = -Os
COPT_FAST3 = -O3 -DNO_REG
COPT_FAST2 = -O2 -DNO_REG
COPT_FAST1 = -O1 -DNO_REG

all: $(OUT) $(OUT)3

clean:
	rm -f *.o
	rm -f *.o3
	rm -f *.csv
	rm -f *.scm.c
	rm -f $(OUT)*
	rm -f $(INT)*
	rm -f $(COMPILED_INT)
	rm -f $(COMPILED_OUT)

$(OUT): $(INT)
	$(OBJC) $(OBJCARGS) $(INT) $(OUT)

$(OUT)3: $(INT)3
	$(OBJC) $(OBJCARGS) $(INT)3 $(OUT)3

$(INT): $(SRC) 
	$(GCC) $(GCC_DEFS) $(VM_SRC) $(CFLAGS) $(COPT_SPACE) -o $(INT)

$(INT)3: $(SRC)
	$(GCC) $(GCC_DEFS) $(VM_SRC) $(CFLAGS) $(COPT_FAST3) -o $(INT)3

%.o.scm: %.scm.c
	$(GCC) $(GCC_DEFS) $^ $(CFLAGS) $(COPT_SPACE) -DCPROG -o $*.o.scm

%.o3.scm: %.scm.c
	$(GCC) $(GCC_DEFS) $^ $(CFLAGS) $(COPT_FAST3) -DCPROG -o $*.o3.scm

%.o: %.o.scm
	$(OBJC) $(OBJCARGS) $^ $*.o

%.o3: %.o3.scm
	$(OBJC) $(OBJCARGS) $^ $*.o3
