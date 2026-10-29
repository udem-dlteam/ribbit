typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned char uint8_t;
typedef unsigned size_t;
/* From https://wiki.osdev.org/8259_PIC#Programming_the_PIC_chips */
#define EOF -1
#define PIC1		0x20		/* IO base address for master PIC */
#define PIC2		0xA0		/* IO base address for slave PIC */
#define PIC1_COMMAND	PIC1
#define PIC1_DATA	(PIC1+1)
#define PIC2_COMMAND	PIC2
#define PIC2_DATA	(PIC2+1)
#define ICW1_ICW4	0x01		/* Indicates that ICW4 will be present */
#define ICW1_SINGLE	0x02		/* Single (cascade) mode */
#define ICW1_INTERVAL4	0x04		/* Call address interval 4 (8) */
#define ICW1_LEVEL	0x08		/* Level triggered (edge) mode */
#define ICW1_INIT	0x10		/* Initialization - required! */
#define ICW4_8086	0x01		/* 8086/88 (MCS-80/85) mode */
#define ICW4_AUTO	0x02		/* Auto (normal) EOI */
#define ICW4_BUF_SLAVE	0x08		/* Buffered mode/slave */
#define ICW4_BUF_MASTER	0x0C		/* Buffered mode/master */
#define ICW4_SFNM	0x10		/* Special fully nested (not) */
#define CASCADE_IRQ 2


#define NULL (void *)0
extern void enable_paging(void);
extern void* irq_handlers[256];
struct stack {
  int data[20];
  int i;
};
extern struct stack irqs;
struct stack_frame {
  struct stack_frame* ebp;
  void* eip;
};

struct huge_page_addr {
  unsigned pat : 1;
  unsigned addr_end : 7;
  unsigned reserved : 1;
  unsigned addr_start : 9;
};

struct heap_chunk {
  size_t size;
  struct heap_chunk *next;
};
struct gate_descriptor {
   uint16_t offset_1;        // offset bits 0..15
   uint16_t selector;        // a code segment selector in GDT or LDT
   uint8_t  zero;            // unused, set to 0
   uint8_t gate_type:4;
   uint8_t reserved_zero:1;
   uint8_t dpl:2;
   uint8_t present:1;
   uint16_t offset_2;
} __attribute((packed));

struct idt {
    uint16_t size;
    struct gate_decriptor* offset;
} __attribute((packed));


struct page_directory_entry {
  unsigned present : 1;
  unsigned writable : 1;
  unsigned supervisor : 1;
  unsigned cache_write_mode : 1;
  unsigned no_cache : 1;
  unsigned accessed : 1;
  unsigned dirty : 1;
  unsigned is_large : 1;
  unsigned global : 1;
  unsigned free_bits : 2;
  union {
    unsigned page_table_addr : 20;
    struct huge_page_addr huge;
  };
};

struct page_table_entry {
  unsigned present : 1;
  unsigned writable : 1;
  unsigned supervisor : 1;
  unsigned cache_write_mode : 1;
  unsigned no_cache : 1;
  unsigned accessed : 1;
  unsigned dirty : 1;
  unsigned is_large : 1;
  unsigned global : 1;
  unsigned free_bits : 2;
  unsigned page_addr : 20;
};

typedef struct {
  struct page_directory_entry entries[1024];
} page_directory;

typedef struct {
  size_t (*write)(char *txt, size_t len);
  size_t (*read)(char *buf, size_t len);
} FILE;
extern FILE* stdin;
extern FILE* stdout;
extern FILE* stderr;
extern int errno;
extern char kernel_heap[];
extern struct heap_chunk *heap;

extern void set_page_directory(page_directory *page);
extern void vga_write(char c, uint8_t color, int index);
extern void exit(int code);
extern void fclose(FILE *file);
extern FILE *fopen(const char *filename, const char *mode);
extern void init();
extern int puts(const char *txt);
extern void *memcpy(char *, const char *, size_t);
extern void write(const char *txt, size_t len);
extern void perror(const char *s);
extern int fflush(FILE *stream);
extern void binary_print(int);
extern char getchar(void);
extern size_t read(char *txt, size_t size);
extern FILE *fdopen(int fd, const char *modes);
extern size_t fread(void *ptr, size_t size, size_t n, FILE *file);
extern size_t fwrite(const void *ptr, size_t size, size_t n, FILE *file);
extern char inb(unsigned short port);
extern void outw(unsigned short, unsigned short);
extern void outb(unsigned short, unsigned char);
extern void lidt(struct idt* gdt);
extern void idtr_generic();
extern int putchar(char);
extern char receive_scancode(void);
// TODO:implement these
extern void *malloc(size_t size);
extern void free(void *ptr);
extern int printf(const char *format, ...);
