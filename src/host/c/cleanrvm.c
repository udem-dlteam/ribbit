#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define MAX_MEMORY 16777216 //16 mega-words

/* Convention: MSB is 1 if field is a pointer (object), 0 if integer. 63 bit range */
#define NUMTAG 9223372036854775808UL //2^63

/* Check if a number is negative for proper arithmetic */
#define NEGTAG 4611686018427387904UL //2^62

#define ISNUM(x) (!((x) & NUMTAG))
#define GETOBJ(x) ( (rib *) (&(heap[(x) & (~NUMTAG)])) )
#define ASNUM(x) ((x) & (~NUMTAG))

#define F0(x) ((x)->fields[0])
#define F1(x) ((x)->fields[1])
#define F2(x) ((x)->fields[2])

typedef struct rib {
	uint64_t fields[3];
} rib;

void start(void); /* Start the RVM at PC */
uint64_t primitive(uint64_t n); /* Execute primitive no. n */
void pop2(void); /* Utility for primitive */
rib *lastcontinuation(void); /* Find and return last continuation rib */
rib *backstack(uint64_t n); /* Find and return rib at n places behind TOS */
uint64_t pop(void); /* Pop stack */
void push(uint64_t obj); /* Push to stack */
void decode_instructions(void); /* Generate instruction graph from encoded RVM ribbon */
int getcode(void); /* Return translated code from ribbon byte (0-91) */
int getbyte(void); /* Return next ribbon byte */
uint64_t getint(int startwithone); /* Return next int from ribbon */
void init_heap(void); /* Create heap and trigger RVM decoding & execution */
rib *alloc_rib(uint64_t first, uint64_t second, uint64_t third); /* Allocate new rib in heap */

/* Memory management stuff */
uint64_t *heap;

/* GC roots and very important variables */
rib *stack;
rib *pc;

/* These behave as if they are allocated, but are never reached */
uint64_t FALSE = -1;
uint64_t TRUE = -2;
uint64_t NIL = -3;

/* Allocate ribs */
uint64_t *allocate;
rib *alloc_rib(uint64_t first, uint64_t second, uint64_t third) {
	rib *allocated = (rib *) allocate;

	if ((uint64_t *) allocated + 3 > heap + MAX_MEMORY) {
		fprintf(stderr, "Out of memory !\n");
		exit(3);
	}


	/* Increment freelist and set values */
	*allocate++ = first;
	*allocate++ = second;
	*allocate++ = third;

	return allocated;
}

void init_heap() {
	heap = (uint64_t *) malloc(MAX_MEMORY * sizeof(uint64_t));
	allocate = heap;

	if (heap == NULL) {
		fputs("Couldn't allocate heap!\n", stderr);
		exit(1);
	}
}

/* Convert from rib -> object, except for special values */
uint64_t ASOBJ(rib *x) {
	if ((uint64_t) x == FALSE || (uint64_t) x == TRUE || (uint64_t) x == NIL) return (uint64_t) x;
	else return ( ((uint64_t) ( ((uint64_t *) (x)) - heap )) | NUMTAG );
}

/* Syntactic sugar for compiler */
// @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
char program[] = ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y";
// )@@

char *p = program;

/* ===== RVM RIBBON DECODING ===== */

void decode_instructions() {
	rib *symtbl = (rib *) NIL; /* Symbol table */

	/* First, create all symbols in the symbol table. Start with unnamed symbols */
	uint64_t qty = getint(0);

	while (qty--)
		symtbl = alloc_rib(ASOBJ(alloc_rib(FALSE, ASOBJ(alloc_rib(NIL, 0, 3)), 2)), ASOBJ(symtbl), 0);

	/* Create named strings */
	rib *str = (rib *) NIL;
	int c, len = 0;

	do {
		c = getbyte();
		if (c == ',' || c == ';') {
			symtbl =
				alloc_rib(ASOBJ(alloc_rib(FALSE, ASOBJ(alloc_rib(ASOBJ(str), len, 3)), 2)), ASOBJ(symtbl), 0);
			str = (rib *) NIL;
			len = 0;
		} else {
			len++;
			str = alloc_rib(c, ASOBJ(str), 0);
		}
	} while (c != ';'); /* Last symbol */

	/* Now decode the actual program */

	int shortencodings[6] = {20, 30, 0, 10, 11, 4};
	int offset, opcode, limit, i;
	uint64_t operand;

	/* We use pc and stack here only as convenience variables to create the instruction tree
	 * Stack is actually the symbol table (so backstack can be used to address it
	 * And pc is the program being built in the form of a stack holding the different closures */

	stack = symtbl;
	pc = (rib *) NIL; /* Field 0 = code, and field 1 is the next on the stack */

	/* There are two levels here: the instruction creator (IR) level and instruction executor (IE) level
	 * This is because decoding a ribbon requires another stack machine whose instructions are which ribs
	 * to create for the actual RVM (the IE machine) to execute */

	while (1) {
		/* First, get the IC opcode and offset */
		offset = c = getcode();
		opcode = 0;

		/* While offset is above what's allowed for this opcode, increment opcode and adjust offset */
		while (offset > (limit = shortencodings[opcode]) + 2) { /* Remember extra 3 non-short encodings */
			offset -= limit + 3;
			opcode++;
		}

		/* No danger of the IF instruction overflowing, as closure const actually has only 1 non-short encoding
		 * which acts as a buffer when c = 91 (if instruction) */

		if (c > 90) { /* IF */
			opcode = 4; /* Set IE opcode as parsing is done for this instruction */
			operand = F0(pc); /* Join two branches of the instruction graph */
			pc = GETOBJ(F1(pc)); /* Pop IC */
		} else {
			/* Jump instruction : push new frame to IC to build this procedure */
			if (opcode == 0) pc = alloc_rib(0, ASOBJ(pc), 0);

			/* Remaining instructions (except closure const) obey the same pattern */
			if (offset >= limit) { /* Are we beyond short encodings ? */
				if (offset == limit) operand = getint(0); /* First non-short is literal number */
				else operand = F0(backstack(getint(offset - limit - 1))); /* Second & third are offset to symtbl + leading digit */
			} else
				/* For short encodings, get and consts = literal num, else offset into symtbl */
				operand = (opcode < 3) ? F0(backstack(offset)) : offset;

			if (opcode > 4) { /* Closure constant */
				/* Create procedure rib pointing to nparams rib which links to pop() of the IC (containing the executable code */
				operand = ASOBJ(alloc_rib(ASOBJ(alloc_rib(operand, 0, F0(pc))), NIL, 1));

				if (F1(pc) == NIL) break; /* Finished if the branch we're closing was the last one */

				pc = GETOBJ(F1(pc)); /* Pop IC */

				opcode = 3; /* Set IE opcode as parsing is done for this instruction */
			} else if (opcode > 0) opcode--; /* Set IE opcode to IC opcode -1, which is the case for the remaining opcodes */
			else opcode = 0; /* Jump is 0 in both IE and IC, so no need */
		}

		/* Push constructed rib to the procedure (IC frame) we're currently building */
		F0(pc) = ASOBJ(alloc_rib(opcode, operand, F0(pc)));
	}

	/* When we're done, set PC to the first procedure created (entry point) */
	uint64_t temp = F0(GETOBJ(operand)); /* Get nparams rib */
	pc = GETOBJ(F2(GETOBJ(temp))); /* Set pc to start of code execution */

	/* Assign base constructs so program has access to them */
	F0(GETOBJ(F0(symtbl))) = ASOBJ(alloc_rib(0, ASOBJ(symtbl), 1)); /* Primitive RIB ; env field must link to rest */
	symtbl = GETOBJ(F1(symtbl));

	F0(GETOBJ(F0(symtbl))) = FALSE; /* Primitive FALSE */
	symtbl = GETOBJ(F1(symtbl));

	F0(GETOBJ(F0(symtbl))) = TRUE; /* Primitive TRUE */
	symtbl = GETOBJ(F1(symtbl));

	F0(GETOBJ(F0(symtbl))) = NIL; /* Primitive NIL */
}

int getbyte() {
	return *p++;
}

int getcode() {
	int byte = *p++ - 35;
	return byte < 0 ? 57 : byte;
}

uint64_t getint(int startwithone) {
	uint64_t acc = startwithone ? 1 : 0;
	uint64_t value;

	while ((value = getcode()) >= 46)
		acc = 46 * acc + value - 46;

	return acc * 46 + value;
}

/* ===== RVM EXECUTION ===== */

void push(uint64_t obj) {
	rib *r = alloc_rib(obj, ASOBJ(stack), 0);
	stack = r;
}

uint64_t pop() {
	uint64_t result;

	result = F0(stack);
	stack = GETOBJ(F1(stack)); //Downstack
	return result;
}

rib *backstack(uint64_t n) {
	/* Returns nth rib from TOS */
	rib *r = stack;

	while (n--) r = GETOBJ(F1(r));

	return r;
}

rib *lastcontinuation() {
	/* Find last continuation rib from stack and return it */
	rib *continuation = stack;

	while (F2(continuation) == 0)
		continuation = GETOBJ(F1(continuation));

	return continuation;
}

/* Primitive handling */

uint64_t x, y, z;

void pop2() {
	y = pop();
	x = pop();
}

uint64_t primitive(uint64_t n) {
	switch (n) {
		case 0:
			/* rib */
			z = pop();
			pop2();
			return ASOBJ(alloc_rib(x, y, z));
		case 1:
			/* id */
			return pop();
		case 2:
			/* arg1 */
			pop();
			return -4; /* Special code to avoid pushing anything because arg1 is special like that */
		case 3:
			/* arg2 */
			pop2();
			return y;
		case 4:
			/* close */
			x = F0(GETOBJ(pop())); /* Get code field of template rib to copy */
			return ASOBJ(alloc_rib(x, ASOBJ(stack), 1));
		case 5:
			/* rib? */
			x = pop();
			return ISNUM(x) ? FALSE : TRUE;
		case 6:
			/* field0 */
			x = F0(GETOBJ(pop()));
			return x;
		case 7:
			/* field1 */
			x = F1(GETOBJ(pop()));
			return x;
		case 8:
			/* field2 */
			x = F2(GETOBJ(pop()));
			return x;
		case 9:
			/* field0-set! */
			pop2();
			F0(GETOBJ(x)) = y;
			return y;
		case 10:
			/* field1-set! */
			pop2();
			F1(GETOBJ(x)) = y;
			return y;
		case 11:
			/* field2-set! */
			pop2();
			F2(GETOBJ(x)) = y;
			return y;
		case 12:
			/* eqv ? */
			pop2();
			return x == y ? TRUE : FALSE;
		case 13:
			/* < */
			pop2();

			/* Adjusting for signed comparison */
			if (x & NEGTAG) x |= NUMTAG;
			if (y & NEGTAG) y |= NUMTAG;

			return ((int64_t) x) < ((int64_t) y) ? TRUE : FALSE;
		case 14:
			/* + */
			pop2();
			return ASNUM(x + y);
		case 15:
			/* - */
			pop2();
			return ASNUM(x - y);
		case 16:
			/* * */
			pop2();

			/* Adjusting for signed comparison */
			if (x & NEGTAG) x |= NUMTAG;
			if (y & NEGTAG) y |= NUMTAG;

			return ASNUM(((int64_t) x) * ((int64_t) y));
		case 17:
			/* quotient */
			pop2();

			/* Adjusting for signed comparison */
			if (x & NEGTAG) x |= NUMTAG;
			if (y & NEGTAG) y |= NUMTAG;

			return ASNUM(((int64_t) x) / ((int64_t) y)); //Integer division because both uint64_t
		case 18:
			/* getchar */
			x = getchar();

			return x == EOF ? ASNUM(-1) : x;
		case 19:
			/* putchar */
			x = pop();
			fputc((int) x, stdout);
			return x;
		default:
			/* Invalid primitive call */
			exit(2);
	}
}

void start() {
	rib *instruction;
	rib *called, *continuation, *last, *tail;
	uint64_t operand, result, nparams;

	/* First continuation returns to the HALT instruction (special form with opcode 5) */
	stack = alloc_rib(0, 0, ASOBJ(alloc_rib(5, 0, 0)));

	while (1) { /* Execution loop */
		instruction = pc;
		operand = F1(instruction);

		/* Switch on opcode */
		switch (F0(instruction)) {
			case 0:
				/* Either jump or call */

				/* Either jump/call to stack offset or symbol (dereference by taking its value (0) field) */
				called = ISNUM(operand) ? GETOBJ(F0(backstack(operand))) : GETOBJ(F0(GETOBJ(operand)));

				/* Case primitive */
				if (ISNUM(F0(called))) result = primitive(F0(called));

				/* Case non-primitive */
				else {
					continuation = alloc_rib(NIL, NIL, NIL); /* Allocate new continuation rib */

					/* Pop parameters, which will serve as our new stack's base */
					nparams = F0(GETOBJ(F0(called)));
					nparams >>= 1; /* Remove variadics tag */
					tail = continuation;

					/* Build new stack base with parameters */
					while (nparams--) tail = alloc_rib(pop(), ASOBJ(tail), 0);

					F1(continuation) = ASOBJ(called); /* Reference to called procedure */

					if (F2(instruction) == 0) { /* Case jump (tail call) */
						last = lastcontinuation();

						F0(continuation) = F0(last);
						F2(continuation) = F2(last);
					} else { /* Case call */
						F0(continuation) = ASOBJ(stack); /* Return to old stack */
						F2(continuation) = F2(instruction); /* Return to next instruction */
					}

					stack = tail; /* Set to new stack frame */
					pc = GETOBJ(F2(GETOBJ(F0(called)))); /* Get nparams rib then get entry point (its F2) and jump there */

					continue; /* Avoid automatic increment. Everything is set, so loop back to normal execution */
				}

				if (F2(instruction) == 0) { /* Jump after primitive (tail call) */
					last = lastcontinuation();

					/* Return to caller continuation and pass control */
					stack = GETOBJ(F0(last));
					pc = GETOBJ(F2(last));

					/* Push result to old stack frame and avoid automatic increment */
					push(result);
					continue;
				}

				if (result != -4) push(result); /* Call to primitive, push back result except for the special case */
				break;
			case 1:
				/* set */
				if (ISNUM(operand)) { /* Careful with undefined behavior */
					F0(backstack(operand)) = F0(stack);
					pop();
				} else {
					F0(GETOBJ(operand)) = F0(stack); /* Set symbol's value field, F0 */
					pop();
				}
				break;
			case 2:
				/* get */
				if (ISNUM(operand)) push(F0(backstack(operand)));
				else push(F0(GETOBJ(operand)));
				break;
			case 3:
				/* const */
				push(operand);
				break;
			case 4:
				/* if */
				if (pop() != FALSE) {
					pc = GETOBJ(operand);
					continue; /* Avoid auto-increment */
				} /* Else let auto-increment pass to fallback value */
				break;
			default:
				/* halt */
				exit(0);
		}

		pc = GETOBJ(F2(instruction)); /* Auto-increment to next instruction */
	}
}

void main() {
	init_heap(); /* Allocate memory for the RVM */
	decode_instructions(); /* Create program graph from ribbon */
	start(); /* Execute instructions */
}
