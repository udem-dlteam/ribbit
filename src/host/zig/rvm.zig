// @@(replace ");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{" (encode 92)
const rvm_code = ");'lvD?m>lvRD?m>lvRA?m>lvRA?m>lvR:?m>lvR=!(:nlkm!':nlkv6{"; // RVM code that prints HELLO!
// )@@

const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const ObjectType = enum(i32) {
    pair = 0,
    procedure = 1,
    symbol = 2,
    string = 3,
    vector = 4,
    special_value = 5,

    fn val(self: ObjectType) i32 {
        return @intFromEnum(self);
    }

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;

        switch (self) {
            .pair => try writer.print("pair", .{}),
            .procedure => try writer.print("procedure", .{}),
            .symbol => try writer.print("symbol", .{}),
            .string => try writer.print("string", .{}),
            .vector => try writer.print("vector", .{}),
            .special_value => try writer.print("special", .{}),
        }
    }
};

const Opcode = enum(i32) {
    jump_call = 0,
    set = 1,
    get = 2,
    const_ = 3,
    if_ = 4,
    halt = 5,

    fn val(self: Opcode) i32 {
        return @intFromEnum(self);
    }
};

const nb_primitives = 20;
const exit_illegal_instr = 6;

const RibField = union(enum) {
    num: i32,
    rib: *Rib,

    fn isNum(self: RibField) bool {
        return self == .num;
    }

    fn isRib(self: RibField) bool {
        return self == .rib;
    }

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;

        switch (self) {
            .num => |n| try writer.print("{}", .{n}),
            .rib => |r| try writer.print("{}", .{r}),
        }
    }
};

const Rib = struct {
    // Prévient le undefined behavior
    car: RibField = .{ .num = 0 },
    cdr: RibField = .{ .num = 0 },
    tag: RibField = .{ .num = 0 },

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;

        switch (self.tag) {
            .rib => |r| {
                try writer.print("rib({},{},{})", .{ self.car, self.cdr, r });
            },
            .num => |n| {
                const en: ObjectType = @enumFromInt(n);
                try writer.print("{}({},{})", .{ en, self.car, self.cdr });
            },
        }
    }
};

fn num(val: i32) RibField {
    return .{ .num = val };
}

fn rib(val: *Rib) RibField {
    return .{ .rib = val };
}

fn listTail(list: *Rib, i: usize) *Rib {
    return if (i == 0) list else listTail(list.cdr.rib, i - 1);
}

const Reader = struct {
    buffer: []const u8,
    pointer: usize,

    fn init(buffer: []const u8) Reader {
        return .{
            .buffer = buffer,
            .pointer = 0,
        };
    }

    fn getByte(self: *@This()) u8 {
        const b: u8 = self.buffer[self.pointer];
        self.pointer += 1;
        return b;
    }

    fn getCode(self: *@This()) i32 {
        const x: i32 = @as(i32, self.getByte()) - 35;
        return if (x < 0) 57 else x;
    }

    fn getInt(self: *@This(), n: i32) i32 {
        const x: i32 = self.getCode();
        const n2: i32 = n * 46;
        return if (x < 46) n2 + x else self.getInt(n2 + x - 46);
    }

    fn decodeSymbolTable(self: *@This(), ribbit: *Ribbit) !void {
        // Symbols with empty names.
        var nb_empty_symbols: i32 = self.getInt(0);

        while (nb_empty_symbols > 0) : (nb_empty_symbols -= 1) {
            _ = try ribbit.addSymbol(ribbit.nil_value, 0);
        }

        // Building the symbol table.
        //
        // A semicolon means that we have reached the end of the symbol
        // table. A comma means that we have reached the end of a symbol name.

        var symbol_name: *Rib = ribbit.nil_value;
        var symbol_name_length: i32 = 0;
        // c is the current character being processed.
        var c: u8 = 0;

        while (c != ';') {
            c = self.getByte();

            if (c == ',') {
                // The end of a symbol name, let's add it in the table
                // and reset the variables.
                _ = try ribbit.addSymbol(symbol_name, symbol_name_length);
                symbol_name = ribbit.nil_value;
                symbol_name_length = 0;
            } else if (c != ';') {
                symbol_name = try ribbit.newRib(
                    num(c),
                    rib(symbol_name),
                    num(ObjectType.pair.val()),
                );
                symbol_name_length += 1;
            }
        }

        // Push the last symbol found.
        _ = try ribbit.addSymbol(symbol_name, symbol_name_length);
    }

    fn decodeInstructionGraph(self: *@This(), ribbit: *Ribbit) !void {
        const short_encodings = [_]i32{ 20, 30, 0, 10, 11, 4 };

        var m: RibField = undefined;
        var continue_: bool = true;

        while (continue_) {
            const x: i32 = self.getCode();
            var n: i32 = x;
            var op: i32 = 0;
            var d: i32 = short_encodings[@intCast(op)];

            while (d + 2 < n) : ({
                op += 1;
                d = short_encodings[@intCast(op)];
            }) {
                n -= d + 3;
            }

            if (x > 90) {
                m = ribbit.stackPop();
            } else {
                if (op == 0) {
                    ribbit.top_of_stack = rib(try ribbit.newRib(
                        num(0),
                        ribbit.top_of_stack,
                        num(0),
                    ));
                    op += 1;
                }

                if (n == d) {
                    m = num(self.getInt(0));
                } else if (n > d) {
                    m = rib(ribbit.symbolRef(
                        @intCast(self.getInt(n - d - 1)),
                    ));
                } else if (op < 3) {
                    m = rib(ribbit.symbolRef(
                        @intCast(n),
                    ));
                } else {
                    m = num(n);
                }

                if (op > 4) {
                    const rib1: *Rib = try ribbit.newRib(
                        m,
                        num(0),
                        ribbit.stackPop(),
                    );
                    const rib2: *Rib = try ribbit.newRib(
                        rib(rib1),
                        num(0),
                        num(1),
                    );

                    m = rib(rib2);

                    if (ribbit.top_of_stack.isNum() and ribbit.top_of_stack.num == 0) {
                        continue_ = false;
                    } else {
                        op = 4;
                    }
                }
            }

            if (continue_) {
                ribbit.top_of_stack.rib.car = rib(try ribbit.newRib(
                    num(op - 1),
                    m,
                    ribbit.top_of_stack.rib.car,
                ));
            }
        }

        ribbit.pc = m.rib.car.rib.tag.rib;
    }
};

/// La mémoire de l'interprète fonctionne comme suit: On a un gros bloc de
/// mémoire linéaire qui contient uniquement des rib. Lorsqu'on replit
/// complètement le bloc de mémoire, il faut faire un GC. Ça consiste à copier
/// toutes les racines dans le nouveau bloc, puis à traverser le nouveau bloc en
/// copiant dans le nouveau bloc tous les champs pas copiés des objets qu'on
/// rencontre.
const Memory = struct {
    block_a: []Rib,
    block_b: []Rib,
    alloc_a: usize = 0,
    alloc_b: usize = 0,

    fn move_field(self: *@This(), obj: RibField) RibField {
        switch (obj) {
            .rib => |r| return rib(self.move(r)),
            .num => |n| return num(n),
        }
    }

    /// On reçoit une addresse d'objet dans le bloc b, puis, selon l'état de
    /// l'objet, on fait deux choses:
    ///
    /// Si il a déjà été bougé, on retourne sa nouvelle addresse.
    ///
    /// Si il n'a pas déjà été bougé, on le copie, on le marque et on retourne
    /// sa nouvelle addresse.
    fn move(self: *@This(), obj: *Rib) *Rib {
        if (obj.car.isRib()) {
            const left = @intFromPtr(self.block_b.ptr);
            const right = left + self.block_b.len;
            const forward = obj.car.rib;

            if (left <= @intFromPtr(forward) and @intFromPtr(forward) < right) {
                // un truc déjà bougé
                return forward;
            }
        }

        if (self.alloc_b >= self.block_b.len) {
            stderr.print(
                "used: {}\n",
                .{self.alloc_b},
            ) catch unreachable;

            @panic("out of bounds. Comment ça arrive?");
        }

        const addr = &self.block_b[self.alloc_b];
        self.alloc_b += 1;
        addr.* = obj.*;

        // On écrit la nouvelle addresse dans le car de l'ancient endroit.
        obj.car = .{ .rib = addr };

        return addr;
    }

    fn gc(self: *@This()) void {
        var i: usize = 0;
        while (i < self.alloc_b) : (i += 1) {
            // stderr.print("{}/{}\n", .{ self.alloc_b, self.block_b.len }) catch unreachable;

            switch (self.block_b[i].car) {
                .num => {},
                .rib => |r| self.block_b[i].car = .{ .rib = self.move(r) },
            }
            switch (self.block_b[i].cdr) {
                .num => {},
                .rib => |r| self.block_b[i].cdr = .{ .rib = self.move(r) },
            }

            switch (self.block_b[i].tag) {
                .num => {},
                .rib => |r| self.block_b[i].tag = .{ .rib = self.move(r) },
            }
        }

        // on échange les deux blocs.
        const tmp = self.block_a;
        self.block_a = self.block_b;
        self.block_b = tmp;
        self.alloc_a = self.alloc_b;
        self.alloc_b = 0;

        // stderr.print("live ribs: {}\n", .{self.alloc_a}) catch @panic("");
    }

    /// Alloue un rib, mais sans potentiellement déclencher un gc.
    fn alloc(self: *@This()) *Rib {
        const o = &self.block_a[self.alloc_a];
        o.* = .{};
        self.alloc_a += 1;
        return o;
    }

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;
        try writer.print("memory(used = {})", .{self.used});
    }
};

test "very long linked list gc" {
    const block_size = 100000;
    var buf_a = try std.testing.allocator.alloc(Rib, block_size);
    defer std.testing.allocator.free(buf_a);
    var buf_b = try std.testing.allocator.alloc(Rib, block_size);
    defer std.testing.allocator.free(buf_b);

    var mem = Memory{
        .block_a = buf_a[0..],
        .block_b = buf_b[0..],
    };

    var i: usize = 0;

    var tail = mem.alloc();
    tail.* = .{
        .car = num(0),
        .cdr = num(0),
        .tag = num(0),
    };

    while (i < block_size - 5) : (i += 1) {
        const newTail = mem.alloc();
        newTail.* = .{
            .car = .{ .num = @intCast(i) },
            .cdr = .{ .rib = tail },
            .tag = .{ .num = ObjectType.pair.val() },
        };
        tail = newTail;
    }
    var roots = [_]**Rib{&tail};
    mem.gc(roots[0..]);
}

test "circular list" {
    const block_size = 100000;
    var buf_a = try std.testing.allocator.alloc(Rib, block_size);
    defer std.testing.allocator.free(buf_a);
    var buf_b = try std.testing.allocator.alloc(Rib, block_size);
    defer std.testing.allocator.free(buf_b);
    var mem = Memory{
        .block_a = buf_a[0..],
        .block_b = buf_b[0..],
    };
    const tail = mem.alloc();
    var head = tail;
    var i: usize = 0;
    while (i < block_size / 2) : (i += 1) {
        const newHead = mem.alloc();
        newHead.* = .{
            .car = num(0),
            .cdr = .{ .rib = head },
            .tag = num(ObjectType.pair.val()),
        };
        head = newHead;
    }

    tail.* = .{
        .car = num(0),
        .cdr = rib(head),
        .tag = num(ObjectType.pair.val()),
    };
    const before = mem.used;
    var roots = [_]**Rib{&head};
    mem.gc(roots[0..]);
    const after = mem.used;
    try std.testing.expectEqual(before, after);
}

test "gc moves the address" {
    const block_size = 100000;
    var buf_a = try std.testing.allocator.alloc(Rib, block_size);
    defer std.testing.allocator.free(buf_a);
    var buf_b = try std.testing.allocator.alloc(Rib, block_size);
    defer std.testing.allocator.free(buf_b);

    var mem = Memory{
        .block_a = buf_a[0..],
        .block_b = buf_b[0..],
    };

    var r = mem.alloc();

    var roots = [_]**Rib{&r};

    mem.gc(roots[0..]);

    try std.testing.expectEqual(&mem.block_a[0], r);
}

const Ribbit = struct {
    memory: Memory,
    allocator: Allocator,

    false_value: *Rib = undefined,
    true_value: *Rib = undefined,
    nil_value: *Rib = undefined,

    symbol_table: *Rib = undefined,
    top_of_stack: RibField = undefined,
    pc: *Rib = undefined,

    fn init(allocator: Allocator) !@This() {
        const block_size = 100_000_000;

        var o: @This() = .{
            .allocator = allocator,
            .memory = .{
                .block_a = try allocator.alloc(Rib, block_size),
                .block_b = try allocator.alloc(Rib, block_size),
            },
        };

        // Let's create the special values (#t, #f and #nil).
        o.false_value = try o.createSpecialValue();
        o.true_value = try o.createSpecialValue();
        o.nil_value = try o.createSpecialValue();

        o.symbol_table = o.nil_value;
        o.top_of_stack = num(0);
        o.pc = o.nil_value;

        return o;
    }

    fn createSpecialValue(self: *@This()) !*Rib {
        return try self.newRib(
            num(0),
            num(0),
            num(ObjectType.special_value.val()),
        );
    }

    fn setupGlobals(self: *@This()) !void {
        const primitive_zero: *Rib = try self.newRib(
            num(0),
            rib(self.symbol_table),
            num(ObjectType.procedure.val()),
        );

        const globals = [_]*Rib{
            primitive_zero,
            self.false_value,
            self.true_value,
            self.nil_value,
        };

        for (globals) |global| {
            self.symbol_table.car.rib.car = rib(global);
            self.symbol_table = self.symbol_table.cdr.rib;
        }
    }

    fn setupStack(self: *@This()) !void {
        const halt: *Rib = try self.newRib(
            num(Opcode.halt.val()),
            num(0),
            num(0),
        );

        self.top_of_stack = rib(try self.newRib(num(0), num(0), rib(halt)));
    }

    fn stackPop(self: *@This()) RibField {
        const popped: RibField = self.top_of_stack.rib.car;
        self.top_of_stack = self.top_of_stack.rib.cdr;
        return popped;
    }

    fn stackPush(self: *@This(), f: RibField) !void {
        self.top_of_stack = rib(try self.newRib(
            f,
            self.top_of_stack,
            num(ObjectType.pair.val()),
        ));
    }

    fn getOperand(self: *@This(), rf: RibField) *Rib {
        if (rf.isRib()) {
            return rf.rib;
        } else {
            return listTail(self.top_of_stack.rib, @intCast(rf.num));
        }
    }

    fn getCont(self: *@This()) !*Rib {
        var s = self.top_of_stack.rib;

        while ((s.tag.isNum() and s.tag.num == 0) and s.cdr.isRib()) {
            s = s.cdr.rib;
        }

        return s;
    }

    // @@(feature bool2scm
    fn bool2scm(self: *@This(), b: bool) *Rib {
        return if (b) self.true_value else self.false_value;
    }
    // )@@

    fn primitiveOperation(self: *@This(), op: i32) !void {
        switch (op) {
            // @@(primitives (gen index " => {\n" body)
            0 => { // @@(primitive (##rib a b c)
                const tag: RibField = self.stackPop();
                const cdr: RibField = self.stackPop();
                const car: RibField = self.stackPop();
                const rf: RibField = rib(try self.newRib(car, cdr, tag));
                try self.stackPush(rf);
            }, // )@@
            1 => { // @@(primitive (##id x)
                const val: RibField = self.stackPop();
                try self.stackPush(val);
            }, // )@@
            2 => { // @@(primitive (##arg1 x y)
                _ = self.stackPop();
            }, // )@@
            3 => { // @@(primitive (##arg2 x y)
                const val = self.stackPop();
                _ = self.stackPop();
                try self.stackPush(val);
            }, // )@@
            4 => { // @@(primitive (##close rib)
                const val = self.stackPop();

                const rf: RibField = rib(try self.newRib(
                    val.rib.car,
                    self.top_of_stack,
                    num(1),
                ));

                try self.stackPush(rf);
            }, // )@@
            5 => { // @@(primitive (##rib? rib) (use bool2scm)
                const val: RibField = self.stackPop();
                try self.stackPush(rib(self.bool2scm(val.isRib())));
            }, // )@@
            6 => { // @@(primitive (##field0 rib)
                const val: RibField = self.stackPop();
                try self.stackPush(val.rib.car);
            }, // )@@
            7 => { // @@(primitive (##field1 rib)
                const val: RibField = self.stackPop();
                try self.stackPush(val.rib.cdr);
            }, // )@@
            8 => { // @@(primitive (##field2 rib)
                const val: RibField = self.stackPop();
                try self.stackPush(val.rib.tag);
            }, // )@@
            9 => { // @@(primitive (##field0-set! rib x)
                const val1: RibField = self.stackPop();
                const val2: RibField = self.stackPop();
                val2.rib.car = val1;
                try self.stackPush(val1);
            }, // )@@
            10 => { // @@(primitive (##field1-set! rib x)
                const val1: RibField = self.stackPop();
                const val2: RibField = self.stackPop();
                val2.rib.cdr = val1;
                try self.stackPush(val1);
            }, // )@@
            11 => { // @@(primitive (##field2-set! rib x)
                const val1: RibField = self.stackPop();
                const val2: RibField = self.stackPop();
                val2.rib.tag = val1;
                try self.stackPush(val1);
            }, // )@@
            12 => { // @@(primitive (##eqv? rib1 rib2) (use bool2scm)
                const val1: RibField = self.stackPop();
                const val2: RibField = self.stackPop();

                const val = switch (val1) {
                    .num => |n| switch (val2) {
                        .num => |m| self.bool2scm(n == m),
                        else => self.bool2scm(false),
                    },
                    .rib => |f| switch (val2) {
                        .rib => |g| self.bool2scm(f == g),
                        else => self.bool2scm(false),
                    },
                };

                try self.stackPush(rib(val));
            }, // )@@
            13 => { // @@(primitive (##< x y) (use bool2scm)
                const val1: RibField = self.stackPop();
                const val2: RibField = self.stackPop();
                try self.stackPush(rib(self.bool2scm(val2.num < val1.num)));
            }, // )@@
            14 => { // @@(primitive (##+ x y)
                const val1: RibField = self.stackPop();
                const val2: RibField = self.stackPop();
                try self.stackPush(num(val2.num + val1.num));
            }, // )@@
            15 => { // @@(primitive (##- x y)
                const val1: RibField = self.stackPop();
                const val2: RibField = self.stackPop();
                try self.stackPush(num(val2.num - val1.num));
            }, // )@@
            16 => { // @@(primitive (##* x y)
                const val1: RibField = self.stackPop();
                const val2: RibField = self.stackPop();
                try self.stackPush(num(val2.num * val1.num));
            }, // )@@
            17 => { // @@(primitive (##quotient x y)
                const val1: RibField = self.stackPop();
                const val2: RibField = self.stackPop();
                if (val1.num != 0) {
                    try self.stackPush(num(@divTrunc(val2.num, val1.num)));
                } else {
                    try self.stackPush(num(0));
                }
            }, // )@@
            18 => { // @@(primitive (##getchar)
                const read: i32 = stdin.readByte() catch -1;
                try self.stackPush(num(read));
            }, // )@@
            19 => { // @@(primitive (##putchar c)
                const val: RibField = self.stackPop();
                const c: u8 = @intCast(val.num);
                try stdout.writeByte(c);
                try self.stackPush(val);
            }, // )@@
            20 => { // @@(primitive (##exit n)
                const x: i32 = self.stackPop().num;
                std.process.exit(@intCast(x));
            }, // )@@
            // )@@
            else => {
                std.process.exit(exit_illegal_instr);
            },
        }
    }

    /// Les moving pieces de la phase run sont les suivantes:
    ///
    /// 1. pc. Le pc (program counter) pointe vers un rib qui va contenir le
    /// opcode à exécuter ce cycle-ci. après chaque cycle, celui-ci sera
    /// remplacé par le rib d'après. Le "programme" est en fait une liste
    /// chaînée de opcodes.
    ///
    /// 2. stack. Je sais pas.
    fn run(self: *Ribbit) !void {
        while (true) {
            // pas garanti de fontionner si il y a ~ plus de 20 allocations dans
            // une itération.
            if (self.memory.alloc_a + 20 >= self.memory.block_a.len) {
                self.pre_gc();
            }
            var operand: RibField = self.pc.cdr;
            const instr: Opcode = switch (self.pc.car) {
                .num => |n| @enumFromInt(n),
                .rib => |r| {
                    _ = r;
                    @panic("got rib as opcode");
                },
            };

            switch (instr) {
                Opcode.halt => {
                    return;
                },
                Opcode.jump_call => {
                    operand = self.getOperand(operand).car;

                    var nargs: i32 = self.stackPop().num; // @@(feature arity-check)@@

                    switch (operand) {
                        .num => |n| {
                            stderr.print("operand: {}\n", .{n}) catch unreachable;
                            @panic("operand should be a rib, but it's a number");
                        },
                        .rib => {},
                    }

                    var c: RibField = operand.rib.car;

                    if (c.isRib()) {
                        const nparams: i32 = c.rib.car.num >> 1;

                        var c2: *Rib = try self.newRib(num(0), operand, num(0));
                        var s2: *Rib = c2;

                        // @@(feature arity-check
                        const vari: i32 = c.rib.car.num & 1;

                        if ((vari == 0 and nparams != nargs) or (vari != 0 and nparams > nargs)) {
                            try stderr.print("*** Unexpected number of arguments nargs: {} nparams: {} vari: {}\n", .{ nargs, nparams, vari });
                            return error.WrongNumberOfParams;
                        }
                        // )@@

                        // @@(feature rest-param (use arity-check)
                        nargs -= nparams;

                        if (vari != 0) {
                            var rest: *Rib = self.nil_value;

                            var i: i32 = 0;
                            while (i < nargs) : (i += 1) {
                                rest = try self.newRib(
                                    self.stackPop(),
                                    rib(rest),
                                    num(ObjectType.pair.val()),
                                );
                            }

                            s2 = try self.newRib(
                                rib(rest),
                                rib(s2),
                                num(ObjectType.pair.val()),
                            );
                        }
                        // )@@

                        var i: i32 = 0;
                        while (i < nparams) : (i += 1) {
                            s2 = try self.newRib(
                                self.stackPop(),
                                rib(s2),
                                num(ObjectType.pair.val()),
                            );
                        }

                        nparams = nparams + vari; // @@(feature arity-check)@@

                        c2 = listTail(s2, @intCast(nparams));

                        if (self.pc.tag.isRib()) {
                            c2.car = self.top_of_stack;
                            c2.tag = self.pc.tag;
                        } else {
                            const k = try self.getCont();
                            c2.car = k.car;
                            c2.tag = k.tag;
                        }

                        self.top_of_stack = rib(s2);
                    } else {
                        if (c.num <= nb_primitives) {
                            try self.primitiveOperation(c.num);
                        } else {
                            return;
                        }

                        if (self.pc.tag.isRib()) {
                            c = rib(self.pc);
                        } else {
                            c = rib(try self.getCont());
                            self.top_of_stack.rib.cdr = c.rib.car;
                        }
                    }

                    if (c.rib.tag.isRib()) {
                        self.pc = c.rib.tag.rib;
                    } else {
                        self.pc = self.nil_value;
                    }

                    continue;
                },
                Opcode.set => {
                    self.getOperand(operand).car = self.stackPop();
                },
                Opcode.get => {
                    const t = self.getOperand(operand);
                    const k = t.car;
                    try self.stackPush(k);
                },
                Opcode.const_ => {
                    try self.stackPush(operand);
                },
                Opcode.if_ => {
                    const popped: RibField = self.stackPop();

                    if (!(popped.isRib() and popped.rib == self.false_value)) {
                        self.pc = self.pc.cdr.rib;
                        continue;
                    }
                },
            }
            self.pc = self.pc.tag.rib;
        }
    }

    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;

        try writer.print(
            \\Ribbit {{
            \\  .top_of_stack = {},
            \\  .pc = {},
            \\  .symbol_table = {}
            \\}}
        , .{ self.top_of_stack, self.pc, self.symbol_table });
    }

    // alloue un rub, mais ça peut déclencher un gc
    fn newRib(self: *@This(), car: RibField, cdr: RibField, tag: RibField) !*Rib {
        var carp = car;
        var cdrp = cdr;
        var tagp = tag;

        if (self.memory.alloc_a == self.memory.block_a.len) {
            carp = self.memory.move_field(carp);
            cdrp = self.memory.move_field(cdrp);
            tagp = self.memory.move_field(tagp);

            self.pre_gc();
        }

        const r = self.memory.alloc();

        r.* = .{
            .car = carp,
            .cdr = cdrp,
            .tag = tagp,
        };

        return r;
    }

    /// Ici, on va loader toutes les racines extras dans memory.gc().
    ///
    /// pour pouvoir allouer la liste re racines sur le stack, on garantit un
    /// nombre constant (au run time) de racines avec le paramètre comptime
    /// `n_roots`
    fn pre_gc(self: *@This()) void {
        self.symbol_table = self.memory.move(self.symbol_table);
        self.top_of_stack.rib = self.memory.move(self.top_of_stack.rib);
        self.pc = self.memory.move(self.pc);
        self.true_value = self.memory.move(self.true_value);
        self.false_value = self.memory.move(self.false_value);
        self.nil_value = self.memory.move(self.nil_value);

        self.memory.gc();
    }

    fn addSymbol(self: *@This(), chars: *Rib, length: i32) !*Rib {
        const string_rib: *Rib = try self.newRib(
            .{ .rib = chars },
            .{ .num = length },
            .{ .num = ObjectType.string.val() },
        );
        const symbol_rib: *Rib = try self.newRib(
            .{ .num = 0 }, // sa valeur globale (initalisée à 0)
            .{ .rib = string_rib }, // son nom
            .{ .num = ObjectType.symbol.val() },
        );

        const pair_rib = try self.newRib(
            .{ .rib = symbol_rib },
            .{ .rib = self.symbol_table },
            .{ .num = ObjectType.pair.val() },
        );

        self.symbol_table = pair_rib;

        return symbol_rib;
    }

    fn symbolRef(self: *@This(), i: usize) *Rib {
        return listTail(self.symbol_table, i).car.rib;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var ribbit = try Ribbit.init(arena.allocator());

    var reader = Reader.init(rvm_code);
    try reader.decodeSymbolTable(&ribbit);
    try reader.decodeInstructionGraph(&ribbit);

    try ribbit.setupGlobals();
    try ribbit.setupStack();

    try ribbit.run();
}
