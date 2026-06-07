const std = @import("std");
const zlox = @import("zlox");

pub fn main(init: std.process.Init) !void {
    var args_it = try std.process.Args.Iterator.initAllocator(init.minimal.args, init.gpa);
    defer args_it.deinit();

    _ = args_it.skip();

    const mode = args_it.next();
    if (mode != null and std.mem.eql(u8, mode.?, "--validate")) {
        const validate_path = args_it.next();
        if (validate_path == null or args_it.next() != null) {
            std.debug.print("Usage: zlox [--validate] [script]\n", .{});
            return;
        }
        try validateFile(init.io, validate_path.?);
        return;
    }

    const script_path = mode;
    if (script_path != null and args_it.next() != null) {
        std.debug.print("Usage: zlox [script]\n", .{});
        return;
    }

    if (script_path) |path| {
        try runFile(init.io, path);
        return;
    }

    const source = "1 + 2 * 3";
    std.debug.print("Evaluating: {s}\n", .{source});
    try interpret(source);
}

fn interpret(source: []const u8) !void {
    var lexer = zlox.Lexer.init(source, std.heap.page_allocator);

    var chunk = zlox.Chunk.init();
    defer chunk.deinit();

    var compiler = zlox.Compiler.init(&lexer, &chunk);
    const compiled = try compiler.compile();

    if (!compiled) {
        std.debug.print("Compilation failed\n", .{});
        return;
    }

    var vm = zlox.VM.init(&chunk, std.heap.page_allocator);
    defer vm.deinit();
    _ = try vm.run();
}

fn runFile(io: std.Io, path: [:0]const u8) !void {
    var buffer: [1024 * 1024]u8 = undefined;
    const source = try std.Io.Dir.readFile(std.Io.Dir.cwd(), io, path, &buffer);

    std.debug.print("Running file: {s}\n", .{path});
    var interpreter = try zlox.Interpreter.init(source, path, io, std.heap.page_allocator);
    defer interpreter.deinit();
    try interpreter.run();
}

fn validateFile(io: std.Io, path: [:0]const u8) !void {
    var runner = zlox.Validator.init(io, std.heap.page_allocator);
    defer runner.deinit();

    std.debug.print("Validating file: {s}\n", .{path});
    try runner.runPath(path);
}
