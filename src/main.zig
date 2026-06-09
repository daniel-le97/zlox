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

    const source = "print 1 + 2 * 3;";
    std.debug.print("Evaluating: {s}\n", .{source});
    try interpret(source);
}

fn interpret(source: []const u8) !void {
    var vm = zlox.VM.init(std.heap.page_allocator);
    defer vm.deinit();
    _ = try vm.interpret(source);
}

fn runFile(io: std.Io, path: [:0]const u8) !void {
    var buffer: [1024 * 1024]u8 = undefined;
    const source = try std.Io.Dir.readFile(std.Io.Dir.cwd(), io, path, &buffer);

    std.debug.print("Running file: {s}\n", .{path});

    var vm = zlox.VM.init(std.heap.page_allocator);
    defer vm.deinit();

    // Pre-load imports: scan for "import" statements and load them
    try preloadImports(&vm, io, source, path);

    _ = try vm.interpret(source);
}

fn preloadImports(vm: *zlox.VM, io: std.Io, source: []const u8, parent_path: []const u8) !void {
    var i: usize = 0;
    while (i + 6 < source.len) : (i += 1) {
        if (std.mem.eql(u8, source[i..][0..6], "import")) {
            var j = i + 6;
            while (j < source.len and (source[j] == ' ' or source[j] == '\t')) : (j += 1) {}
            if (j < source.len and source[j] == '"') {
                j += 1;
                const path_start = j;
                while (j < source.len and source[j] != '"') : (j += 1) {}
                const import_path = source[path_start..j];
                const module_name = moduleNameFromPath(import_path);

                if (vm.globals.contains(module_name)) continue;

                var resolved_buf: [1024]u8 = undefined;
                const resolved_path = try resolveImportPath(parent_path, import_path, &resolved_buf);

                // Try importing relative to parent first, then fall back to CWD
                var import_buffer: [1024 * 1024]u8 = undefined;
                const import_source = std.Io.Dir.readFile(std.Io.Dir.cwd(), io, resolved_path, &import_buffer) catch blk: {
                    // Fallback: try resolving relative to CWD
                    const cwd_resolved = try resolveImportPath(".", import_path, &resolved_buf);
                    break :blk std.Io.Dir.readFile(std.Io.Dir.cwd(), io, cwd_resolved, &import_buffer) catch continue;
                };

                try preloadImports(vm, io, import_source, resolved_path);

                var sub_vm = zlox.VM.init(std.heap.page_allocator);
                defer sub_vm.deinit();
                if (sub_vm.interpret(import_source)) |_| {} else |_| continue;

                // Copy sub-VM globals into an ObjModule
                var mod = zlox.value.ObjModule.init(std.heap.page_allocator, module_name);
                var it = sub_vm.globals.iterator();
                while (it.next()) |entry| {
                    if (!std.mem.eql(u8, entry.key_ptr.*, module_name)) {
                        try mod.exports.put(entry.key_ptr.*, entry.value_ptr.*);
                    }
                }
                const mod_ptr = try std.heap.page_allocator.create(zlox.value.ObjModule);
                mod_ptr.* = mod;
                mod_ptr.obj = .{ .obj_type = .module, .next = null };
                try vm.globals.put(module_name, .{ .obj = &mod_ptr.obj });
            }
        }
    }
}

fn moduleNameFromPath(path: []const u8) []const u8 {
    var start: usize = 0;
    var end: usize = path.len;
    if (std.mem.lastIndexOfScalar(u8, path, '/')) |idx| start = idx + 1;
    if (std.mem.lastIndexOfScalar(u8, path, '\\')) |idx| start = @max(start, idx + 1);
    var dot_idx: ?usize = null;
    var k: usize = start;
    while (k < end) : (k += 1) {
        if (path[k] == '.') dot_idx = k;
    }
    if (dot_idx) |idx| end = idx;
    return path[start..end];
}

fn resolveImportPath(parent_path: []const u8, import_path: []const u8, buf: []u8) ![]const u8 {
    // If import_path is absolute, return as-is
    if (import_path.len > 0 and (import_path[0] == '/' or import_path[0] == '\\')) {
        return import_path;
    }
    // Get parent directory (everything up to the last separator)
    var parent_dir_end: usize = 0;
    if (std.mem.lastIndexOfScalar(u8, parent_path, '/')) |idx| parent_dir_end = idx;
    if (std.mem.lastIndexOfScalar(u8, parent_path, '\\')) |idx| parent_dir_end = @max(parent_dir_end, idx);
    const parent_dir = if (parent_dir_end > 0) parent_path[0..parent_dir_end] else ".";
    // Combine: parent_dir/import_path
    const resolved = try std.fmt.bufPrint(buf, "{s}/{s}", .{ parent_dir, import_path });
    return resolved;
}

fn validateFile(io: std.Io, path: [:0]const u8) !void {
    var runner = zlox.Validator.init(io, std.heap.page_allocator);
    defer runner.deinit();

    std.debug.print("Validating file: {s}\n", .{path});
    try runner.runPath(path);
}
