//! Runtime value types for the Lox VM
const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;

pub const MAX_PARAMS = 16;
pub const MAX_UPVALUES = 256;

pub const Value = union(enum) {
    number: f64,
    boolean: bool,
    string: []const u8,
    obj: *Obj,
    nil,

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .number => try writer.print("{d}", .{self.number}),
            .boolean => try writer.print("{}", .{self.boolean}),
            .string => try writer.print("\"{s}\"", .{self.string}),
            .obj => |obj| try obj.format(writer),
            .nil => try writer.writeAll("nil"),
        }
    }

    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .nil => false,
            .boolean => |b| b,
            .number, .string, .obj => true,
        };
    }

    pub fn isEqual(self: Value, other: Value) bool {
        const left_tag = std.meta.activeTag(self);
        const right_tag = std.meta.activeTag(other);
        if (left_tag != right_tag) return false;

        return switch (left_tag) {
            .number => self.number == other.number,
            .boolean => self.boolean == other.boolean,
            .string => std.mem.eql(u8, self.string, other.string),
            .obj => self.obj == other.obj,
            .nil => true,
        };
    }
};

pub const ObjType = enum {
    function,
    closure,
    upvalue,
    class,
    instance,
    bound_method,
    native,
    module,
};

pub const Obj = struct {
    obj_type: ObjType,
    next: ?*Obj,

    pub fn format(self: *Obj, writer: anytype) !void {
        switch (self.obj_type) {
            .function => {
                const func: *ObjFunction = @fieldParentPtr("obj", self);
                try writer.print("<fn {s}>", .{func.name});
            },
            .closure => {
                const closure: *ObjClosure = @fieldParentPtr("obj", self);
                try writer.print("<closure {s}>", .{closure.func.name});
            },
            .upvalue => try writer.writeAll("<upvalue>"),
            .class => {
                const class_def: *ObjClass = @fieldParentPtr("obj", self);
                try writer.print("<class {s}>", .{class_def.name});
            },
            .instance => {
                const instance: *ObjInstance = @fieldParentPtr("obj", self);
                try writer.print("<instance {s}>", .{instance.class_def.name});
            },
            .bound_method => {
                const bound: *ObjBoundMethod = @fieldParentPtr("obj", self);
                try writer.print("<method {s}>", .{bound.method.func.name});
            },
            .native => try writer.writeAll("<native fn>"),
            .module => try writer.writeAll("<module>"),
        }
    }
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: u8,
    upvalue_count: u8,
    num_registers: u8,
    chunk: Chunk,
    name: []const u8,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) ObjFunction {
        return .{
            .obj = .{ .obj_type = .function, .next = null },
            .arity = 0,
            .upvalue_count = 0,
            .num_registers = 1,
            .chunk = Chunk.init(allocator),
            .name = name,
        };
    }

    pub fn deinit(self: *ObjFunction) void {
        self.chunk.deinit();
    }
};

pub const ObjClosure = struct {
    obj: Obj,
    func: *ObjFunction,
    upvalues: []?*ObjUpvalue,

    pub fn init(allocator: std.mem.Allocator, func: *ObjFunction) !*ObjClosure {
        const closure = try allocator.create(ObjClosure);
        closure.obj = .{ .obj_type = .closure, .next = null };
        closure.func = func;

        const upvalues = try allocator.alloc(?*ObjUpvalue, func.upvalue_count);
        @memset(upvalues, null);
        closure.upvalues = upvalues;

        return closure;
    }

    pub fn deinit(self: *ObjClosure, allocator: std.mem.Allocator) void {
        allocator.free(self.upvalues);
    }
};

pub const ObjUpvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value = .nil,
    next: ?*ObjUpvalue = null,

    pub fn init(allocator: std.mem.Allocator, slot: *Value) !*ObjUpvalue {
        const upvalue = try allocator.create(ObjUpvalue);
        upvalue.obj = .{ .obj_type = .upvalue, .next = null };
        upvalue.location = slot;
        return upvalue;
    }
};

pub const ObjClass = struct {
    obj: Obj,
    name: []const u8,
    methods: std.StringHashMap(Value),
    superclass: ?*ObjClass,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, superclass: ?*ObjClass) ObjClass {
        return .{
            .obj = .{ .obj_type = .class, .next = null },
            .name = name,
            .methods = std.StringHashMap(Value).init(allocator),
            .superclass = superclass,
        };
    }

    pub fn findMethod(self: *ObjClass, name: []const u8) ?Value {
        if (self.methods.get(name)) |method| return method;
        if (self.superclass) |sc| return sc.findMethod(name);
        return null;
    }

    pub fn deinit(self: *ObjClass) void {
        self.methods.deinit();
    }
};

pub const ObjInstance = struct {
    obj: Obj,
    class_def: *ObjClass,
    fields: std.StringHashMap(Value),

    pub fn init(allocator: std.mem.Allocator, class_def: *ObjClass) ObjInstance {
        return .{
            .obj = .{ .obj_type = .instance, .next = null },
            .class_def = class_def,
            .fields = std.StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *ObjInstance) void {
        self.fields.deinit();
    }
};

pub const ObjBoundMethod = struct {
    obj: Obj,
    receiver: Value,
    method: *ObjClosure,
};

pub const ObjNative = struct {
    obj: Obj,
    function: NativeFn,
};

pub const NativeFn = *const fn (*VM, []const Value) Value;

pub const ObjModule = struct {
    obj: Obj,
    name: []const u8,
    exports: std.StringHashMap(Value),

    pub fn init(allocator: std.mem.Allocator, name: []const u8) ObjModule {
        return .{
            .obj = .{ .obj_type = .module, .next = null },
            .name = name,
            .exports = std.StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *ObjModule) void {
        self.exports.deinit();
    }
};

// Forward declare for native functions
const VM = @import("vm.zig").VM;
