//! Runtime value types for the Lox interpreter
const std = @import("std");

pub const MAX_PARAMS = 16;

pub const Value = union(enum) {
    number: f64,
    boolean: bool,
    string: []const u8,
    function_template: *FunctionTemplate,
    closure: *FunctionInstance,
    class_def: *ClassDefinition,
    instance: *Instance,
    module: *Environment,
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
            .function_template => try writer.print("<fn {s}>", .{self.function_template.name}),
            .closure => try writer.print("<closure {s}>", .{self.closure.template.name}),
            .class_def => try writer.print("<class {s}>", .{self.class_def.name}),
            .instance => try writer.print("<instance {s}>", .{self.instance.class.name}),
            .module => try writer.print("<module>", .{}),
            .nil => try writer.writeAll("nil"),
        }
    }

    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .nil => false,
            .boolean => |b| b,
            .number, .string => true,
            .function_template, .closure, .class_def, .instance, .module => true,
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
            .function_template => self.function_template == other.function_template,
            .closure => self.closure == other.closure,
            .class_def => self.class_def == other.class_def,
            .instance => self.instance == other.instance,
            .module => self.module == other.module,
            .nil => true,
        };
    }
};

pub const Environment = struct {
    allocator: std.mem.Allocator,
    parent: ?*Environment,
    values: std.StringHashMap(Value),

    pub fn init(allocator: std.mem.Allocator, parent: ?*Environment) Environment {
        return .{
            .allocator = allocator,
            .parent = parent,
            .values = std.StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit();
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.values.put(name, value);
    }

    pub fn get(self: *Environment, name: []const u8) ?Value {
        if (self.values.get(name)) |value| {
            return value;
        }

        if (self.parent) |parent| {
            return parent.get(name);
        }

        return null;
    }

    pub fn assign(self: *Environment, name: []const u8, value: Value) !void {
        if (self.values.contains(name)) {
            try self.values.put(name, value);
            return;
        }

        if (self.parent) |parent| {
            try parent.assign(name, value);
            return;
        }

        return error.UndefinedVariable;
    }
};

pub const FunctionTemplate = struct {
    name: []const u8,
    arity: u8 = 0,
    param_names: [MAX_PARAMS][]const u8 = undefined,
    param_count: u8 = 0,
    body_source: []const u8 = "",
};

pub const FunctionInstance = struct {
    template: *FunctionTemplate,
    closure: *Environment,
};

pub const ClassDefinition = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    superclass: ?*ClassDefinition,
    methods: std.StringHashMap(*FunctionInstance),

    pub fn init(allocator: std.mem.Allocator, name: []const u8, superclass: ?*ClassDefinition) ClassDefinition {
        return .{
            .allocator = allocator,
            .name = name,
            .superclass = superclass,
            .methods = std.StringHashMap(*FunctionInstance).init(allocator),
        };
    }

    pub fn deinit(self: *ClassDefinition) void {
        self.methods.deinit();
    }

    pub fn defineMethod(self: *ClassDefinition, name: []const u8, method: *FunctionInstance) !void {
        try self.methods.put(name, method);
    }

    pub fn findMethod(self: *ClassDefinition, name: []const u8) ?*FunctionInstance {
        if (self.methods.get(name)) |method| {
            return method;
        }

        if (self.superclass) |superclass| {
            return superclass.findMethod(name);
        }

        return null;
    }
};

pub const Instance = struct {
    allocator: std.mem.Allocator,
    class: *ClassDefinition,
    fields: std.StringHashMap(Value),

    pub fn init(allocator: std.mem.Allocator, class: *ClassDefinition) Instance {
        return .{
            .allocator = allocator,
            .class = class,
            .fields = std.StringHashMap(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Instance) void {
        self.fields.deinit();
    }

    pub fn getField(self: *Instance, name: []const u8) ?Value {
        return self.fields.get(name);
    }

    pub fn setField(self: *Instance, name: []const u8, value: Value) !void {
        try self.fields.put(name, value);
    }
};
