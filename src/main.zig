const std = @import("std");

/// Function type that returns a prototype struct.
/// Self will be the resulting statspatch struct. Methods must use this for the
/// self-parameter instead of @This()!
pub const PrototypeFn = fn (comptime Self: type) type;

/// Create a statically-dispatched type backed by a union(enum) for all implementations.
/// Implementations must have all functions the prototype has.
pub fn StatspatchType(
    /// The prototype struct declaring the functions of the type.
    comptime Prototype: PrototypeFn,
    /// Some data that will be added as a field to the resulting struct,
    /// allowing for extra data storage common to all implementations.
    comptime Data: type,
    /// List of types implementing the prototype.
    comptime impls: []const type,
) type {
    const U = Union(impls);

    return struct {
        u: U,
        data: Data,

        const Self = @This();
        pub usingnamespace Prototype(Self);

        pub fn create(imp: anytype, data: Data) Self {
            inline for (@typeInfo(U).Union.fields) |f| {
                if (f.type == @TypeOf(imp)) {
                    return .{ .u = @unionInit(U, f.name, imp), .data = data };
                }
            }

            @compileError("Unknown Implementation: " ++ @typeName(@TypeOf(imp)));
        }

        pub fn DowncastResult(comptime SelfType: type, comptime T: type) type {
            return switch (SelfType) {
                Self => ?T,
                *Self => ?*T,
                *const Self => ?*const T,
                else => @compileError("Invalid self type " ++ @typeName(SelfType) ++ " for downcast!"),
            };
        }

        pub fn downcast(self: anytype, comptime T: type) DowncastResult(@TypeOf(self), T) {
            inline for (std.meta.fields(U)) |f| {
                if (f.type == T) {
                    if (std.mem.eql(u8, @tagName(self.u), f.name)) {
                        return switch (@TypeOf(self)) {
                            Self => @field(self.u, f.name),
                            *Self, *const Self => &@field(self.u, f.name),
                            else => unreachable,
                        };
                    } else {
                        return null;
                    }
                }
            }

            @compileError("Unknown Implementation: " ++ @typeName(T));
        }
    };
}

/// Call the given function in the implementation. Use this in prototype functions.
pub inline fn implcall(
    /// The statspatch type. Passed as argument to the prototype function
    self: anytype,
    /// How the self-parameter to the function should be handled.
    comptime self_arg: enum { none, self, ptr, const_ptr },
    /// Name of the function to call
    comptime func_name: []const u8,
    /// Function return type
    comptime Return: type,
    /// Argument tuple to pass excluding self argument.
    args: anytype,
) Return {
    if (@typeInfo(@TypeOf(self.u)).Union.fields.len == 0)
        unreachable;
    switch (self.u) {
        inline else => |*impl| {
            const func = @field(@TypeOf(impl.*), func_name);
            return switch (self_arg) {
                .none => @call(.auto, func, args),
                .self => @call(.auto, func, .{impl.*} ++ args),
                inline .ptr, .const_ptr => @call(.auto, func, .{impl} ++ args),
            };
        },
    }
}

/// Call the given function in the implementation or do nothing if the implementation
/// does not have the function. Use this in prototype functions.
pub inline fn implcallOptional(
    /// The statspatch type. Passed as argument to the prototype function
    self: anytype,
    /// How the self-parameter to the function should be handled.
    comptime self_arg: enum { none, self, ptr, const_ptr },
    /// Name of the function to call
    comptime func_name: []const u8,
    /// Function return type
    comptime Return: type,
    /// Argument tuple to pass excluding self argument.
    args: anytype,
) ?Return {
    if (@typeInfo(@TypeOf(self.u)).Union.fields.len == 0)
        unreachable;
    switch (self.u) {
        inline else => |*impl| {
            if (@hasDecl(@TypeOf(impl.*), func_name)) {
                const func = @field(@TypeOf(impl.*), func_name);
                return switch (self_arg) {
                    .none => @call(.auto, func, args),
                    .self => @call(.auto, func, .{impl.*} ++ args),
                    inline .ptr, .const_ptr => @call(.auto, func, .{impl} ++ args),
                };
            }
            return null;
        },
    }
}

fn Union(comptime impls: []const type) type {
    var enum_fields: []const std.builtin.Type.EnumField = &.{};
    for (impls, 0..) |impl, i| {
        enum_fields = enum_fields ++ [1]std.builtin.Type.EnumField{.{
            .name = @typeName(impl),
            .value = i,
        }};
    }

    const Enum = @Type(.{ .Enum = .{
        .tag_type = std.math.IntFittingRange(0, impls.len),
        .fields = enum_fields,
        .decls = &.{},
        .is_exhaustive = true,
    } });

    var union_fields: []const std.builtin.Type.UnionField = &.{};

    for (impls) |impl| {
        union_fields = union_fields ++ [1]std.builtin.Type.UnionField{.{
            .name = @typeName(impl),
            .type = impl,
            .alignment = 0, // infer alignment
        }};
    }

    return @Type(.{ .Union = .{
        .layout = .auto,
        .tag_type = Enum,
        .fields = union_fields,
        .decls = &.{},
    } });
}

test "Union" {
    const Impl1 = struct { foo: u32 };
    const Impl2 = struct { bar: u16 };
    const Impl3 = struct { baz: bool };
    const U = Union(&.{ Impl1, Impl2, Impl3 });
    // Couldn't figure out how to test this :/
    std.debug.print(
        "Union: \n{s}\n",
        .{comptime std.fmt.comptimePrint("{}\n\n{}", .{
            @typeInfo(@typeInfo(U).Union.tag_type.?).Enum,
            @typeInfo(U).Union,
        })},
    );
}

test "StatspatchType" {
    const Impl1 = struct { foo: u32 };
    const Impl2 = struct { bar: u16 };
    const T = StatspatchType(
        struct {
            fn Proto(comptime Self: type) type {
                _ = Self;
                return struct {};
            }
        }.Proto,
        void,
        &.{ Impl1, Impl2 },
    );
    _ = T.create(Impl1{ .foo = 5 }, {});
    _ = T.create(Impl2{ .bar = 5 }, {});
}

test "Prototype func" {
    const Proto = struct {
        fn Proto(comptime Self: type) type {
            return struct {
                pub fn getNum(self: *const Self) u32 {
                    return implcall(self, .const_ptr, "getNum", u32, .{});
                }

                pub fn mulNum(self: Self, mul: u32) u32 {
                    return implcall(self, .self, "mulNum", u32, .{mul});
                }
                pub fn favoriteNum(self: Self) ?u32 {
                    return implcallOptional(self, .none, "favoriteNum", u32, .{});
                }
            };
        }
    }.Proto;

    const Impl1 = struct {
        n: u8,

        pub fn getNum(self: *const @This()) u32 {
            return self.n;
        }

        pub fn mulNum(self: @This(), mul: u32) u32 {
            return self.getNum() * mul;
        }

        pub fn favoriteNum() u32 {
            return 42;
        }
    };

    const Impl2 = struct {
        n: u8,

        pub fn getNum(self: *const @This()) u32 {
            return self.n * 2;
        }

        pub fn mulNum(self: @This(), mul: u32) u32 {
            return self.getNum() * mul / 2;
        }
    };

    const T = StatspatchType(Proto, u32, &.{ Impl1, Impl2 });

    const a = T.create(Impl1{ .n = 10 }, 1);
    const b = T.create(Impl2{ .n = 5 }, 2);

    try std.testing.expectEqual(@as(u32, 1), a.data);
    try std.testing.expectEqual(@as(u32, 2), b.data);

    try std.testing.expectEqual(@as(u32, 10), a.getNum());
    try std.testing.expectEqual(@as(u32, 10), b.getNum());

    try std.testing.expectEqual(@as(u32, 20), a.mulNum(2));
    try std.testing.expectEqual(@as(u32, 20), b.mulNum(4));

    try std.testing.expectEqual(@as(?u32, 42), a.favoriteNum());
    try std.testing.expectEqual(@as(?u32, null), b.favoriteNum());

    try std.testing.expectEqual(Impl1{ .n = 10 }, a.downcast(Impl1).?.*);
    try std.testing.expectEqual(Impl2{ .n = 5 }, b.downcast(Impl2).?.*);

    try std.testing.expectEqual(@as(?*const Impl1, null), b.downcast(Impl1));
    try std.testing.expectEqual(@as(?*const Impl2, null), a.downcast(Impl2));
}
