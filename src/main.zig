const std = @import("std");

/// Function type that returns a prototype struct.
/// Self will be the resulting statspatch struct. Methods must use this for the 
/// self-parameter instead of @This()!
pub const PrototypeFn = fn (comptime Self: type) type;

/// Create a statically-dispatched type backed by a union(enum) for all implementations.
/// Implementations must have all functions the prototype has.
pub fn StatspatchType(comptime Prototype: PrototypeFn, comptime impls: []const type) type {
    const U = Union(impls);

    return struct {
        u: U,

        const Self = @This();
        const Proto = Prototype(Self);
        pub usingnamespace Proto;

        comptime {
            for (impls) |impl| {
                for (@typeInfo(Proto).Struct.decls) |d| {
                    if (!@hasDecl(impl, d.name)) {
                        @compileError("Implementation " ++ @typeName(impl) ++ " missing decl " ++ d.name);
                    }
                }
            }
        }

        pub fn create(imp: anytype) Self {
            inline for (@typeInfo(U).Union.fields) |f| {
                if (f.type == @TypeOf(imp)) {
                    return .{ .u = @unionInit(U, f.name, imp) };
                }
            }

            @compileError("Unknown Implementation: " ++ @typeName(@TypeOf(imp)));
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
    switch (self.u) {
        inline else => |impl| {
            const func = @field(@TypeOf(impl), func_name);
            return switch (self_arg) {
                .none => @call(.always_inline, func, args),
                .self => @call(.always_inline, func, .{impl} ++ args),
                inline .ptr, .const_ptr => @call(.always_inline, func, .{&impl} ++ args),
            };
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
        .tag_type = std.meta.Int(.unsigned, std.math.log2(impls.len)),
        .fields = enum_fields,
        .decls = &.{},
        .is_exhaustive = true,
    } });

    var union_fields: []const std.builtin.Type.UnionField = &.{};

    for (impls) |impl| {
        union_fields = union_fields ++ [1]std.builtin.Type.UnionField{.{
            .name = @typeName(impl),
            .type = impl,
            .alignment = @alignOf(impl),
        }};
    }

    return @Type(.{ .Union = .{
        .layout = .Auto,
        .tag_type = Enum,
        .fields = union_fields,
        .decls = &.{},
    } });
}

test "Union" {
    const Impl1 = struct { foo: u32 };
    const Impl2 = struct { bar: u16 };
    _ = Union(&.{ Impl1, Impl2 });
    // Couldn't figure out how to test this :/
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
        &.{ Impl1, Impl2 },
    );
    _ = T.create(Impl1{ .foo = 5 });
    _ = T.create(Impl2{ .bar = 5 });
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

                pub fn favoriteNum(self: Self) u32 {
                    return implcall(self, .none, "favoriteNum", u32, .{});
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

        pub fn favoriteNum() u32 {
            return 69;
        }
    };

    const T = StatspatchType(Proto, &.{ Impl1, Impl2 });

    const a = T.create(Impl1{ .n = 10 });
    const b = T.create(Impl2{ .n = 5 });

    try std.testing.expectEqual(@as(u32, 10), a.getNum());
    try std.testing.expectEqual(@as(u32, 10), b.getNum());

    try std.testing.expectEqual(@as(u32, 20), a.mulNum(2));
    try std.testing.expectEqual(@as(u32, 20), b.mulNum(4));

    try std.testing.expectEqual(@as(u32, 42), a.favoriteNum());
    try std.testing.expectEqual(@as(u32, 69), b.favoriteNum());
}
