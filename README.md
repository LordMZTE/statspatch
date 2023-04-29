# Statspatch — dynamic static dispatch for Zig

Statspatch is a library that can build a union(enum) dynamically to replace dynamically dispatched types.

## Why?

This library makes for an alternative for VTable types (like std.mem.Allocator). This allows for extensibility without the performance drawbacks that come with VTables (no pointer indirections).

Statspatch does this by creating a type dynamically given a prototype that defines the API and a list of all implementors.

This might seem non-extensible at first. For example, if I'm writing a GUI library using Statspatch for Widgets, how would you add your own widgets? Simple! My library can `@import("root")` to get your root module, and then you declare a list of your own widget types. When my library constructs its widget type using Statspatch, your widgets will also be added.

## Usage

1. Add statspatch to your `build.zig.zon`
2. Refer to the tests and documentation in `main.zig`
