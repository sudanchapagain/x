source: <https://gist.github.com/ityonemo/769532c2017ed9143f3571e5ac104e50>

res:
    - https://ziglang.org/learn/overview
    - https://ziglang.org/documentation/master
    - https://matklad.github.io/2025/03/19/comptime-zig-orm.html
    - https://zig.guide
    - https://cookbook.ziglang.cc
    - https://ziglings.org
    - https://github.com/zigcc/awesome-zig/wiki
    - https://www.openmymind.net/learning_zig
    - https://pedropark99.github.io/zig-book
    - https://www.manning.com/books/systems-programming-with-zig
    - https://github.com/zigcc/awesome-zig

# A half-hour to learn Zig

This is inspired by https://fasterthanli.me/blog/2020/a-half-hour-to-learn-rust/

## Basics

the command `zig run my_code.zig` will compile and immediately run your Zig
program.  Each of these cells contains a zig program that you can try to run 
(some of them contain compile-time errors that you can comment out to play 
with)

You'll want to declare a main() function to get started running code.

This program does almost nothing:

```zig
// comments look like this and go to the end of the line
pub fn main() void {}
```

You can import from the standard library by using the `@import` builtin and
assigning the namespace to a const value.  Almost everything in zig must be
explicitly assigned its identifier.  You can also import other zig files this
way, and C files in a similar fashion with `@cImport`.

```zig
const std = @import("std");

pub fn main() void {
    std.debug.print("hello world!\n", .{});
}
```

Note:
- I'll explain the funny second parameter in the print statement, later in the structs section.

`var` declares a variable, in most cases you should declare the variable type.

```zig
const std = @import("std");

pub fn main() void {
    var x: i32 = 47; // declares "x" of type i32 to be 47.
    std.debug.print("x: {}\n", .{x});
}
```

`const` declares that a variable's value is immutable.

```zig
pub fn main() void {
    const x: i32 = 47;
    x = 42; // error: cannot assign to constant
}
```

Zig is very picky and will NOT let you shadow identifiers from an
outside scope, to keep you from being confused:

```zig
const x: i32 = 47;

pub fn main() void {
    var x: i32 = 42;  // error: redefinition of 'x'
}
```

Constants in the global scope are by default compile-time "comptime" values, 
and if you omit the type they are comptime typed and can turn into 
runtime types for your runtime values.

```zig
const x: i32 = 47;
const y = -47;  // comptime integer.

pub fn main() void {
    var a: i32 = y; // comptime constant coerced into correct type
    var b: i64 = y; // comptime constant coerced into correct type
    var c: u32 = y; // error: cannot cast negative value -47 to unsigned integer
}
```

You can explicitly choose to leave it undefined if it will get set later.  Zig will
fill in a dummy value with 0XAA bytes to help detect errors in debug mode if you cause an
error from accidentally using it in debug.

```zig
const std = @import("std");

pub fn main() void {
  var x: i32 = undefined;
  std.debug.print("undefined: {}\n", .{x});
}
```

In some cases, zig will let you omit the type information if it can figure
it out.

```zig
const std = @import("std");

pub fn main() void {
    var x: i32 = 47;
    var y: i32 = 47;
    var z = x + y; // declares z and sets it to 94.
    std.debug.print("z: {}\n", .{z});
}
```

But be careful, integer literals are comptime-typed, so this won't work:

```zig
pub fn main() void {
    var x = 47; // error: variable of type 'comptime_int' must be const or comptime
}
```

## Functions

Here's a function (`foo`) that returns nothing.  The `pub` keyword means that the
function is exportable from the current scope, which is why main must be `pub`.  You 
call functions just as you would in most programming languages:

```zig
const std = @import("std");

fn foo() void {
    std.debug.print("foo!\n", .{});

    //optional:
    return;
}

pub fn main() void {
    foo();
}
```

Here's a function that returns an integer value:

```zig
const std = @import("std");

fn foo() i32 {
    return 47;
}

pub fn main() void {
    var result = foo();
    std.debug.print("foo: {}\n", .{result});
}
```

Zig won't let you ignore return values for functions:

```zig
fn foo() i32 {
    return 47;
}

pub fn main() void {
    foo(); // error: expression value is ignored
}
```

but you can if you assign it to the throw-away `_`.

```zig
fn foo() i32 {
    return 47;
}

pub fn main() void {
  _ = foo();
}
```

You can make a function that can take a parameter by declaring its type:

```zig
const std = @import("std");

fn foo(x: i32) void {
    std.debug.print("foo param: {}\n", .{x});
}

pub fn main() void {
    foo(47);
}
```

## Structs

structs are declared by assigning them a name using the const keyword,
they can be assigned out of order, and they can be
used by dereferencing with the usual dot syntax.

```zig
const std = @import("std");

const Vec2 = struct{
    x: f64,
    y: f64
};

pub fn main() void {
    var v = Vec2{.y = 1.0, .x = 2.0};
    std.debug.print("v: {}\n", .{v});
}
```

structs can have default values; structs can also be anonymous, and
can coerce into another struct so long as all of the values can be 
figured out:


```zig
const std = @import("std");

const Vec3 = struct{
    x: f64 = 0.0,
    y: f64,
    z: f64
};

pub fn main() void {
    var v: Vec3 = .{.y = 0.1, .z = 0.2};  // ok
    var w: Vec3 = .{.y = 0.1}; // error: missing field: 'z'
    std.debug.print("v: {}\n", .{v});
}
```

You can drop functions into an struct to make it work like a OOP-style object.
There is syntactic sugar where if you make the functions' first parameter be
a pointer to the object, it can be called "Object-style", similar to how Python
has the self-parametered member functions.  The typical convention is to make this
obvious by calling the variable self.

```zig
const std = @import("std");

const LikeAnObject = struct{
    value: i32,

    fn print(self: *LikeAnObject) void {
        std.debug.print("value: {}\n", .{self.value});
    }
};

pub fn main() void {
    var obj = LikeAnObject{.value = 47};
    obj.print();
}
```

By the way, that thing we've been passing into the std.debug.print's second parameter
is a tuple.  Without going into too much detail, it's an *anonymous struct with number fields*.  
At *compile time*, `std.debug.print` figures out types of the parameters in that tuple and 
generates a version of itself tuned for the parameters string that you provided, and that's 
how zig knows how to make the contents of the print pretty.

```zig
const std = @import("std");

pub fn main() void {
    std.debug.print("{}\n", .{1, 2}); #  error: Unused arguments
}
```

## Enums

Enums are declared by assigning the group of enums as a type using the
const keyword.

Note:
- In some cases you can shortcut the name of the Enum.
- You can set the value of an Enum to an integer, but it does not automatically
  coerce, you have to use `@enumToInt` or `@intToEnum` to do conversions.

```zig
const std = @import("std");

const EnumType = enum{
    EnumOne,
    EnumTwo,
    EnumThree = 3
};

pub fn main() void {
    std.debug.print("One: {}\n", .{EnumType.EnumOne});
    std.debug.print("Two?: {}\n", .{EnumType.EnumTwo == .EnumTwo});
    std.debug.print("Three?: {}\n", .{@enumToInt(EnumType.EnumThree) == 3});
}
```

## Arrays and Slices

zig has Arrays, which are contiguous memory with compile-time known length.
You can initialize them by declaring the type up front and providing the list 
of values.  You can access the length with the `len` field of the array.

Note:
- Arrays in zig are zero-indexed.

```zig
const std = @import("std");

pub fn main() void {
    var array: [3]u32 = [3]u32{47, 47, 47};

    // also valid:
    // var array = [_]u32{47, 47, 47};

    var invalid = array[4]; // error: index 4 outside array of size 3.
    std.debug.print("array[0]: {}\n", .{array[0]});
    std.debug.print("length: {}\n", .{array.len});
}
```

zig also has slices, which are have run-time known length.  You can construct
slices from arrays or other slices using the slicing operation.  Similarly to 
arrays, slices have a len field which tells you its length.

Note:

- The interval parameter in the slicing operation is open (non-inclusive) on the 
big end.

Attempting to access beyond the range of the slice is a runtime panic (this means
your program will crash).

```zig
const std = @import("std");

pub fn main() void {
    var array: [3]u32 = [_]u32{47, 47, 47};
    var slice: []u32 = array[0..2];

    // also valid:
    // var slice = array[0..2];

    var invalid = slice[3]; // panic: index out of bounds

    std.debug.print("slice[0]: {}\n", .{slice[0]});
    std.debug.print("length: {}\n", .{slice.len});
}
```

string literals are null-terminated utf-8 encoded arrays of `const u8` bytes.  
Unicode characters are only allowed in string literals and comments.

Note:
- length does not include the null termination (officially called 
  "sentinel termination")
- it's safe to access the null terminator.
- indices are by byte, not by unicode glyph.

```zig
const std = @import("std");
const string = "hello 世界";
const world = "world";

pub fn main() void {
    var slice: []const u8 = string[0..5];

    std.debug.print("string {}\n", .{string});
    std.debug.print("length {}\n", .{world.len});
    std.debug.print("null {}\n", .{world[5]});
    std.debug.print("slice {}\n", .{slice});
    std.debug.print("huh? {}\n", .{string[0..7]});
}
```

const arrays can be coerced into const slices.

```zig
const std = @import("std");

fn foo() []const u8 {  // note function returns a slice
    return "foo";      // but this is a const array.
}

pub fn main() void {
    std.debug.print("foo: {}\n", .{foo()});
}
```

## Control structures

Zig gives you an if statement that works as you would expect.

```zig
const std = @import("std");

fn foo(v: i32) []const u8 {
    if (v < 0) {
        return "negative";
    }
    else {
        return "non-negative";
    }
}

pub fn main() void {
    std.debug.print("positive {}\n", .{foo(47)});
    std.debug.print("negative {}\n", .{foo(-47)});
}
```

as well as a switch statement

```zig
const std = @import("std");

fn foo(v: i32) []const u8 {
    switch (v) {
        0 => return "zero",
        else => return "nonzero"
    }
}

pub fn main() void {
    std.debug.print("47 {}\n", .{foo(47)});
    std.debug.print("0 {}\n", .{foo(0)});
}
```

Zig provides a for-loop that works only on arrays and slices.

```zig
const std = @import("std");

pub fn main() void {
    var array = [_]i32{47, 48, 49};

    for (array) | value | {
        std.debug.print("array {}\n", .{value});
    }
    for (array) | value, index | {
        std.debug.print("array {}:{}\n", .{index, value});
    }

    var slice = array[0..2];

    for (slice) | value | {
        std.debug.print("slice {}\n", .{value});
    }
    for (slice) | value, index | {
        std.debug.print("slice {}:{}\n", .{index, value});
    }
}
```

Zig provides a while-loop that also works as you might expect:

```zig
const std = @import("std");

pub fn main() void {
    var array = [_]i32{47, 48, 49};
    var index: u32 = 0;

    while (index < 2) {
        std.debug.print("value: {}\n", .{array[index]});
        index += 1;
    }
}
```

## Error handling

Errors are special union types, you denote that a function can error by
prepending `!` to the front.  You throw the error by simply returning it as
if it were a normal return.

```zig
const MyError = error{
    GenericError,  // just a list of identifiers, like an enum.
    OtherError
};

pub fn main() !void {
    return MyError.GenericError;
}
```

If you write a function that can error, you must decide what to do with it when
it returns.  Two common options are `try` which is very lazy, and simply forwards
the error to be the error for the function.  `catch` explicitly handles the error.

- `try` is just sugar for `catch | err | {return err}`

```zig
const std = @import("std");
const MyError = error{
    GenericError
};

fn foo(v: i32) !i32 {
    if (v == 42) return MyError.GenericError;
    return v;
}

pub fn main() !void {
    // catch traps and handles errors bubbling up
    _ = foo(42) catch |err| {
        std.debug.print("error: {}\n", .{err});
    };

    // try won't get activated here.
    std.debug.print("foo: {}\n", .{try foo(47)});

    // this will ultimately cause main to print an error trace and return nonzero
    _ = try foo(42);
}
```

You can also use if to check for errors.

```zig
const std = @import("std");
const MyError = error{
    GenericError
};

fn foo(v: i32) !i32 {
    if (v == 42) return MyError.GenericError;
    return v;
}

// note that it is safe for wrap_foo to not have an error ! because
// we handle ALL cases and don't return errors.
fn wrap_foo(v: i32) void {    
    if (foo(v)) | value | {
        std.debug.print("value: {}\n", .{value});
    } else | err | {
        std.debug.print("error: {}\n", .{err});
    }
}

pub fn main() void {
    wrap_foo(42);
    wrap_foo(47);
}
```

## Pointers

Pointer types are declared by prepending `*` to the front of the type.  No spiral declarations
like C!  They are dereferenced, with the `.*` field:

```zig
const std = @import("std");

pub fn printer(value: *i32) void {
    std.debug.print("pointer: {}\n", .{value});
    std.debug.print("value: {}\n", .{value.*});
}

pub fn main() void {
    var value: i32 = 47;
    printer(&value);
}
```

Note: 
- in Zig, pointers need to be aligned correctly with the alignment of the
  value it's pointing to.

For structs, similar to Java, you can dereference the pointer and get the field
in one shot with the `.` operator.  Note this only works with one level of
indirection, so if you have a pointer to a pointer, you must dereference the
outer pointer first.

```zig
const std = @import("std");

const MyStruct = struct {
    value: i32
};

pub fn printer(s: *MyStruct) void {
    std.debug.print("value: {}\n", .{s.value});
}

pub fn main() void {
    var value = MyStruct{.value = 47};
    printer(&value);
}
```

Zig allows any type (not just pointers) to be nullable, but note that they are unions of the base type
and the special value `null`.  To access the unwrapped optional type, use the `.?` field:

```zig
const std = @import("std");

pub fn main() void {
    var value: i32 = 47;
    var vptr: ?*i32 = &value;
    var throwaway1: ?*i32 = null;
    var throwaway2: *i32 = null; // error: expected type '*i32', found '(null)'

    std.debug.print("value: {}\n", .{vptr.*}); // error: attempt to dereference non-pointer type
    std.debug.print("value: {}\n", .{vptr.?.*});
}
```
Note:
- when you use pointers from C ABI functions they are automatically converted to nullable pointers.

Another way of obtaining the unwrapped optional pointer is with the `if` statement:

```zig
const std = @import("std");

fn nullChoice(value: ?*i32) void {
    if (value) | v | {
        std.debug.print("value: {}\n", .{v.*});
    } else {
        std.debug.print("null!\n", .{});
    }
}

pub fn main() void {
    var value: i32 = 47;
    var vptr1: ?*i32 = &value;
    var vptr2: ?*i32 = null;

    nullChoice(vptr1);
    nullChoice(vptr2);
}
```

## A taste of metaprogramming

Zig's metaprogramming is driven by a few basic concepts:
- Types are valid values at compile-time
- most runtime code will also work at compile-time.
- struct field evaluation is compile-time duck-typed.
- the zig standard library gives you tools to perform compile-time
  reflection.

Here's an example of multiple dispatch (though you have already seen this in action with 
`std.debug.print`, perhaps now you can imagine how it's implemented:

```zig
const std = @import("std");

fn foo(x : anytype) @TypeOf(x) {
    // note that this if statement happens at compile-time, not runtime.
    if (@TypeOf(x) == i64) {
        return x + 2;
    } else {
        return 2 * x;
    }
}

pub fn main() void {
    var x: i64 = 47;
    var y: i32 =  47;

    std.debug.print("i64-foo: {}\n", .{foo(x)});
    std.debug.print("i32-foo: {}\n", .{foo(y)});
}
```

Here's an example of generic types:

```zig
const std = @import("std");

fn Vec2Of(comptime T: type) type {
    return struct{
        x: T,
        y: T
    };
}

const V2i64 = Vec2Of(i64);
const V2f64 = Vec2Of(f64);

pub fn main() void {
    var vi = V2i64{.x = 47, .y = 47};
    var vf = V2f64{.x = 47.0, .y = 47.0};
    
    std.debug.print("i64 vector: {}\n", .{vi});
    std.debug.print("f64 vector: {}\n", .{vf});
}
```

From these concepts you can build very powerful generics!

## The HEAP

Zig gives you many ways to interact with the heap, and usually requires you to be
explicit about your choices.  They all follow the same pattern:

1. Create an Allocator factory struct.
2. Retrieve the `std.mem.Allocator` struct creacted by the Allocator factory.
3. Use the alloc/free and create/destroy functions to manipulate the heap.
4. (optional) deinit the Allocator factory.

Whew!  That sounds like a lot.  But
- this is to discourage you from using the heap.
- it makes anything which calls the heap (which are fundamentally failable) obvious.
- by being unopinionated, you can carefully tune your tradeoffs and use standard datastructures
  *without having to rewrite the standard library*.
- you can run an extremely safe allocator in your tests and swap it out for a different
  allocator in release/prod.

Ok.  But **you can still be lazy**.  Do you miss just using jemalloc everywhere?  
Just pick a global allocator and use that everywhere (being aware that some allocators are 
threadsafe and some allocators are not)!  Please don't do this if you are writing
a general purpose library.

In this example we'll use the std.heap.GeneralPurposeAllocator factory to create an allocator
with a bunch of bells and whistles (including leak detection) and see how this comes together.

One last thing, this uses the `defer` keyword, which is a lot like go's defer keyword!  There's also
an `errdefer` keyword, but to learn more about that check the Zig docs (linked below).

```zig
const std = @import("std");

// factory type
const Gpa = std.heap.GeneralPurposeAllocator(.{});

pub fn main() !void {
    // instantiates the factory
    var gpa = Gpa{};
    
    // retrieves the created allocator.
    var galloc = &gpa.allocator;
    
    // scopes the lifetime of the allocator to this function and
    // performs cleanup; 
    defer _ = gpa.deinit();

    var slice = try galloc.alloc(i32, 2);
    // uncomment to remove memory leak warning
    // defer galloc.free(slice);
    
    var single = try galloc.create(i32);
    // defer gallo.destroy(single);

    slice[0] = 47;
    slice[1] = 48;
    single.* = 49;

    std.debug.print("slice: [{}, {}]\n", .{slice[0], slice[1]});
    std.debug.print("single: {}\n", .{single.*});
}
```

## Coda

That's it!  Now you know a fairly decent chunk of zig.  Some (pretty important) things I didn't cover include:

- tests!  Dear god please write tests.  Zig makes it easy to do it.
- the standard library
  - the memory model (somewhat uniquely, zig is aggressively unopinionated about allocators)
- async
- cross-compilation
- build.zig

For more details, check the latest documentation:
https://ziglang.org/documentation/master/

or for a less half-baked tutorial, go to:
https://ziglearn.org/
