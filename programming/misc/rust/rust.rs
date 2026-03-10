/* =============================================================================
//                        THIS FILE CANNOT BE COMPILED.
// =============================================================================



ABOUT
=====

Modern systems programming language focused on performance, safety, and 
concurrency. The benefit rust provides is thread safety and memory safety due
to the programs being required to be written in explicit and intentional ways.


RESOURCES
=========

- The Rust Programming Language (TRPL): <https://doc.rust-lang.org/book/>
- CLI: <https://rust-cli.github.io/book/index.html>
- Cargo: <https://doc.rust-lang.org/cargo/index.html>
- Rust by example: <https://doc.rust-lang.org/rust-by-example/>
- GTK4-rs: <https://gtk-rs.org/gtk4-rs/stable/latest/book>
- <https://www.arewewebyet.org/>

- Unsafe rust: <https://doc.rust-lang.org/nomicon/index.html>
- Rust Reference (a more comprehensive rust book): <https://doc.rust-lang.org/reference/index.html>
- Awesome Rust: <https://github.com/rust-unofficial/awesome-rust>

- Little book of Rust Macros: <https://veykril.github.io/tlborm/>
- Rust compiler book: <https://doc.rust-lang.org/rustc/index.html>
- Rust for polyglot programmers: <https://www.chiark.greenend.org.uk/~ianmdlvl/rust-polyglot/index.html>

MISC
----

- <https://github.com/ImplFerris/LearnRust>
- <https://github.com/pretzelhammer/rust-blog>
- <https://github.com/skyzh/type-exercise-in-rust>
- <https://github.com/ctjhoa/rust-learning>




BASIC RUST CONCEPTS
===================

1. Ownership: Each value has a single owner. When the owner goes out of scope,
   the value is dropped (memory is freed).

2. Borrowing: Allows temporary access to a value without transferring
   ownership. **Immutable Borrowing (&T)**: Multiple immutable references are
   allowed. Cannot modify the value. **Mutable Borrowing (&mut T)**: Only one
   mutable reference is allowed at a time. Can modify the value.

3. References: References are a way to access data without taking ownership.
   Must adhere to borrowing rules (no mutable and immutable references
   simultaneously).

4. Lifetimes: Lifetimes specify how long a reference is valid. Help prevent
   dangling references and ensure memory safety.

5. Structs and Enums: Structs are custom data types that group related
   values. Enums are type that can be one of several variants, useful for
   modeling choices.

6. Pattern Matching: Powerful control flow mechanism, commonly used with match
   statements. Can destructure values and execute code based on their structure.

7. Traits: Define shared behavior in Rust, similar to interfaces in other
   languages. Can be implemented for types to define specific functionality.

8. Error Handling: Rust uses `Result<T, E>` for recoverable errors and
   `Option<T>` for optional values. Promotes explicit error handling and avoids
   exceptions.

9. Concurrency: Rust’s ownership model ensures safe concurrent programming.
   Data races are prevented at compile time through borrowing rules.

10. Modules and Crates: Modules: Organize code into namespaces. Crates:
    Packages of Rust code, can be libraries or executables.



DATA TYPES
==========

**Basics (Scaler type: represents a single value): Integer, Floating point,
Char, Booleans**

INTEGER

| length | signed | unsigned |
| ------ | ------ | -------- |
| 8-bit  | i8     | u8       |
| 16-bit | i16    | u16      |
| 32-bit | i32    | u32      |
| 64-bit | i64    | u64      |
| 128-bit| i128   | u128     |
| arch   | isize  | usize    |

signed variant can store numbers from `-(2n - 1) to 2n - 1 - 1` inclusive

FORMATTING

| Number literals | Example     |
| --------------- | ----------- |
| Decimal         | 98_222      |
| Hex             | 0xff        |
| Octal           | 0o77        |
| Binary          | 0b1111_0000 |
| Byte (u8 only)  | b'A'        |

FLOATING POINT: floating-point types are f32 and f64, which are 32 bits and 64
                bits in size.
BOOLEAN: `true` & `false`

CHAR: Unicode Scalar Value, four bytes in size.
*/

fn main() {
    let c = 'z';
    let z: char = 'ℤ'; // with explicit type annotation
    let heart_eyed_cat = '😻';

    // rust doesnt make a variable and store the data * in it
    let x = 5;
    // it's doing pattern matching as its a expression
    let x = { x = 5 };
}

/* we specify char literals with single quotes, as opposed to string literals,
which use double quotes. Rust’s char type is four bytes in size and represents
a Unicode Scalar Value, which means it can represent a lot more than just ASCII.



COMPOUND TYPES: grouped multiple values into one type. Rust has two primitive
compound types: tuples and arrays.



THE TUPLE TYPE: A tuple is a general way of grouping together a number of values
with a variety of types into one compound type. Tuples have a fixed length:
once declared, they cannot grow or shrink in size. We create a tuple by writing
a comma-separated list of values inside parentheses. Each position in the
tuple has a type, and the types of the different values in the tuple don’t
have to be the same. We’ve added optional type annotations in this example:*/

fn main() {
    let tup: (i32, f64, u8) = (500, 6.4, 1);
    let pair = ('a', 17);
    println!("{}", pair.0); // a
    println!("{}", pair.1); // 17
}

/* The variable tup binds to the entire tuple because a tuple is considered a
single compound element. To get the individual values out of a tuple, we
can use pattern matching to destructure a tuple value, like this: */

fn main() {
    let tup = (500, 6.4, 1);
    let (x, y, z) = tup;
    println!("The value of y is: {y}");
}

/* Pattern with let to take tup and turn it into three separate variables,
`x`, `y`, and `z`. This is called destructuring.

Tuple element can directly be accessed by using a period (.) followed
by the index of the value we want to access. */

fn main() {
    let x: (i32, f64, u8) = (500, 6.4, 1);
    let five_hundred = x.0;
    let six_point_four = x.1;
    let one = x.2;

    let (some_char, some_int) = ('a', 1);
    assert!(some_char, 'a');
    assert!(some_int, 1);
    let (l, r) = slice.split_at(middle);
    let (_, right) = slice.split_at(middle);
}

/* tuples with no values has a special name *unit*. This value and its
corresponding type are both written `()` and represent an empty value
or an empty return type. see this: */

fn do_something() {
    println!("Doing something...");
}

fn main() {
    let result = do_something(); // result is of type ()
    println!("{:?}", result);    // This will print: ()
}

/* ARRAY TYPE: Unlike a tuple, every element of an array must have the same
type. Arrays in rust have a fixed length. Example: */

fn main() {
    let a = [1, 2, 3, 4, 5];
}

/* An array isn’t as flexible as the vector type, though. A vector is a
similar collection type provided by the standard library that is
allowed to grow or shrink in size. If you're unsure which one to use,
use vector.

You write an array’s type using square brackets with the type of each
element, a semicolon, and then the number of elements in the array, like so:*/

let a: [i32; 5] = [1, 2, 3, 4, 5];

/* Here, `i32` is the type of each element. After the semicolon, the number
5 indicates the array contains five elements.

You can also initialize an array to contain the same value for each
element by specifying the initial value, followed by a semicolon, and then
the length of the array in square brackets, as shown here: */

let a = [3; 5];

/* The array named a will contain 5 elements that will all be set to the value
`3` initially. This is the same as writing `let a = [3, 3, 3, 3, 3];`
but in a more concise way. Access array elements with index: */

fn main() {
    let a = [1, 2, 3, 4, 5];

    let first = a[0];
    let second = a[1];
}

// Invalid array access:

```rust
use std::io;

fn main() {
    let a = [1, 2, 3, 4, 5];

    println!("Please enter an array index.");

    let mut index = String::new();

    io::stdin()
        .read_line(&mut index)
        .expect("Failed to read line");

    let index: usize = index
        .trim()
        .parse()
        .expect("Index entered was not a number");

    let element = a[index];

    println!("The value of the element at index {index} is: {element}");
}

/* This code compiles successfully. If you run this code using `cargo run`
and enter `0, 1, 2, 3`, or `4`, the program will print out the corresponding
value at that index in the array. If you instead enter a number past the
end of the array, such as 10, you’ll see output like this:

```console
thread 'main' panicked at src/main.rs:19:19:
index out of bounds: the len is 5 but the index is 10
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
```

The program resulted in a runtime error at the point of using an invalid
value in the indexing operation. The program exited with an error message
and didn’t execute the final println! statement. When you attempt to access
an element using indexing, Rust will check that the index you’ve specified
is less than the array length. If the index is greater than or equal to the
length, Rust will panic. This check has to happen at runtime, especially in
this case, because the compiler can’t possibly know what value a user will
enter when they run the code later.

This is an example of Rust’s memory safety principles in action. In many
low-level languages, this kind of check is not done, and when you provide
an incorrect index, invalid memory can be accessed. Rust protects you
against this kind of error by immediately exiting instead of allowing the
memory access and continuing.



PRINTING
========

`print!` & `println!` is a macro not a function.
`println!("{}", value)` the `{}` is where value is inserted.
`println!("{:?}", value);` here `{:?}` displays values in a debug format.

Example: `println!("{:?}", vec![1, 2, 3]); // Output: [1, 2, 3]`

`{:#?}` is for indented, more readable output. (pretty print)

`format!` to hold format text.



FUNCTIONS
=========

Rust code uses snake case as the conventional style for function and
variable names. **Statements** are instructions that perform some action and do
not return a value. **Expressions** evaluate to a resultant value.
Let’s look at some examples. `let y = 6;` is a statement.

function definations are also statements. Statements do not return a value.
Therefore, you can’t assign a let statement to another variable, as the
following code tries to do; you’ll get an error: */

fn main() {
    let x = (let y = 6);
}

/* it will output this error

```console
$ cargo run
   Compiling functions v0.1.0 (file:///projects/functions)
error: expected expression, found `let` statement
 --> src/main.rs:2:14
  |
2 |     let x = (let y = 6);
  |              ^^^
  |
  = note: only supported directly in conditions of `if` and `while` expressions

warning: unnecessary parentheses around assigned value
 --> src/main.rs:2:13
  |
2 |     let x = (let y = 6);
  |             ^         ^
  |
  = note: `#[warn(unused_parens)]` on by default
help: remove these parentheses
  |
2 -     let x = (let y = 6);
2 +     let x = let y = 6;
  |

warning: `functions` (bin "functions") generated 1 warning
error: could not compile `functions` (bin "functions") due to 1 previous error; 1 warning emitted
```

The `let y = 6` statement does not return a value, so there isn’t
anything for x to bind to. This is different from what happens in
other languages, such as C and Ruby, where the assignment returns
the value of the assignment. In those languages, you can write
`x = y = 6` and have both `x` and `y` have the value `6`; that is
not the case in Rust.

Expressions evaluate to a value and make up most of the rest of the
code that you’ll write in Rust. Consider a math operation, such as
`5 + 6`, which is an expression that evaluates to the value `11`.
Expressions can be part of statements

The `6` text in the statement `let y = 6;` is an expression
that evaluates to the value `6`. Calling a function is an expression.
Calling a macro is an expression. A new scope block created with curly
brackets is an expression, for example: */

fn main() {
    let y = {
        let x = 3;
        x + 1
    };

    println!("The value of y is: {y}");
}


// This expression:

{
    let x = 3;
    x + 1
}

/* is a block that, in this case, evaluates to `4`. That value gets bound
to `y` as part of the let statement. Note that the `x + 1` line doesn’t
have a semicolon at the end. Expressions do not include ending semicolons.

if a function is supposed to return something and the logical end is a
statement then it will fail to compile. */

fn main() {
    let x = plus_one(5);

    println!("The value of x is: {x}");
}

fn plus_one(x: i32) -> i32 {
    x + 1;
}

/* this outputs the following error

```console
$ cargo run
   Compiling functions v0.1.0 (file:///projects/functions)
error[E0308]: mismatched types
 --> src/main.rs:7:24
  |
7 | fn plus_one(x: i32) -> i32 {
  |    --------            ^^^ expected `i32`, found `()`
  |    |
  |    implicitly returns `()` as its body has no tail or `return` expression
8 |     x + 1;
  |          - help: remove this semicolon to return this value

For more information about this error, try `rustc --explain E0308`.
error: could not compile `functions` (bin "functions") due to 1 previous error
```

The main error message, `mismatched types`, reveals the core issue
with this code. The definition of the function `plus_one` says that
it will return an `i32`, but statements don’t evaluate to a value,
which is expressed by `()`, the unit type. Therefore, nothing is
returned, which contradicts the function definition and results in
an error. In this output, Rust provides a message to possibly help
rectify this issue: it suggests removing the semicolon, which would
fix the error.



MODULES
=======

The fundamental structure for organizing code in rust is through using a module
system and crates. A module in Rust allows grouping of related function
definitions and struct definitions in a named scope. It’s defined using the mod
keyword. The modules can also be nested and make code more concise, readable,
and manageable.

On the other hand, a crate is a binary or a library project in Rust. It’s the
largest compilation unit of Rust. It’s a tree of modules that produces a library
or executable. The crate root is a source file which the Rust compiler starts
from and makes up the root module of your crate (e.g. main.rs or lib.rs).

By default, the items in a module have private visibility, but this can be
overridden with the `pub` modifier. Only the public items of a module can be
accessed from outside the module scope.

`use` declaration can be used to bind a full path to a new name, for easier
access. `as` keyword can be used to bind imports to a different name. The
`super` and `self` keywords can be used in the path to remove ambiguity when
accessing items and to prevent unnecessary hardcoding of paths.
*/

// A module named `my_mod`
mod my_mod {
    // Items in modules default to private visibility.
    fn private_function() {
        println!("called `my_mod::private_function()`");
    }

    // Use the `pub` modifier to override default visibility.
    pub fn function() {
        println!("called `my_mod::function()`");
    }

    // Items can access other items in the same module, even when private.
    pub fn indirect_access() {
        print!("called `my_mod::indirect_access()`, that\n> ");
        private_function();
    }

    // Modules can also be nested
    pub mod nested {
        pub fn function() {
            println!("called `my_mod::nested::function()`");
        }

        #[allow(dead_code)]
        fn private_function() {
            println!("called `my_mod::nested::private_function()`");
        }

        // Functions declared using `pub(in path)` syntax are only visible
        // within the given path. `path` must be a parent or ancestor module
        pub(in crate::my_mod) fn public_function_in_my_mod() {
            print!("called `my_mod::nested::public_function_in_my_mod()`, that\n> ");
            public_function_in_nested();
        }

        // Functions declared using `pub(self)` syntax are only visible within
        // the current module, which is the same as leaving them private
        pub(self) fn public_function_in_nested() {
            println!("called `my_mod::nested::public_function_in_nested()`");
        }

        // Functions declared using `pub(super)` syntax are only visible within
        // the parent module
        pub(super) fn public_function_in_super_mod() {
            println!("called `my_mod::nested::public_function_in_super_mod()`");
        }
    }

    pub fn call_public_function_in_my_mod() {
        print!("called `my_mod::call_public_function_in_my_mod()`, that\n> ");
        nested::public_function_in_my_mod();
        print!("> ");
        nested::public_function_in_super_mod();
    }

    // pub(crate) makes functions visible only within the current crate
    pub(crate) fn public_function_in_crate() {
        println!("called `my_mod::public_function_in_crate()`");
    }

    // Nested modules follow the same rules for visibility
    mod private_nested {
        #[allow(dead_code)]
        pub fn function() {
            println!("called `my_mod::private_nested::function()`");
        }

        // Private parent items will still restrict the visibility of a child item,
        // even if it is declared as visible within a bigger scope.
        #[allow(dead_code)]
        pub(crate) fn restricted_function() {
            println!("called `my_mod::private_nested::restricted_function()`");
        }
    }
}

fn function() {
    println!("called `function()`");
}

fn main() {
    function();
    my_mod::function(); // Modules allow disambiguation between items that have the same name.

    // Public items, including those inside nested modules, can be
    // accessed from outside the parent module.
    my_mod::indirect_access();
    my_mod::nested::function();
    my_mod::call_public_function_in_my_mod();

    // pub(crate) items can be called from anywhere in the same crate
    my_mod::public_function_in_crate();

    // pub(in path) items can only be called from within the module specified
    // Error! function `public_function_in_my_mod` is private
    //my_mod::nested::public_function_in_my_mod();
    // TODO ^ Try uncommenting this line

    // Private items of a module cannot be directly accessed, even if
    // nested in a public module:

    // Error! `private_function` is private
    //my_mod::private_function();
    // TODO ^ Try uncommenting this line

    // Error! `private_function` is private
    //my_mod::nested::private_function();
    // TODO ^ Try uncommenting this line

    // Error! `private_nested` is a private module
    //my_mod::private_nested::function();
    // TODO ^ Try uncommenting this line

    // Error! `private_nested` is a private module
    //my_mod::private_nested::restricted_function();
    // TODO ^ Try uncommenting this line
}



/* STRUCTS VISIBILITY
   ===============
*/
mod my {
    // A public struct with a public field of generic type `T`
    pub struct OpenBox<T> {
        pub contents: T,
    }

    // A public struct with a private field of generic type `T`
    pub struct ClosedBox<T> {
        contents: T,
    }

    impl<T> ClosedBox<T> {
        // A public constructor method
        pub fn new(contents: T) -> ClosedBox<T> {
            ClosedBox {
                contents: contents,
            }
        }
    }
}

fn main() {
    // Public structs with public fields can be constructed as usual
    let open_box = my::OpenBox { contents: "public information" };

    // and their fields can be normally accessed.
    println!("The open box contains: {}", open_box.contents);

    // Public structs with private fields cannot be constructed using field names.
    // Error! `ClosedBox` has private fields
    //let closed_box = my::ClosedBox { contents: "classified information" };
    // TODO ^ Try uncommenting this line

    // However, structs with private fields can be created using
    // public constructors
    let _closed_box = my::ClosedBox::new("classified information");

    // and the private fields of a public struct cannot be accessed.
    // Error! The `contents` field is private
    //println!("The closed box contains: {}", _closed_box.contents);
    // TODO ^ Try uncommenting this line
} /*



CLOSURES
========

Closures are functions that can capture the enclosing environment. For example,
a closure that captures the `x` variable:

|val| val + x

The syntax and capabilities of closures make them very convenient for on the
fly usage. Calling a closure is exactly like calling a function. However, both
input and return types can be inferred and input variable names must be
specified. Other characteristics of closures include:

- using `||` instead of `()` around input variables.
- optional body delimitation (`{}`) for a single line expression (mandatory otherwise).
- the ability to capture the outer environment variables. */

fn main() {
    let outer_var = 42;

    // A regular function can't refer to variables in the enclosing environment
    //fn function(i: i32) -> i32 { i + outer_var }

    // Closures are anonymous, here we are binding them to references.
    // Annotation is identical to function annotation but is optional
    // as are the `{}` wrapping the body. These nameless functions
    // are assigned to appropriately named variables.
    let closure_annotated = |i: i32| -> i32 { i + outer_var };
    let closure_inferred  = |i     |          i + outer_var  ;

    // Call the closures.
    println!("closure_annotated: {}", closure_annotated(1));
    println!("closure_inferred: {}", closure_inferred(1));
    // Once closure's type has been inferred, it cannot be inferred again with another type.
    //println!("cannot reuse closure_inferred with another type: {}", closure_inferred(42i64));
    // TODO: uncomment the line above and see the compiler error.

    // A closure taking no arguments which returns an `i32`.
    // The return type is inferred.
    let one = || 1;
    println!("closure returning one: {}", one());
}
/* LEARN MORE: <https://doc.rust-lang.org/rust-by-example/fn/closures/capture.html>



COMMENTS
========

- Single line comments `//`
- Multi line comments `/* <multiple lines> */`



CONTROL FLOW
============

If Else
-------

- `if` expression doesn't require parentheses.
- `else if` exists.
- `if` in `let` statement. example: */

fn main() {
    let condition = true;
    let number = if condition { 5 } else { 6 };

    println!("The value of number is: {number}");
}

/* Remember that blocks of code evaluate to the last expression in them,
and numbers by themselves are also expressions. In this case, the value
of the whole if expression depends on which block of code executes.
This means the values that have the potential to be results from each arm
of the if must be the same type like above example, the results of both the
if arm and the else arm were i32 integers. If the types are mismatched, as
in the following example, we’ll get an error: */

fn main() {
    let condition = true;
    let number = if condition { 5 } else { "six" };
    println!("The value of number is: {number}");
}

/* we get following error:

```console
$ cargo run
   Compiling branches v0.1.0 (file:///projects/branches)
error[E0308]: `if` and `else` have incompatible types
 --> src/main.rs:4:44
  |
4 |     let number = if condition { 5 } else { "six" };
  |                                 -          ^^^^^ expected integer, found `&str`
  |                                 |
  |                                 expected because of this

For more information about this error, try `rustc --explain E0308`.
error: could not compile `branches` (bin "branches") due to 1 previous error
```

Rust needs to know at compile time what type the number variable is,
definitively. Knowing the type of number lets the compiler verify the
type is valid everywhere we use number. Rust wouldn’t be able to do
that if the type of number was only determined at runtime; the compiler
would be more complex and would make fewer guarantees about the code if
it had to keep track of multiple hypothetical types for any variable.



LOOPS
=====

Rust has three loops. `loop`, `while`, `for`.

- loop executes forever unless explicitly it's told to stop. */

fn main() {
    loop {
        println!("again!");
    }
}

/* will loop forever unless `ctrl-c`. Rust also provides a way to break
out of a loop using code. You can place the `break` keyword within the
loop to tell the program when to stop executing the loop. use `continue`
to skip iterations.

RETURNING VALUES FROM LOOPS
---------------------------

One of the uses of a `loop` is to retry an operation you know might fail,
such as checking whether a thread has completed its job. You might also
need to pass the result of that operation out of the loop to the rest of
your code. To do this, you can add the value you want returned after the
`break` expression you use to stop the loop; that value will be returned
out of the loop so you can use it, as shown here: */

fn main() {
    let mut counter = 0;
    let result = loop {
        counter += 1;
        if counter == 10 {
            break counter * 2;
        }
    };
    println!("The result is {result}");
}

/* Before the loop, we declare a variable named `counter` and initialize it
to `0`. Then we declare a variable named `result` to hold the value returned
from the loop. On every iteration of the loop, we add `1` to the counter
variable, and then check whether the `counter` is equal to `10`. When it is,
we use the `break` keyword with the value `counter * 2`. After the loop, we
use a semicolon to end the statement that assigns the value to `result`.
Finally, we print the value in `result`, which in this case is `20`.

You can also `return` from inside a loop. While `break` only exits the
current loop, `return` always exits the current function.

LOOP LABELS
-----------

If you have loops within loops, break and continue apply to the innermost
loop at that point. You can optionally specify a loop label on a loop that
you can then use with break or continue to specify that those keywords apply
to the labeled loop instead of the innermost loop. Loop labels must begin with
a single quote. Here’s an example with two nested loops: */

fn main() {
    let mut count = 0;
    'counting_up: loop {
        println!("count = {count}");
        let mut remaining = 10;

        loop {
            println!("remaining = {remaining}");
            if remaining == 9 {
                break;
            }
            if count == 2 {
                break 'counting_up;
            }
            remaining -= 1;
        }
        count += 1;
    }
    println!("End count = {count}");
}

/* The outer loop has the label `'counting_up`, and it will count up from `0`
to `2`. The inner loop without a label counts down from 10 to 9. The first
`break` that doesn’t specify a label will exit the inner loop only. The
`break 'counting_up;` statement will exit the outer loop. This code prints:

```console
$ cargo run
   Compiling loops v0.1.0 (file:///projects/loops)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.58s
     Running `target/debug/loops`
count = 0
remaining = 10
remaining = 9
count = 1
remaining = 10
remaining = 9
count = 2
remaining = 10
End count = 2
```

WHILE LOOP
----------

A program will often need to evaluate a condition within a loop. While the
condition is `true`, the loop runs. When the condition ceases to be `true`,
the program calls `break`, stopping the loop. It’s possible to implement
behavior like this using a combination of `loop`, `if`, `else`, and `break`;
you could try that now in a program, if you’d like. However, this pattern is
so common that Rust has a built-in language construct for it, called a `while`
loop. */

fn main() {
    let mut number = 3;
    while number != 0 {
        println!("{number}!");

        number -= 1;
    }
    println!("LIFTOFF!!!");
}

/* This construct eliminates a lot of nesting that would be necessary if you used
`loop`, `if`, `else`, and `break`, and it’s clearer. While a condition
evaluates to `true`, the code runs; otherwise, it exits the loop.

**Aside**: this approach is error prone; we could cause the program to panic if
the index value or test condition is incorrect. For example, if you changed the
definition of the a array to have four elements but forgot to update the
condition to while index < 4, the code would panic. It’s also slow, because the
compiler adds runtime code to perform the conditional check of whether the
index is within the bounds of the array on every iteration through the loop.

FOR LOOP
--------

As a more concise alternative, you can use a for loop and execute some code for
each item in a collection. See below: */

fn main() {
    let a = [10, 20, 30, 40, 50];
    for element in a {
        println!("the value is: {element}");
    }
}

/* We’ve now increased the safety of the code and eliminated the chance of bugs
that might result from going beyond the end of the array or not going far
enough and missing some items.

The safety and conciseness of for loops make them the most commonly used loop
construct in Rust. Even in situations in which you want to run some code a
certain number of times, as in the countdown example that used a `while` loop.

The way to do that would be to use a `Range`, provided by the standard library,
which generates all numbers in sequence starting from one number and ending
before another number.

Here’s what the countdown would look like using a `for` loop and another method
we’ve not yet talked about, rev, to reverse the range: */

fn main() {
    for number in (1..4).rev() {
        println!("{number}!");
    }
    println!("LIFTOFF!!!");
}

/* The `rev` method reverses the range so that it goes from 3 to 1 instead of
from 1 to 3. The range `1..4` is a `Range` that starts at 1 and ends at 4
(exclusive).



PATTERN MATCHING
================

A extremely powerful control flow construct that allows you to compare a value
against a series of patterns and then execute code based on which pattern
matches. */

enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter,
}

fn value_in_cents(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => 1,
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter => 25,
    }
}

// Matching with `Option<T>`

```rs
fn plus_one(x: Option<i32>) -> Option<i32> {
    match x {
        None => None,
        Some(i) => Some(i + 1),
    }
}

let five = Some(5);
let six = plus_one(five);
let none = plus_one(None);

// If let

let config_max = Some(3u8);
match config_max {
    Some(max) => println!("The maximum is configured to be {max}"),
    _ => (),
}

// above can be written in concise form with if let

let config_max = Some(3u8);
if let Some(max) = config_max {
    println!("The maximum is configured to be {max}");
} /*



TRAITS
======

Traits in Rust define behaviors that are shared among different data types. (
collection of methods defined for an unknown type). Implementing traits for data
types is a great way to group method signatures together and define a set of
behaviors your types require. Essentially, anything with a certain trait applied
to it will “inherit” the behavior of that trait’s methods, but this is not the
same thing as inheritance found in object-oriented programming languages.

Traits are abstract; it’s not possible to create instances of traits. However,
we can define pointers of trait types, and these can hold any data type that
implements the trait. A trait is implemented for something else with the
syntax. */

impl TraitAbc for Xyz {
    // ...
}

/* which can be a concrete type or another trait.



STRUCTS
=======

In Rust, a struct is a custom data type used for grouping related values
together into one entity. Structs are similar to classes in other programming
languages. Essentially, each struct creates a new type that we can use to
streamline complex data handling.

There are three types of struct in Rust:

**Classic ‘C’** structs are named-field structs and are the most commonly
used. */

struct Person {
    name: String,
    age: u32,
    email: String,
}

fn main() {
    let person = Person {
        name: String::from("Alice"),
        age: 30,
        email: String::from("alice@example.com"),
    };
    println!("Name: {}, Age: {}", person.name, person.age);
} /*

Tuple structs
-------------

while not being common, are useful when you want to couple together just a few
data points that don’t need field names. */

struct Point(i32, i32);

fn main() {
    let origin = Point(0, 0);
    println!("Point: ({}, {})", origin.0, origin.1);
} /*

Unit structs
------------

useful in situations where you want to implement a trait on some type, but don’t
have any data that you want to store in the type itself. */

struct Unit;

impl Unit {
    fn do_something() {
        println!("Unit struct called!");
    }
}

fn main() {
    let u = Unit;
    u.do_something();
} /*



ENUMERATIONS
============

A custom data type that allows you to define a type by enumerating (listing out
one-by-one) all of its possible variants. In Rust, if something is one of a
given set of possibilities (e.g., `Rock or Paper or Scissors`), it’s probably
appropriate to represent that data with an enum, like so: */

enum RpsChoice {
    Rock,
    Paper,
    Scissors
} /*

An instance of an enum can be one and only one of the enum’s declared variants
at any given time. Unlike enumerations in some other languages, variants in Rust
are not restricted to a singular data type. When you define an enum, you can
decide for each of its possible variants whether or not that variant will hold
additional embedded data; each variant of the enum is also allowed to hold data
of completely different types and amounts. You can even embed structs and other
enums in a variant, making enums incredibly versatile.

Enums in Rust are one way to enable simple pattern matching for a value, which
allows you to compare the interior structure of a value to a series of patterns
using a match block. For example, you can execute different branches of code
based on whether an `RpsChoice` value is `Rock`, `Paper`, or `Scissors` without
a verbose tree of `if/else` blocks. You can also handle whatever data might be
embedded within that instance of the enum variant, as you will do frequently
with Rust’s standard `Option<T>` and `Result<T, E>` enums.



IMPL BLOCKS
===========

Impl blocks use the `impl` keyword, and are used to implement behavior in the
form of methods for a struct, enum, or trait. If you want your data type or
trait to have methods, you need a corresponding impl block containing functions
for the type or trait.

Note that `self` and `Self` have different meanings in the context of an `impl`
block’s functions. `self` represents the specific value in your program that’s
calling the method and passing itself as an argument, while `Self` is syntax
sugar for the impl block’s data type, which is commonly used in constructor
methods that return a new instance of the type. */

struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    // Associated function to create a new Rectangle (not tied to an instance)
    fn new(width: u32, height: u32) -> Self {
        Self { width, height }
    }

    // Method to calculate area (requires an instance of Rectangle)
    fn area(&self) -> u32 {
        self.width * self.height
    }

    // Method to check if the rectangle is square
    fn is_square(&self) -> bool {
        self.width == self.height
    }
}

fn main() {
    let rect = Rectangle::new(10, 20);
    println!("Area: {}", rect.area());    
    println!("Is square: {}", rect.is_square());
} /*



`self`, `&self`, `&mut self`
============================ */

struct Counter {
    value: u32,
}

impl Counter {
    // Associated function to create a new Counter
    fn new() -> Self {
        Counter { value: 0 }
    }

    // Method to increment the counter (mutable borrow)
    fn increment(&mut self) {
        self.value += 1;
    }

    // Method to get the current value (immutable borrow)
    fn get_value(&self) -> u32 {
        self.value
    }
}

fn main() {
    let mut counter = Counter::new();
    counter.increment();
    println!("Counter: {}", counter.get_value());
} /*



GENERIC TYPES
=============

A type parameter is specified as generic by the use of angle brackets and upper
camel case: <Aaa, Bbb, ...>. `fn foo<T>(arg: T) { ... }` T is a generic type. */

// A concrete type `A`.
struct A;

// In defining the type `Single`, the first use of `A` is not preceded by `<A>`.
// Therefore, `Single` is a concrete type, and `A` is defined as above.
struct Single(A);
//            ^ Here is `Single`s first use of the type `A`.

// Here, `<T>` precedes the first use of `T`, so `SingleGen` is a generic type.
// Because the type parameter `T` is generic, it could be anything, including
// the concrete type `A` defined at the top.
struct SingleGen<T>(T);

fn main() {
    // `Single` is concrete and explicitly takes `A`.
    let _s = Single(A);
    
    // Create a variable `_char` of type `SingleGen<char>`
    // and give it the value `SingleGen('a')`.
    // Here, `SingleGen` has a type parameter explicitly specified.
    let _char: SingleGen<char> = SingleGen('a');

    // `SingleGen` can also have a type parameter implicitly specified:
    let _t    = SingleGen(A); // Uses `A` defined at the top.
    let _i32  = SingleGen(6); // Uses `i32`.
    let _char = SingleGen('a'); // Uses `char`.
} /* <https://doc.rust-lang.org/rust-by-example/generics/gen_fn.html>



STACK & HEAP
============

Many programming languages don’t require you to think about the stack and the
heap very often. But in a systems programming language like Rust, whether a
value is on the stack or the heap affects how the language behaves and why you
have to make certain decisions.

Both the stack and the heap are parts of memory available to your code to use
at runtime, but they are structured in different ways. The stack stores values
in the order it gets them and removes the values in the opposite order. This is
referred to as last in, first out. Think of a stack of plates: when you add
more plates, you put them on top of the pile, and when you need a plate, you
take one off the top. Adding or removing plates from the middle or bottom
wouldn’t work as well! Adding data is called pushing onto the stack, and
removing data is called popping off the stack. All data stored on the stack
must have a known, fixed size. Data with an unknown size at compile time or
a size that might change must be stored on the heap instead.

The heap is less organized: when you put data on the heap, you request a
certain amount of space. The memory allocator finds an empty spot in the
heap that is big enough, marks it as being in use, and returns a pointer,
which is the address of that location. This process is called allocating
on the heap and is sometimes abbreviated as just allocating (pushing values
onto the stack is not considered allocating). Because the pointer to the
heap is a known, fixed size, you can store the pointer on the stack, but
when you want the actual data, you must follow the pointer. Think of being
seated at a restaurant. When you enter, you state the number of people in
your group, and the host finds an empty table that fits everyone and leads
you there. If someone in your group comes late, they can ask where you’ve
been seated to find you.

Pushing to the stack is faster than allocating on the heap because the
allocator never has to search for a place to store new data; that location
is always at the top of the stack. Comparatively, allocating space on the
heap requires more work because the allocator must first find a big enough
space to hold the data and then perform bookkeeping to prepare for the next
allocation.

Accessing data in the heap is slower than accessing data on the stack because
you have to follow a pointer to get there. Contemporary processors are faster
if they jump around less in memory. Continuing the analogy, consider a server
at a restaurant taking orders from many tables. It’s most efficient to get all
the orders at one table before moving on to the next table. Taking an order
from table A, then an order from table B, then one from A again, and then one
from B again would be a much slower process. By the same token, a processor can
do its job better if it works on data that’s close to other data (as it is on
the stack) rather than farther away (as it can be on the heap).

When your code calls a function, the values passed into the function (including,
potentially, pointers to data on the heap) and the function’s local variables
get pushed onto the stack. When the function is over, those values get popped
off the stack.

Keeping track of what parts of code are using what data on the heap, minimizing
the amount of duplicate data on the heap, and cleaning up unused data on the
heap so you don’t run out of space are all problems that ownership addresses.
Once you understand ownership, you won’t need to think about the stack and the
heap very often, but knowing that the main purpose of ownership is to manage
heap data can help explain why it works the way it does.



BOX
===

All values in Rust are stack allocated by default. Values can be boxed
(allocated on the heap) by creating a `Box<T>`. A box is a smart pointer to a
heap allocated value of type `T`. When a box goes out of scope, its destructor
is called, the inner object is destroyed, and the memory on the heap is freed.

Boxed values can be dereferenced using the `*` operator; this removes one layer
of indirection. */

use std::mem;

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

// A Rectangle can be specified by where its top left and bottom right 
// corners are in space
#[allow(dead_code)]
struct Rectangle {
    top_left: Point,
    bottom_right: Point,
}

fn origin() -> Point {
    Point { x: 0.0, y: 0.0 }
}

fn boxed_origin() -> Box<Point> {
    // Allocate this point on the heap, and return a pointer to it
    Box::new(Point { x: 0.0, y: 0.0 })
}

fn main() {
    // (all the type annotations are superfluous)
    // Stack allocated variables
    let point: Point = origin();
    let rectangle: Rectangle = Rectangle {
        top_left: origin(),
        bottom_right: Point { x: 3.0, y: -4.0 }
    };

    // Heap allocated rectangle
    let boxed_rectangle: Box<Rectangle> = Box::new(Rectangle {
        top_left: origin(),
        bottom_right: Point { x: 3.0, y: -4.0 },
    });

    // The output of functions can be boxed
    let boxed_point: Box<Point> = Box::new(origin());

    // Double indirection
    let box_in_a_box: Box<Box<Point>> = Box::new(boxed_origin());

    println!("Point takes {} bytes on stack", mem::size_of_val(&point));
    println!("Rectangle takes {} bytes on stack", mem::size_of_val(&rectangle));

    // box size == pointer size
    println!("Boxed point takes {} bytes on stack", mem::size_of_val(&boxed_point));
    println!("Boxed rectangle takes {} bytes on stack", mem::size_of_val(&boxed_rectangle));
    println!("Boxed box takes {} bytes on stack", mem::size_of_val(&box_in_a_box));

    // Copy the data contained in `boxed_point` into `unboxed_point`
    let unboxed_point: Point = *boxed_point;
    println!("Unboxed point takes {} bytes on stack", mem::size_of_val(&unboxed_point));
} /*



OWNERSHIP
=========

Ownership is a set of rules that govern how a Rust program manages memory.
All programs have to manage the way they use a computer’s memory while running.
Some languages have garbage collection that regularly looks for no-longer-used
memory as the program runs; in other languages, the programmer must explicitly
allocate and free the memory. Rust uses a third approach: memory is managed
through a system of ownership with a set of rules that the compiler checks.
If any of the rules are violated, the program won’t compile. None of the
features of ownership will slow down your program while it’s running.

Ownership Rules
---------------

- Each value in Rust has an owner.
- There can only be one owner at a time.
- When the owner goes out of scope, the value will be dropped.

Variable Scope
--------------

A scope is the range within a program for which an item is valid. Take the
following variable: */

let s = "hello";

/* The variable `s` refers to a string literal, where the value of the string is
hardcoded into the text of our program. The variable is valid from the point at
which it’s declared until the end of the current scope. */

{                      // s is not valid here, it’s not yet declared
    let s = "hello";   // s is valid from this point forward
    // do stuff with s
}                      // this scope is now over, and s is no longer valid

/* In other words, there are two important points in time here:

- When `s` comes into scope, it is valid.
- It remains valid until it goes out of scope.

At this point, the relationship between scopes and when variables are valid is
similar to that in other programming languages. Now we’ll build on top of this
understanding by introducing the String type.

The `String` type
-----------------

String is stored on the heap. You can create a String from a string literal
using the from function, like so: */

let s = String::from("hello");

/* The double `::` operator allows us to namespace this particular `from` function
under the `String` type rather than using some sort of name like `string_from`.

This kind of string can be mutated: */

let mut s = String::from("hello");
s.push_str(" world");
println!("{s}");

/* So, what’s the difference here? Why can `String` be mutated but literals
cannot? The difference is in how these two types deal with memory.

Memory and Allocation
---------------------

In rust, the memory is automatically returned once the variable that owns it
goes out of scope. Here’s a version of our scope example from Listing 4-1 using
a String instead of a string literal: */

{
    let s = String::from("hello"); // s is valid from this point forward
    // do stuff with s
}                                  // this scope is now over, and s is no
                                    // longer valid

/* There is a natural point at which we can return the memory our String needs to
the allocator: when s goes out of scope. When a variable goes out of scope,
Rust calls a special function for us. This function is called drop, and it’s
where the author of String can put the code to return the memory. Rust calls
drop automatically at the closing curly bracket.



ERROR HANDLING
==============

Error handling in Rust is achieved primarily through two types of results.

An enum called `Option<T>` in the std library is used when absence is a
possibility. It manifests itself as one of two "options":

`Some(T)`: An element of type T was found
`None`: No element was found

These cases can either be explicitly handled via `match` or implicitly with
`unwrap`. Implicit handling will either return the inner element or panic. (it
is possible to manually customize panic with `expect`, but `unwrap` otherwise
leaves us with a less meaningful output than explicit handling). */

// The adult has seen it all, and can handle any drink well.
// All drinks are handled explicitly using `match`.
fn give_adult(drink: Option<&str>) {
    match drink {
        Some("lemonade") => println!("Yuck! Too sugary."),
        // inner matches to every other value
        Some(inner) => println!("{}? How nice.", inner),
        None => println!("No drink? Oh well."),
    }
}

// Others will `panic` before drinking sugary drinks.
// All drinks are handled implicitly using `unwrap`.
fn drink(drink: Option<&str>) {
    // `unwrap` returns a `panic` when it receives a `None`.
    let inside = drink.unwrap();
    if inside == "lemonade" { panic!("AAAaaaaa!!!!"); }

    println!("I love {}s!!!!!", inside);
}

fn main() {
    let water  = Some("water");
    let lemonade = Some("lemonade");
    let void  = None;

    give_adult(water);
    give_adult(lemonade);
    give_adult(void);

    let coffee = Some("coffee");
    let nothing = None;

    drink(coffee);
    drink(nothing);
}

/* You can unpack Options by using match statements, but it's often easier to
use the ? operator. If x is an Option, then evaluating x? will return the
underlying value if x is Some, otherwise it will terminate whatever function is
being executed and return None. */

fn next_birthday(current_age: Option<u8>) -> Option<String> {
    // If `current_age` is `None`, this returns `None`.
    // If `current_age` is `Some`, the inner `u8` value + 1
    // gets assigned to `next_age`
    let next_age: u8 = current_age? + 1;
    Some(format!("Next year I will be {}", next_age))
}

/* `?` operator can be chained, but the function must return a Result or
Option. Result is a richer version of the Option type that describes possible
error instead of possible absence. That is, `Result<T, E>` could have one of
two outcomes:

- Ok(T): An element T was found
- Err(E): An error was found with element E

By convention, the expected outcome is Ok while the unexpected outcome is Err.
Like Option, Result has many methods associated with it. unwrap(), for example,
either yields the element T or panics. For case handling, there are many
combinators between Result and Option that overlap.

In working with Rust, you will likely encounter methods that return the Result
type, such as the parse() method. It might not always be possible to parse a
string into the other type, so parse() returns a Result indicating possible
failure. Let's see what happens when we successfully and unsuccessfully parse()
a string: */

fn multiply(first_number_str: &str, second_number_str: &str) -> i32 {
    // Let's try using `unwrap()` to get the number out. Will it bite us?
    let first_number = first_number_str.parse::<i32>().unwrap();
    let second_number = second_number_str.parse::<i32>().unwrap();
    first_number * second_number
}

fn main() {
    let twenty = multiply("10", "2");
    println!("double is {}", twenty);

    let tt = multiply("t", "2");
    println!("double is {}", tt);
}

/* In the unsuccessful case, parse() leaves us with an error for unwrap() to
panic on. Additionally, the panic exits our program and provides an unpleasant
error message.

To improve the quality of our error message, we should be more specific about
the return type and consider explicitly handling the error. main is also able
to have a return type of Result. If an error occurs within the main function it
will return an error code and print a debug representation of the error
(using the Debug trait). */

use std::num::ParseIntError;

fn main() -> Result<(), ParseIntError> {
    let number_str = "10";
    let number = match number_str.parse::<i32>() {
        Ok(number)  => number,
        Err(e) => return Err(e),
    };
    println!("{}", number);
    Ok(())
} /*

Map for result
---------------

Panicking in the previous example's multiply does not make for robust code.
Generally, we want to return the error to the caller so it can decide what is
the right way to respond to errors.

We first need to know what kind of error type we are dealing with. To determine
the Err type, we look to parse(), which is implemented with the FromStr trait
for i32. As a result, the Err type is specified as ParseIntError.

In the example below, the straightforward match statement leads to code that is
overall more cumbersome. */

use std::num::ParseIntError;

// With the return type rewritten, we use pattern matching without `unwrap()`.
fn multiply(first_number_str: &str, second_number_str: &str) -> Result<i32, ParseIntError> {
    match first_number_str.parse::<i32>() {
        Ok(first_number)  => {
            match second_number_str.parse::<i32>() {
                Ok(second_number)  => {
                    Ok(first_number * second_number)
                },
                Err(e) => Err(e),
            }
        },
        Err(e) => Err(e),
    }
}

fn print(result: Result<i32, ParseIntError>) {
    match result {
        Ok(n)  => println!("n is {}", n),
        Err(e) => println!("Error: {}", e),
    }
}

fn main() {
    // This still presents a reasonable answer.
    let twenty = multiply("10", "2");
    print(twenty);

    // The following now provides a much more helpful error message.
    let tt = multiply("t", "2");
    print(tt);
}

/* Luckily, Option's map, and_then, and many other combinators are also
implemented for Result. Result contains a complete listing. */

use std::num::ParseIntError;

// As with `Option`, we can use combinators such as `map()`.
// This function is otherwise identical to the one above and reads:
// Multiply if both values can be parsed from str, otherwise pass on the error.
fn multiply(first_number_str: &str, second_number_str: &str) -> Result<i32, ParseIntError> {
    first_number_str.parse::<i32>().and_then(|first_number| {
        second_number_str.parse::<i32>().map(|second_number| first_number * second_number)
    })
}

fn print(result: Result<i32, ParseIntError>) {
    match result {
        Ok(n)  => println!("n is {}", n),
        Err(e) => println!("Error: {}", e),
    }
}

fn main() {
    // This still presents a reasonable answer.
    let twenty = multiply("10", "2");
    print(twenty);

    // The following now provides a much more helpful error message.
    let tt = multiply("t", "2");
    print(tt);
} /*

Early returns
--------------

Another way to deal with this case analysis is to use a combination of match
statements and early returns. That is, we can simply stop executing the
function and return the error if one occurs. For some, this form of code can be
easier to both read and write. Consider this version of the previous example,
rewritten using early returns: */

use std::num::ParseIntError;

fn multiply(first_number_str: &str, second_number_str: &str) -> Result<i32, ParseIntError> {
    let first_number = match first_number_str.parse::<i32>() {
        Ok(first_number)  => first_number,
        Err(e) => return Err(e),
    };

    let second_number = match second_number_str.parse::<i32>() {
        Ok(second_number)  => second_number,
        Err(e) => return Err(e),
    };

    Ok(first_number * second_number)
}

fn print(result: Result<i32, ParseIntError>) {
    match result {
        Ok(n)  => println!("n is {}", n),
        Err(e) => println!("Error: {}", e),
    }
}

fn main() {
    print(multiply("10", "2"));
    print(multiply("t", "2"));
} /*

Sometimes we just want the simplicity of unwrap without the possibility of a
panic. Until now, unwrap has forced us to nest deeper and deeper when what we
really wanted was to get the variable out. This is exactly the purpose of ?.

Upon finding an Err, there are two valid actions to take:

1. `panic!` which we already decided to try to avoid if possible
2. `return` because an Err means it cannot be handled

? is almost exactly equivalent to an unwrap which returns instead of panicking
on Errs. Let's see how we can simplify the earlier example that used
combinators: */

use std::num::ParseIntError;

fn multiply(
    first_number_str: &str,
    second_number_str: &str
) -> Result<i32, ParseIntError> {
    let first_number = first_number_str.parse::<i32>()?;
    let second_number = second_number_str.parse::<i32>()?;

    Ok(first_number * second_number)
}

fn print(result: Result<i32, ParseIntError>) {
    match result {
        Ok(n)  => println!("n is {}", n),
        Err(e) => println!("Error: {}", e),
    }
}

fn main() {
    print(multiply("10", "2"));
    print(multiply("t", "2"));
}

/* CUSTOM ERROR TYPES
   ------------------
*/

use std::fmt;

type Result<T> = std::result::Result<T, DoubleError>;

// Define our error types. These may be customized for our error handling cases.
// Now we will be able to write our own errors, defer to an underlying error
// implementation, or do something in between.
#[derive(Debug, Clone)]
struct DoubleError;

// Generation of an error is completely separate from how it is displayed.
// There's no need to be concerned about cluttering complex logic with the display style.
//
// Note that we don't store any extra info about the errors. This means we can't state
// which string failed to parse without modifying our types to carry that information.
impl fmt::Display for DoubleError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid first item to double")
    }
}

fn double_first(vec: Vec<&str>) -> Result<i32> {
    vec.first()
        // Change the error to our new type.
        .ok_or(DoubleError)
        .and_then(|s| {
            s.parse::<i32>()
                // Update to the new error type here also.
                .map_err(|_| DoubleError)
                .map(|i| 2 * i)
        })
}

fn print(result: Result<i32>) {
    match result {
        Ok(n) => println!("The first doubled is {}", n),
        Err(e) => println!("Error: {}", e),
    }
}

fn main() {
    let numbers = vec!["42", "93", "18"];
    let empty = vec![];
    let strings = vec!["tofu", "93", "18"];

    print(double_first(numbers));
    print(double_first(empty));
    print(double_first(strings));
} /*

<https://doc.rust-lang.org/rust-by-example/error/multiple_error_types.html>



SNIPPETS
======== */

enum Direction {
    North,
    East,
    South,
    West,
}

fn main() {
    let my_direction = Direction::North;
    let new_direction = my_direction; // No error, because Direction is Copy
    move_around(new_direction);
}

fn move_around(direction: Direction) {
    // implements logic to move a character around
}

// Define an enum called Shape
enum Shape {
    Circle(f64),         // Variant with associated data (radius)
    Square(f64),         // Variant with associated data (side length)
    Rectangle(f64, f64), // Variant with associated data (width, height)
}

// Function to calculate area based on the shape
fn calculate_area(shape: Shape) -> f64 {
    // calculates the area of the shape
    return 0;
}

fn main() {
    // Create instances of different shapes
    let circle = Shape::Circle(5.0);
    let square = Shape::Square(4.0);
    let rectangle = Shape::Rectangle(3.0, 6.0);
}

fn main() {
    let num: u8 = 9;
    println!("{}", is_fibo(num));
    println!("{}", is_fibo_another(num));
}

fn is_fibo(num: u8) -> u8 {
    if num <= 1 {
        return num;
    }
    return is_fibo(num - 1) + is_fibo(num - 2);
}

fn is_fibo_another(num: u8) -> u8 {
    let mut first = 0;
    let mut second = 1;
    if num == 0 {
        return first;
    }

    if num == 1 {
        return second;
    }

    for _ in 0..(num - 1) {
        let temp = second;
        second += first;
        first = temp;
    }
    return second;
}

fn main() {
    let s1 = String::from("hello");
    let s2 = s1;
    println!("{}", s1); // This line would cause a compile error because ownership has been moved.
}

fn main() {
    let my_string = String::from("hello");
    takes_ownership(my_string);
    println!("{}", my_string); // This line would cause a compile error because ownership has been moved.
}

fn takes_ownership(some_string: String) {
    println!("{}", some_string); // `some_string` now owns the data.
}

// to fix can be .clone() or pass reference or re-assign it

// re-assign it to return ownership

fn main() {
    let s1 = String::from("hello");
    let s2 = takes_ownership(s1);
    println!("{}", s2);
}

fn takes_ownership(some_string: String) -> String {
    println!("{}", some_string);
    return some_string; // return the string ownership back to the original main fn
}

// alternative re-assign

fn main() {
    let mut s1 = String::from("hello");
    s1 = takes_ownership(s1);
    println!("{}", s1);
}

fn takes_ownership(some_string: String) -> String {
    println!("{}", some_string);
    return some_string; // return the string ownership back to the original main fn
}

// REFERENCE

fn main() {
    let s1 = String::from("Hello");
    let s2 = &s1;

    println!("{}", s2);
    println!("{}", s1);    // This is valid, The first pointer wasn't invalidated
}

// BORROWING

fn main() {
    let my_string = String::from("Hello, Rust!");
    takes_ownership(&my_string);  // Pass a reference to my_string
    println!("{}", my_string);    // This is valid because ownership was not transferred
}

fn takes_ownership(some_string: &String) {
    println!("{}", some_string);  // some_string is borrowed and not moved
}

// muteable borrow
// cannot borrow to mutiple now

fn main() {
    let mut s1 = String::from("Hello");
    update_word(&mut s1);
    println!("{}", s1);
}

fn update_word(word: &mut String) {
    word.push_str(" World");
}

// multiple muteable borrow -- DOESNT COMPILE

fn main() {
    let mut s1 = String::from("Hello");
    let s2 = &mut s1;
    update_word(&mut s1);
    println!("{}", s1);
    println!("{}", s2);
}

fn update_word(word: &mut String) {
    word.push_str(" World");
}

use std::io;

fn main() {
    println!("Guess the number!");
    println!("Please input your guess.");

    let mut guess = String::new();

    io::stdin()
        .read_line(&mut guess)
        .expect("Failed to read line");

    println!("You guessed: {}", guess);
}

use std::io;
use rand::Rng;

fn main() {
    println!("Guess the number!");
    let secret_number = rand::thread_rng().gen_range(1..=100);

    println!("The secret number is: {secret_number}");
    println!("Please input your guess.");

    let mut guess = String::new();
    io::stdin()
        .read_line(&mut guess)
        .expect("Failed to read line");

    println!("You guessed: {guess}");
}

fn main() {
    let num: u8 = 9;
    println!("{}", is_even(num));
}

fn is_even(num: u8) -> bool {
    if num % 2 == 0 {
        return true;
    }
    false
}

// DOES NOT COMPILE
fn main() {
    let s1 = String::from("hello");
    let s2 = s1;
    println!("{}", s1);
}

// DOES COMPILE
fn main() {
    let s1 = String::from("hello");
    let _s2 = s1.clone();
    println!("{}", s1);
}

// DOES COMPILE
fn main() {
    let s1 = String::from("hello");
    let s2 = takes_ownership(s1);
    println!("{}", s2);
}

fn takes_ownership(some_string: String) -> String {
    println!("{}", some_string);
    return some_string; // return the string ownership back to the original main fn
}

// DOES COMPILE
fn main() {
    let mut s1 = String::from("hello");
    s1 = takes_ownership(s1);
    println!("{}", s2);
}

fn takes_ownership(some_string: String) -> String {
    println!("{}", some_string);
    return some_string; // return the string ownership back to the original main fn
}

// Define an enum called Shape
enum Shape {
    Circle(f64),         // Variant with associated data (radius)
    Square(f64),         // Variant with associated data (side length)
    Rectangle(f64, f64), // Variant with associated data (width, height)
}

// Function to calculate area based on the shape
fn calculate_area(shape: Shape) -> f64 {
    match shape {
        Shape::Circle(radius) => std::f64::consts::PI * radius * radius,
        Shape::Square(side_length) => side_length * side_length,
        Shape::Rectangle(width, height) => width * height,
    }
}

fn main() {
    // Create instances of different shapes
    let circle = Shape::Circle(5.0);
    let square = Shape::Square(4.0);
    let rectangle = Shape::Rectangle(3.0, 6.0);

    // Calculate and print the areas
    println!("Area of circle: {}", calculate_area(circle));
    println!("Area of square: {}", calculate_area(square));
    println!("Area of rectangle: {}", calculate_area(rectangle));
}

fn main() {
    let sudan = String::from("sudan chapagain");
    let result = count_chars(sudan);
    println!("the length is {}", result);
}

fn count_chars(sudan: String) -> usize {
    sudan.chars().count()
}

// split name
let mut first_name = String::from("");
for c in str.chars() {
    if c == ' ' {
        break
    }
    first_name.push(c);
}
return first_name;

struct User {
    age: u8,
    name: String,
    email: String,
    active: bool,
}

fn main() {
    let name = String::from("Sudan Chapagain");
    let age = 20;
    let active = true;
    let email = String::from("hello@me.com");
    let me = User {
        age: age,
        name: name,
        email: email,
        active: active,
    };

    println!("{} {} {} {}", me.age, me.name, me.email, me.active);
}

// structs can be implemented

struct Rect {
    width: u32,
    height: u32,
}

impl Rect {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

fn main() {
    let rect = Rect {
        width: 30,
        height: 50,
    };
    print!("The area of the rectangle is {}", rect.area());
}

/*
- Convert temperatures between Fahrenheit and Celsius.
- Generate the nth Fibonacci number.
- Print the lyrics to the Christmas carol “The Twelve Days of Christmas,” taking advantage of the repetition in the song.
*/
use std::io;

fn main(){
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Failed to read line");

    let a: i32 = input.trim().parse().expect("Please enter a valid integer");
    let b: i32 = (a * (9/5)) + 32;
    println!("{}C is {}F", a, b);
}



// take in vector of tuple with string and i32
// use that to construct a hashmap where string is key i32 is value

use std::collections::HashMap;

fn group_values_by_key(input: Vec<(String, i32)>) -> HashMap<String, i32> {
    let mut new = HashMap::new();

    for (k, v) in input {
        new.insert(k,v);
    }

    new
}

fn main(){
    let pairs: Vec<(String, i32)> = vec![
        (String::from("Sudan"), 20),
        (String::from("Someonelse"), 31)
    ];

    let grouped_pairs = group_values_by_key(pairs);
    for (key, value) in grouped_pairs {
        println!("{}: {:?}",key,value);
    }
}

//////////////////////////////////////////////////////////////////////////////

pub fn calculate_area() -> u32 {
    let height: u32 = 30;
    let width: u32 = 30;
    prints_values(width, height);

    return height * width;
}

pub fn prints_values(width: u32, height: u32) {
    println!("The width is: {}", width);
    println!("The height is: {}", height);
}

//////////////////////////////////////////////////////////////////////////////

/*
    mutability & reference
*/

pub fn mutating_variables() -> String {
    let text = String::from("hello");
    mutates_value(&mut text);
    return text;
}

pub fn mutates_value(value: &mut String) {
    *value = String::from("bye")
}

//////////////////////////////////////////////////////////////////////////////

/*
    constants
    - must be declared outside of functions
   - must be type annotated
*/

pub const MAX_SIZE: i32 = 100;

pub fn main() -> i32 {
    return MAX_SIZE;
}

/////////////////////////////////////////////////////////////////////////////

/*
    basic types
*/

pub fn data_types() -> (u8, f64, bool, char) {
    let a: u8 = 42;
    let b: f64 = 3.14;
    let c: bool = false;
    let d: char = 'a';
    return (a, b, c, d);
}

/////////////////////////////////////////////////////////////////////////////

/*
    The as keyword in Rust is used for casting between different types. It
    is commonly used to convert between numerical types. The as keyword
    is used to convert a value from one type to another, as long as the
    conversion is valid and does not result in data loss or overflow.
*/

pub fn numerical_type_conversion(n: i32) -> u32 {
    return n as u32;
}

// let result = numerical_type_conversion(42i32);
// assert_eq!(result, 42u32);

/////////////////////////////////////////////////////////////////////////////

/*
    mathematical operations
*/

pub fn math_operations(a: i32, b: i32) -> (i32, i32, i32, i32) {
    let n1: i32 = a + b;
    let n2: i32 = b - b;
    let n3: i32 = a * b;
    let n4: i32 = a / b;
    return (n1, n2, n3, n4);
}

// let a = 10;
// let b = 5;
// let result = math_operations(a, b);
// assert_eq!(result, (15, 5, 50, 2));

/////////////////////////////////////////////////////////////////////////////

/*
    Arrays are a fundamental data structure in Rust that allow you to
    store a fixed-size collection of elements of the same type. A common
    operation is to calculate the sum of all elements in an array.
*/

pub fn sum_array(arr: &[i32]) -> i32 {
    let result = arr.iter().sum();
    return result;
}

// let arr = [1, 2, 3, 4, 5];

// let sum = sum_array(&arr);
// assert_eq!(sum, 15); // 1 + 2 + 3 + 4 + 5 = 15

/////////////////////////////////////////////////////////////////////////////

/*
    Tuples are a simple and versatile data structure in Rust that allow
    you to group multiple values of different types into a single compound
    value. They are particularly useful for returning multiple values from
    a function.
*/

pub fn create_tuple(a: i32, b: f64, c: &str) -> (i32, f64, String) {
    c = c.to_string();
    return (a, b, c);
}

// let result = create_tuple(42, 3.14, "hello");
// assert_eq!(result, (42, 3.14, String::from("hello")));

/////////////////////////////////////////////////////////////////////////////

/*
    Unit type
    In Rust, the unit type `()` is a type that has exactly one value, also
    written as `()`. It is used to indicate the absence of a meaningful
    value and is often seen in functions that do not return a value.
*/

pub fn print_message() -> () {
    println!("Hello, Rust!")
}

// let result = print_message();
// assert_eq!(result, ());

/////////////////////////////////////////////////////////////////////////////

/*
    Functions are a fundamental building block in Rust, as in any programming
    language. They allow you to encapsulate logic and reuse code, making
    your programs more modular and easier to understand. In this challenge,
    you will define and implement a series of simple functions that perform
    basic operations.
*/

pub fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

pub fn subtract(a: i32, b: i32) -> i32 {
    return a - b;
}

pub fn multiply(a: i32, b: i32) -> i32 {
    return a * b;
}

// let result = add(2, 3);
// assert_eq!(result, 5);

// let result = subtract(5, 3);
// assert_eq!(result, 2);

// let result = multiply(2, 3);
// assert_eq!(result, 6);

/////////////////////////////////////////////////////////////////////////////

/*
    control flow
*/

pub fn check_number_sign(number: i32) -> String {
    if number > 0 {
        return "positive".to_string();
    } else if number < 0 {
        return "negative".to_string();
    } else {
        return "zero".to_string();
    }
}

// let result = check_number_sign(10);
// assert_eq!(result, "positive");

// let result = check_number_sign(-5);
// assert_eq!(result, "negative");

// let result = check_number_sign(0);
// assert_eq!(result, "zero");

/////////////////////////////////////////////////////////////////////////////

/*
    Ownership & Borrowing
    =====================

    Ownership:
    ----------

    In Rust, each value has a variable that's called its owner. There can only
    be one owner at a time, and when the owner goes out of scope, the
    value is dropped. Here are the basic rules of ownership:

    1. Each value in Rust has a variable that's called its owner.

    2. There can only be one owner at a time.

    3. When the owner goes out of scope, the value will be dropped (no
       longer valid).

    4. You can have either one mutable reference or any number of immutable
       references, but not both simultaneously.

    ```rust
    {
        // s is the owner of the String
        let s = String::from("hello");
    } // s goes out of scope and "hello" is dropped
    ```

    Borrowing:
    ----------

    Rust allows you to create references to a value, which lets you access
    it without taking ownership. This is called borrowing. Borrowing can
    be immutable or mutable.

    You can create multiple immutable references to a value, but you cannot
    have a mutable reference while immutable references exist. This allows
    you to read from the value without changing it.

    ```rust
    fn main() {
        let s1 = String::from("hello");

        // Borrow s1 as immutable
        let len = calculate_length(&s1);
        println!("The length of '{}' is {}.", s1, len);
    }

    // s is an immutable reference to a String
    fn calculate_length(s: &String) -> usize {
        s.len()
    }
    ```

    In the example above, `&s1` creates an immutable reference to `s1`. This
    means that `calculate_length` borrows `s1` but does not take ownership
    of it. The `&` symbol is used to denote a reference in Rust. This
    allows the function to access the value without taking ownership,
    which means `s1` can still be used after the function call.

    Similarly, in the function signature `fn calculate_length(s: &String)
    -> usize`, `&String` indicates that the parameter `s` is an immutable
    reference to a `String`. This allows the function to read from the
    String without modifying it or taking ownership.
*/

pub fn calculate_length(s: &String) -> usize {
    return s.len();
}

// let s1 = String::from("hello");
// let len = calculate_length(&s1);
// assert_eq!(len, 5);

/*
    Mutable references allow you to modify the value you are borrowing. You
    can only have one mutable reference to a particular piece of data in
    a particular scope. This prevents data races at compile time.

    In Rust, `&mut` is used to create a mutable reference. This means
    that you can borrow a value and make changes to it without taking
    ownership. However, Rust enforces that there can only be one mutable
    reference to a particular piece of data in a particular scope. This
    ensures that no data races occur, as only one part of the code can
    modify the data at a time.

    ```rs
    fn main() {
        let mut s = String::from("hello");

        change(&mut s); // borrow s as mutable
        println!("{}", s);
    }

    fn change(some_string: &mut String) {
        some_string.push_str(", world");
    }
    ```
*/

pub fn append_suffix(s: &mut String, suffix: &str) {
    s.push_str(suffix);
}

// let mut s2 = String::from("hello");
// append_suffix(&mut s2, " world");
// assert_eq!(s2, "hello world");

pub fn calculate_and_modify() -> (String, usize) {
    let mut s = String::from("hello");
    let length = s.len();

    s.push_str(", world");
    let s2 = &s;
    println!("{}", s2);

    (s, length)
}

/////////////////////////////////////////////////////////////////////////////

pub fn count_characters(s: &str) -> u32 {
    return s.chars().count();
}

// let s = "hello";
// let result = character_counting_string(s);
// assert_eq!(result, 5);

/////////////////////////////////////////////////////////////////////////////

// Consider this struct

struct User<a'> {
    first_name: &'a str,
    last_name: &'a str,
}

// What is we want to create a User<'static> that fully owns its data?

// Now consider this struct

struct User {
    first_name: String,
    last_name: String,
}

// What if we want to avoid unnecessary allocations while still allowing the strings to be mutable?

// Combined type that does not require allocation

use std::borrow::cow;

struct User<'a> {
    first_name: Cow<'a, str>,
    last_name: Cow<'a, str>,
}

impl<'a> User<'a> {
    // 'find_by' returns an owned User
    pub fn find_by(first_name: &str, last_name: &str) -> User<'static> {
        // Deserialized and must be owned
        User {
            first_name: Cow::Owned(first_name.to_owned())
            last_name: Cow::Owned(last_name.to_owned())
        }
    }

    // 'get_user' can accept borrowed or owned identifiers due to cow
    pub fn get_user(user: &user) {
        // Serialize or use references as needed, no allocation required
        println!(
            "Getting user: first name = '{entity.first_name}', last name '{entity.last_name}'"
        );
    }
}

/////////////////////////////////////////////////////////////////////////////

#![allow(dead_code)]

#[no_mangle]
pub extern "C" fn flatmap_sum(xss: &[Vec<i32>]) -> i32 {
    xss.iter().flat_map(|x| xs.iter()).sum()
}

// the generates assembly for `flatmap_sum` hot inner loop:
/*
.LBB01_11:
    vpaddd ymm0, ymm0, ymmword ptr [r8 + 4*rax]
    vpaddd ymm1, ymm1, ymmword ptr [r8 + 4*rax + 32]
    vpaddd ymm2, ymm2, ymmword ptr [r8 + 4*rax + 64]
    vpaddd ymm3, ymm3, ymmword ptr [r8 + 4*rax + 96]
    add    rax, 32
    cmp    r9, rax
    jne    .LBB0_11
    ; horizontal reduce ymm0..ymm3 down to eax
*/

// and now the manually crafted loop using nested for:

#[no_mangle]
pub extern "C" fn nested_sum(xss: &[Vec<i32>]) -> i32 {
    let mut acc = 0;
    for xs in xss {
        for &x in xs {
            acc += x;
        }
    }
    acc
}

// and the corresponding generated assembly for hot inner loop...
/*
.LBB0_16:
    vpaddd ymm0, ymm0, ymmword ptr [r9 + 4*rax]
    vpaddd ymm1, ymm1, ymmword ptr [r9 + 4*rax + 32]
    vpaddd ymm2, ymm2, ymmword ptr [r9 + 4*rax + 64]
    vpaddd ymm3, ymm3, ymmword ptr [r9 + 4*rax + 96]
    add    rax, 32
    cmp    r11, rax
    jne    .LBB0_16
    ; same style horizontal reduction to eax
*/

// Basically Identical, except for some bookkeeping differences ..

// - Base pointer register is r8 in one, r9 in the other.
// - Loop limit register is r9 vs r11.
// - But structurally: identical AVX2 vectorized summation over chunks of 32 i32s.

/////////////////////////////////////////////////////////////////////////////

/*

repr(C): languages like C and C++ have predictable, well-defined memory
layouts that are not subject to automatic rearrangement by the compiler.

*/

#[repr(C)]
struct Foo {
    tiny: bool,
    normal: u32,
    small: u8,
    long: u64,
    short: u16,
}

/*
b = byte

[ [1b tiny] [3b padding*] [4b normal] [7b padding*] [8b long] [2b short] [6b padding*] ]
[                                 32 bytes                                             ]

*Padding: bytes with an indeterminate value that are ignored in user code

*/

//===============================================================================

/*

repr(Rust): The rust compiler opimizes the memory layout to minimize padding
and ensure proper field alignment, which can reduce the overall size of a
struct and improve cache efficiency.

*/

#[repr(Rust)]
struct Foo {
    tiny: bool,
    normal: u32,
    small: u8,
    long: u64,
    short: u16,
}

/*

b = byte

[ [8b long] [4b normal] [2b short] [1b small] [1b tiny] ]
[ [            8b            ] [       8b             ] ]
|<-------------------------16 bytes-------------------->|

*/

/////////////////////////////////////////////////////////////////////////////
