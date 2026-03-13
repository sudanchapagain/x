# Go Programming Language

- <https://gobyexample.com/>
- Go memory model <https://go.dev/ref/mem>
- Memory Management in Go: The good, bad and ugly <https://www.youtube.com/watch?v=SKenR18NM04>
- GOLANG GRPC MICROSERVICES WITH MONGODB JWT AUTHENTICATION & KUBERNETES <https://www.youtube.com/watch?v=f6KG5eqOPFw>

> This is a modified version of existing resources, some of which may be
  restricted i.e. not free. It is only for personal use as notes, not for
  resale or profit.

Conceived in September 2007 by Robert Griesemer, Rob pike, and Ken Thompson, all
at Google, and was announced in November 2009. Go learns from C and others.

One major stream of influence comes from languages by Niklaus Wirth, beginning
with pascal. Modula-2 inspired the package concept. Oberon eliminated the
distinction between module interface files and module implementation files.
Oberon-2 influenced the syntax for packages, imports, and declarations,
particularly method declarations.

## Imports, Exports, Modules, Packages, & Workspaces

In Go, the `import` keyword is used to declare the packages that are used by the
code in the file. Go is strict about importing packages. It will not compile if
you import a package but don't use it. package is collection of Go files.

```txt
/client
/client/main.go
/client/calculate.go
/client/display.go
```

The `main` package should also contain a `main()` function which is a special
function that acts as the entry point of an executable program.

exports in packages work with following convention:

```go
package custom

var value int = 10 // Will not be exported
var Value int = 20 // Will be exported
```

Lower case identifiers will not be exported and will be private to the package
it's defined in. We can also alias our imports to avoid collisions. To alias an
import alias can be prepended to import path.

```go
package main

import (
  "fmt"
  abcd "example/custom"
)

func main() {
  fmt.Println(abcd.Value)
}
```

Module is a collection of Go packages.

```txt
/
/client/
/server/
/.../
```

Go modules were introduced in Go 1.11, which brings native support for versions
and modules. Earlier, we needed the `GO111MODULE=on` flag to turn on the
modules functionality when it was experimental. But now after Go 1.13 modules
mode is the default for all development.

**what is `GOPATH`?**

Well, `GOPATH` is a variable that defines the root of your workspace and it
contains the following folders:

- **src**: contains Go source code organized in a hierarchy.
- **pkg**: contains compiled package code.
- **bin**: contains compiled binaries and executables.

![gopath](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-I/modules/gopath.png)

`go.mod` is the file that defines the module's _module path_ and also the
import path used for the root directory, and its _dependency requirements_.

```go
module <name>

go <version>

require (
  ...
)
```

And if we want to add a new dependency, we will use `go install` command:

```bash
> go install github.com/rs/zerolog
```

We can list all the dependencies using `go list` command as follows:

```bash
> go list -m all
```

If the dependency is not used, we can simply remove it using `go mod tidy` command:

```bash
> go mod tidy
```

Vendoring is the act of making your own copy of the 3rd party packages your
project is using. Those copies are traditionally placed inside each project and
then saved in the project repository. This can be done through `go mod vendor`
command.

After the `go mod vendor` command is executed, a `vendor` directory will be
created.

```text
├── go.mod
├── go.sum
├── go.work
├── main.go
└── vendor
    ├── github.com
    │   └── rs
    │       └── zerolog
    │           └── ...
    └── modules.txt
```

Workspaces allow us to work with multiple modules simultaneously without having
to edit `go.mod` files for each module. Each module within a workspace is
treated as a root module when resolving dependencies. To understand this
better, let's start by creating a `hello` module.

```text
> mkdir workspaces && cd workspaces
> mkdir hello && cd hello
> go mod init hello
```

For demonstration purposes, I will add a simple `main.go` and install an
example package.

```go
package main

import (
  "fmt"

  "golang.org/x/example/stringutil"
)

func main() {
  result := stringutil.Reverse("Hello Workspace")
  fmt.Println(result)
}
```

```sh
> go get golang.org/x/example
go: downloading golang.org/x/example v0.0.0-20220412213650-2e68773dfca0
go: added golang.org/x/example v0.0.0-20220412213650-2e68773dfca0
```

And if we run this, we should see our output in reverse.

```sh
> go run main.go
ecapskroW olleH
```

This is great, but what if we want to modify the `stringutil` module that our
code depends on? Until now, we had to do it using the `replace` directive in
the `go.mod` file, but now let's see how we can use workspaces here. So, let's
create our workspace in the `workspaces` directory.

```sh
> go work init
```

This will create a `go.work` file.

```sh
> cat go.work
go 1.18
```

We will also add our `hello` module to the workspace.

```sh
> go work use ./hello
```

This should update the `go.work` file with a reference to our `hello` module.

```go
go 1.18

use ./hello
```

Now, let's download and modify the `stringutil` package and update the
`Reverse` function implementation.

```sh
> git clone https://go.googlesource.com/example
Cloning into 'example'...
remote: Total 204 (delta 39), reused 204 (delta 39)
Receiving objects: 100% (204/204), 467.53 KiB | 363.00 KiB/s, done.
Resolving deltas: 100% (39/39), done.
```

`example/stringutil/reverse.go`

```go
func Reverse(s string) string {
  return fmt.Sprintf("I can do whatever!! %s", s)
}
```

Finally, let's add `example` package to our workspace.

```sh
> go work use ./example
> cat go.work
go 1.18

use (
  ./example
  ./hello
)
```

Perfect, now if we run our `hello` module we will notice that the `Reverse`
function has been modified.

```sh
> go run hello
I can do whatever!! Hello Workspace
```

## Variables and Declarations

By default, Go assigns a zero value to variables. Integers are assigned `0`,
booleans `false`, strings `""` and so on.

Declaration without initialization:

```go
var foo string
```

Declaration with initialization:

```go
var foo string = "Go is awesome"
```

Multiple declarations:

```go
var foo, bar string = "Hello", "World"
// OR
var (
  foo string = "Hello"
  bar string  = "World"
)
```

Type is omitted but will be inferred:

```go
var foo = "What's my type?"
```

Shorthand declaration, here we omit `var` keyword and type is always implicit.
This is how we will see variables being declared most of the time. We also use
the `:=` for declaration plus assignment.

```go
foo := "Shorthand!"
```

_Note: Shorthand only works inside `function` bodies._ Like imports, Go won't
let you have unused variables.

## Types

The size of the generic `int` and `uint` types are platform-dependent.
This means it is 32-bits wide on a 32-bit system and 64-bits wide on a 64-bit
system.

```go
var i int = 404                     // Platform dependent
var i8 int8 = 127                   // -128 to 127
var i16 int16 = 32767               // -2^15 to 2^15 - 1
var i32 int32 = -2147483647         // -2^31 to 2^31 - 1
var i64 int64 = 9223372036854775807 // -2^63 to 2^63 - 1

var ui uint = 404                     // Platform dependent
var ui8 uint8 = 255                   // 0 to 255
var ui16 uint16 = 65535               // 0 to 2^16
var ui32 uint32 = 2147483647          // 0 to 2^32
var ui64 uint64 = 9223372036854775807 // 0 to 2^64
var uiptr uintptr                     // Integer representation of a memory address

var name string = "My name is Go"

var bio string = `I am statically typed.
                  I was designed at Google.`
var value bool = false
var isItTrue bool = true
```

There's also an unsigned integer pointer `uintptr` type, which is an integer
representation of a memory address. It is not recommended to use this, so we
don't have to worry about it.

Golang has two additional integer types called `byte` and `rune` that are
aliases for `uint8` and `int32` data types respectively.

```go
type byte = uint8
type rune = int32
```

_A `rune` represents a unicode code point._

```go
var b byte = 'a'
var r rune = '🍕'
```

Go has two floating point types `float32` and `float64`. Both type follows the
IEEE-754 standard. _The default type for floating point values is float64._

```go
var f32 float32 = 1.7812 // IEEE-754 32-bit
var f64 float64 = 3.1415 // IEEE-754 64-bit
```

Go provides several operators for performing operations on numeric types.

| Type                | Syntax                                                   |
| ------------------- | -------------------------------------------------------- |
| Arithmetic          | `+` `-` `*` `/` `%`                                      |
| Comparison          | `==` `!=` `<` `>` `<=` `>=`                              |
| Bitwise             | `&` `\|` `^` `<<` `>>`                                   |
| Increment/Decrement | `++` `--`                                                |
| Assignment          | `=` `+=` `-=` `*=` `/=` `%=` `<<=` `>>=` `&=` `\|=` `^=` |

There are 2 complex types in Go, `complex128` where both real and imaginary
parts are `float64` and `complex64` where real and imaginary parts are
`float32`. We can define complex numbers either using the built-in complex
function or as literals.

```go
var c1 complex128 = complex(10, 1)
var c2 complex64 = 12 + 4i
```

### Type Conversion

```go
i := 42
f := float64(i)
u := uint(f)

fmt.Printf("%T %T", f, u)
```

### Alias types

Alias types were introduced in Go 1.9. They allow developers to provide an
alternate name for an existing type and use it interchangeably with the
underlying type.

```go
package main

import "fmt"

type MyAlias = string

func main() {
  var str MyAlias = "I am an alias"

  fmt.Printf("%T - %s", str, str) // Output: string - I am an alias
}
```

### Defined types

Unlike alias types, defined types do not use an equals sign.

```go
package main

import "fmt"

type MyDefined string

func main() {
  var str MyDefined = "I am defined"

  fmt.Printf("%T - %s", str, str) // Output: main.MyDefined - I am defined
}
```

**the difference?**

defined types do more than just give a name to a type. It first defines a new
named type with an underlying type. However, this defined type is different
from any other type, including its underline type. Hence, it cannot be used
interchangeably with the underlying type like alias types.

```go
package main

import "fmt"

type MyAlias = string

type MyDefined string

func main() {
  var alias MyAlias
  var def MyDefined

  // ✅ Works
  var copy1 string = alias

  // ❌ Cannot use def (variable of type MyDefined) as string value in variable
  var copy2 string = def

  fmt.Println(copy1, copy2)
}
```

## String Formatting

String formatting or sometimes also known as templating.

```go
fmt.Print("What", "is", "your", "name?")
fmt.Print("My", "name", "is", "golang")
// Whatisyourname?Mynameisgolang
```

`Print` does not format anything, it simply takes a string and prints it.

`Println` which is the same as `Print` but it adds a new line at the end and
also inserts space between the arguments.

```go
fmt.Println("What", "is", "your", "name?")
fmt.Println("My", "name", "is", "golang")
// What is your name?
// My name is golang
```

`Printf` also known as _"Print Formatter"_, allows us to format numbers,
strings, booleans, and much more.

```go
name := "golang"
fmt.Println("What is your name?")
fmt.Printf("My name is %s", name)
// What is your name?
// My name is golang
```

`%s` is substituted with `name` variable. These are called _annotation verbs_
and they tell the function how to format the arguments. We can control things
like width, types, and precision with these and there are lots of them.
Here's a [cheatsheet](https://pkg.go.dev/fmt).

```go
percent := (7.0 / 9) * 100
fmt.Printf("%f", percent)
// 77.777778
```

For 2 point precision, we can do that as well by using `.2f`. For percentage
sign we can escape it with `%`

```go
percent := (7.0 / 9) * 100
fmt.Printf("%.2f %%", percent)
// 77.78 %
```

`Sprint`, `Sprintln`, and `Sprintf`.

Basically the same as the print functions, the only difference being they
return the string instead of printing it.

```go
s := fmt.Sprintf("hex:%x bin:%b", 10 ,10)
fmt.Println(s)
// hex:a bin:1010
```

`Sprintf` formats our integer as hex or binary and returns it as a string.

## Flow Control

If else does not require (parentheses)

```go
func main() {
  x := 10

  if x > 5 {
    fmt.Println("x is gt 5")
  } else if x > 10 {
    fmt.Println("x is gt 10")
  } else {
    fmt.Println("else case")
  }
}
```

We can also compact our if statements.

```go
func main() {
  if x := 10; x > 5 {
    fmt.Println("x is gt 5")
  }
}
```

Switch doesn't require breaks. For fallthroughs use fallthrough

```go
func main() {
  day := "monday"

  switch day {
  case "monday":
    fmt.Println("time to work!")
    fallthrough
  case "friday":
    fmt.Println("let's party")
  default:
    fmt.Println("browse memes")
  }
}
```

Switch also supports shorthand declaration like this.

```go
  switch day := "monday"; day {
  case "monday":
    fmt.Println("time to work!")
    fallthrough
  case "friday":
    fmt.Println("let's party")
  default:
    fmt.Println("browse memes")
  }
```

We can also use it without any condition, which is the same as `switch true`.

```go
x := 10

switch {
  case x > 5:
    fmt.Println("x is greater")
  default:
    fmt.Println("x is not greater")
}
```

## Loops

Go only has for loops. For loop, doesn't need any parenthesis `()` unlike other
languages.

```go
func main() {
  for i := 0; i < 10; i++ {
    fmt.Println(i)
  }
}
```

Go also supports both `break` and `continue` statements for loop control.

```go
func main() {
  for i := 0; i < 10; i++ {
    if i < 2 {
      continue
    }

    fmt.Println(i)

    if i > 5 {
      break
    }
  }

  fmt.Println("We broke out!")
}
```

Also, Init and post statements are optional.

```go
func main() {
  i := 0

  for ;i < 10; { // can be written as `for i < 10 {}`
    i += 1
  }
}
```

## Functions

```go
func log(message string) {}
func add(a int, b int) int {}
func power(name string) (int, bool) {}
```

We'd use the last one like so:

```go
value, exists := power("goku")
if exists == false {
  // handle this error case
}
```

To ignore a return value use underscore

```go
_, exists := power("goku")
if exists == false {
  // handle this error case
}
```

It is not just convention, `_`, the blank identifier, is special in that the
return value isn't actually assigned. This lets you use `_` over and over again
regardless of the returned type.

If parameters share the same type, we can use a shorter syntax:

```go
func add(a, b int) int {}
```

Being able to return multiple values is something you'll use often. You'll also
frequently use `_` to discard a value. Named return values and the slightly less
verbose parameter declaration aren't that common. Still, you'll run into all of
these sooner than later so it's important to know about them.

[Named returns](https://go.dev/tour/basics/7) is where return values can be
named and treated as their own variables.

```go
func myFunction(p1 string) (s string, i int) {
  s = fmt.Sprintf("%s function", p1)
  i = 10

  return
}
```

the above `return` statement without any arguments is also known as
_naked return_.

In Go functions are first class and we can use them as values.

```go
func myFunction() {
  fn := func() {
    fmt.Println("inside fn")
  }

  fn()
}
```

We can also simplify this by making `fn` an _anonymous function_.

```go
func myFunction() {
  func() {
    fmt.Println("inside fn")
  }()
}
```

_Note:_ the parenthesis at the end executes it.

---

A closure is a function value that references variables from outside its body.
Closures are lexically scoped, which means functions can access the values in
scope when defining the function.

```go
func myFunction() func(int) int {
  sum := 0

  return func(v int) int {
    sum += v

    return sum
  }
}

add := myFunction()
add(5)
fmt.Println(add(10))
```

We get a result of 15 as `sum` variable is _bound_ to the function.

---

Variadic functions, which are functions that can take zero or multiple
arguments using the `...` ellipses operator.

```go
func main() {
  sum := add(1, 2, 3, 5)
  fmt.Println(sum)
}

func add(values ...int) int {
  sum := 0

  for _, v := range values {
    sum += v
  }

  return sum
}
```

## Init block

In Go, `init` is a special lifecycle function that is executed before the
`main` function. Similar to `main`, the `init` function does not take any
arguments nor returns any value.

```go
package main

import "fmt"

func init() {
  fmt.Println("Before main!")
}

func main() {
  fmt.Println("Running main")
}

// Before main!
// Running main
```

Unlike `main`, there can be more than one `init` function in single or multiple
files. For multiple `init` in a single file, their processing is done in the
order of their declaration, while `init` functions declared in multiple files
are processed according to the lexicographic filename order.

```go
package main

import "fmt"

func init() {
  fmt.Println("Before main!")
}

func init() {
  fmt.Println("Hello again?")
}

func main() {
  fmt.Println("Running main")
}
// Before main!
// Hello again?
// Running main
```

The `init` function is optional and is particularly used for any global setup
which might be essential for our program, such as establishing a database
connection, fetching configuration files, setting up environment variables,
etc.

## Defer Statement

It lets us postpones the execution of a function until the surrounding function
returns.

```go
func main() {
  defer fmt.Println("I am finished")
  fmt.Println("Doing some work...")
}
```

When multiple defer statements are used we need to be known about _defer
stack_.

```go
func main() {
  defer fmt.Println("I am finished")
  defer fmt.Println("Are you?")

  fmt.Println("Doing some work...")
}

// Doing some work...
// Are you?
// I am finished
```

defer statements are stacked and executed in a _last in first out_ manner.

## Pointers

```go
var x *T
```

Where `T` is the type such as `int`, `string`, `float`, and so on.

```go
package main

import "fmt"

func main() {
  var p *int

  fmt.Println(p)
}
// nil
```

what is `nil`? nil is a predeclared identifier in Go that represents zero value
for pointers, interfaces, channels, maps, and slices.

```go
package main

import "fmt"

func main() {
  a := 10

  var p *int = &a

  fmt.Println("address:", p)
}
// 0xc0000b8000
```

### De-referencing

We can also use the `*` asterisk operator to retrieve the value stored in the
variable that the pointer points to. This is also called **dereferencing**.

For example, we can access the value of the variable `a` through the pointer
`p` using that `*` asterisk operator.

```go
package main

import "fmt"

func main() {
  a := 10

  var p *int = &a

  fmt.Println("address:", p)
  fmt.Println("value:", *p)
}
// address: 0xc000018030
// value: 10
```

We can not only access it but change it as well through the pointer.

```go
package main

import "fmt"

func main() {
  a := 10

  var p *int = &a

  fmt.Println("before", a)
  fmt.Println("address:", p)

  *p = 20
  fmt.Println("after:", a)
}
// before 10
// address: 0xc000192000
// after: 20
```

### Pointers as function args

Pointers can also be used as arguments for a function when we need to pass some
data by reference.

```go
myFunction(&a)

func myFunction(ptr *int) {}
```

### New function

There's also another way to initialize a pointer. We can use the `new` function
which takes a type as an argument, allocates enough memory to accommodate a
value of that type, and returns a pointer to it.

```go
package main

import "fmt"

func main() {
  p := new(int)
  *p = 100

  fmt.Println("value", *p)
  fmt.Println("address", p)
}
// value 100
// address 0xc000018030
```

### Pointer to a Pointer

```go
package main

import "fmt"

func main() {
  p := new(int)
  *p = 100

  p1 := &p

  fmt.Println("P value", *p, " address", p)
  fmt.Println("P1 value", *p1, " address", p)

  fmt.Println("Dereferenced value", **p1)
}
// P value 100  address 0xc0000be000
// P1 value 0xc0000be000  address 0xc0000be000
// Dereferenced value 100
```

Also, it is important to know that pointers in Go do not support pointer
arithmetic like in C or C++.

```go
  p1 := p * 2 // Compiler Error: invalid operation
```

However, we can compare two pointers of the same type for equality using a
`==` operator.

```go
p := &a
p1 := &a

fmt.Println(p == p1)
```

## Structures

Go has structures which has be associated with methods. Go also supports a
simple but effective form of composition. (It's worth pointing out that
_composition over inheritance_ is an old battle cry and Go is the among the
languages that takes a firm stand on the issue.)

We use the `type` keyword to introduce a new type, followed by the name and
then the `struct` keyword to indicate that we're defining a struct.

```go
type Person struct {
  FirstName string
  LastName  string
  Age       int
}
```

And, if the fields have the same type, we can collapse them as well.

```go
type Person struct {
  FirstName, LastName string
  Age                 int
}
```

The simplest way to create a value of our structure is:

```go
sudan := Person {
  FirstName: "Sudan",
  LastName: "Chapagain",
  Age: 21,
}
```

_Note:_ The trailing `,` in the above structure is required. Without it, the
compiler will give an error.

We don't have to set all or even any of the fields. Both of these are valid:

```go
type Saiyan struct {
    Name string,
    Power int,
}

var goky Saiyan // initialization

goku := Saiyan{} // init as struct literal
// or
goku := Saiyan{Name: "Goku"}
goku.Power = 9000
```

Just like unassigned variables have a zero value, so do fields. Furthermore,
you can skip the field name and rely on the order of the field declarations
(though for the sake of clarity, you should only do this for structures with
few fields). Also, we will need to provide all the values during the
initialization or it will fail.

```go
goku := Saiyan{"Goku", 9000}
```

Many times though, we don't want a variable that is directly associated with
our value but rather a variable that has a pointer to our value.

```go
func main() {
  goku := Saiyan{"Goku", 9000}
  Super(goku)
  fmt.Println(goku.Power)
}

func Super(s Saiyan) {
  s.Power += 10000
}
```

The answer is 9000, not 19000. Why? Because `Super` made changes to a copy of
our original `goku` value and thus, changes made in `Super` weren't reflected
in the caller. To make this work as you probably expect, we need to pass a
pointer to our value.

```go
func main() {
  var ptr = Saiyan { "Sudan", 10 }
  fmt.Println((*ptr).FirstName)
  fmt.Println(ptr.FirstName)
}
```

Both statements are equal as in Go we don't need to explicitly dereference the
pointer. We can also use the built-in `new` function.

```go
func main() {
  sai := new(Person)
  sai.Name = "sudan"
}
```

Two structs are equal if all their corresponding fields are equal as well.

```go
func main() {
  var p1 = Person{"a", "b", 20}
  var p2 = Person{"a", "b", 20}

  fmt.Println(p1 == p2)
}
```

Pointer, Address, and Functions

```go
func main() {
  goku := &Saiyan{"Goku", 9000}
  Super(goku)
  fmt.Println(goku.Power)
}

func Super(s *Saiyan) {
  s.Power += 10000
}
```

We made two changes. The first is the use of the `&` operator (_address of_
operator) to get the address of our value. Next, we changed the type of
parameter `Super` expects. It used to expect a value of type `Saiyan` but now
expects an address of type `*Saiyan`, where `*X` means _pointer to value of
type X_. There's obviously some relation between the types `Saiyan` and
`*Saiyan`, but they are two distinct types.

Note that we're still passing a copy of `goku's` value to `Super` it just so
happens that `goku's` value has become an address. That copy is the same
address as the original, which is what that indirection buys us. Think of it as
copying the directions to a restaurant. What you have is a copy, but it still
points to the same restaurant as the original.

We can prove that it's a copy by trying to change where it points to (not
something you'd likely want to actually do):

```go
func main() {
  goku := &Saiyan{"Goku", 9000}
  Super(goku)
  fmt.Println(goku.Power)
}

func Super(s *Saiyan) {
  s = &Saiyan{"Gohan", 1000}
}
```

It should also be obvious that copying a pointer is going to be cheaper than
copying a complex structure. On a 64-bit machine, a pointer is 64 bits large.
If we have a structure with many fields, creating copies can be expensive. The
real value of pointers though is that they let you share values. Do we want
`Super` to alter a copy of `goku` or alter the shared `goku` value itself?

### Exported fields

Same as the rules for variables and functions, if a struct field is declared
with a lower case identifier, it will not be exported and only be visible to
the package it is defined in.

```go
type Person struct {
  FirstName, LastName  string
  Age                  int
  zipCode              string
}
```

So, the `zipCode` field won't be exported. Also, the same goes for the `Person`
struct, if we rename it as `person`, it won't be exported as well.

```go
type person struct {
  FirstName, LastName  string
  Age                  int
  zipCode              string
}
```

### Embedding and composition

Since, Go doesn't necessarily support inheritance, but we can do something
similar with embedding.

```go
type Person struct {
  FirstName, LastName  string
  Age                  int
}

type SuperHero struct {
  Person
  Alias string
}
```

So, our new struct will have all the properties of the original struct. And it
should behave the same as our normal struct.

```go
func main() {
  s := SuperHero{}

  s.FirstName = "Bruce"
  s.LastName = "Wayne"
  s.Age = 40
  s.Alias = "batman"

  fmt.Println(s)
}
```

However, this is usually not recommended and in most cases, composition is
preferred. So rather than embedding, we will just define it as a normal field.
i.e. the act of including one structure into another. In some languages, this
is called a trait or a mixin. Languages that don't have an explicit composition
mechanism can always do it the long way.

```go
type Person struct {
  FirstName, LastName  string
  Age                  int
}

type SuperHero struct {
  Person Person
  Alias  string
}
```

Hence, we can rewrite our example with composition as well.

```go
func main() {
  p := Person{"Bruce", "Wayne", 40}
  s := SuperHero{p, "batman"}

  fmt.Println(s)
}
```

In Java, there's the possibility to extend structures with _inheritance_ but,
in a scenario where this is not an option, a mixin would be written like this.

```java
public class Person {
  private String name;

  public String getName() {
    return this.name;
  }
}

public class Saiyan {
  // Saiyan is said to have a person
  private Person person;

  // we forward the call to person
  public String getName() {
    return this.person.getName();
  }
  // ...
}
```

This can get pretty tedious. Every method of `Person` needs to be duplicated in
`Saiyan`. Go avoids this tediousness:

```go
type Person struct {
  Name string
}

func (p *Person) Introduce() {
  fmt.Printf("Hi, I'm %s\n", p.Name)
}

type Saiyan struct {
  *Person
  Power int
}

// and to use it:
goku := &Saiyan{
  Person: &Person{"Goku"},
  Power: 9001,
}
goku.Introduce()
```

The `Saiyan` structure has a field of type `*Person`. Because we didn't give it
an explicit field name, we can implicitly access the fields and functions of
the composed type. However, the Go compiler _did_ give it a field name,
consider the perfectly valid:

```go
goku := &Saiyan{
  Person: &Person{"Goku"},
}
fmt.Println(goku.Name)
fmt.Println(goku.Person.Name)
```

Both of the above will print "Goku".

### Struct tags

A struct tag is just a tag that allows us to attach metadata information to
the field which can be used for custom behavior using the `reflect` package.

Let's learn how we can define struct tags.

```go
type Animal struct {
  Name    string `key:"value1"`
  Age     int    `key:"value2"`
}
```

You will often find tags in encoding packages, such as XML, JSON, YAML, ORMs,
and Configuration management. Here's a tags example for the JSON encoder.

```go
type Animal struct {
  Name    string `json:"name"`
  Age     int    `json:"age"`
}
```

### Properties

Structs are value types. When we assign one `struct` variable to another, a
new copy of the `struct` is created and assigned. Similarly, as discussed
before when we pass a `struct` to another function, the function gets its own
copy of the `struct`.

```go
package main

import "fmt"

type Point struct {
  X, Y float64
}

func main() {
  p1 := Point{1, 2}
  p2 := p1 // Copy of p1 is assigned to p2

  p2.X = 2

  fmt.Println(p1) // Output: {1 2}
  fmt.Println(p2) // Output: {2 2}
}
```

Empty struct occupies zero bytes of storage.

```go
package main

import (
  "fmt"
  "unsafe"
)

func main() {
  var s struct{}
  fmt.Println(unsafe.Sizeof(s)) // Output: 0
}
```

Go is smart enough to interpret our function call correctly, and hence, pointer
receiver method calls are just syntactic sugar provided by Go for convenience.

```go
(&c).UpdateName(...)
```

We can omit the variable part of the receiver as well if we're not using it.

```go
func (Car) UpdateName(...) {}
```

Methods are not limited to structs but can also be used with non-struct types
as well.

```go
package main

import "fmt"

type MyInt int

func (i MyInt) isGreater(value int) bool {
  return i > MyInt(value)
}

func main() {
  i := MyInt(10)

  fmt.Println(i.isGreater(5))
}
```

### Methods

Methods, sometimes also known as function receivers. A method is nothing but a
function with a special _receiver_ argument. Let's see how we can declare
methods.

```go
func (variable T) Name(params) (returnTypes) {}
```

The _receiver_ argument has a name and a type. It appears between the `func`
keyword and the method name. For example, let's define a `Car` struct.

```go
type Car struct {
  Name string
  Year int
}
```

Now, let us define a method like `IsLatest` which will tell us if a car was
manufactured within the last 5 years.

```go
func (c Car) IsLatest() bool {
  return c.Year >= 2017
}
```

As you can see, we can access the instance of `Car` using the receiver variable
`c`. I like to think of it as `this` keyword from the object-oriented world.

Now we should be able to call this method after we initialize our struct, just
like we do with classes in other languages.

```go
func main() {
  c := Car{"Tesla", 2021}

  fmt.Println("IsLatest", c.IsLatest())
}
```

#### Methods with Pointer receivers

All the examples that we saw previously had a value receiver.

With a value receiver, the method operates on a copy of the value passed to it.
Therefore, any modifications done to the receiver inside the methods are not
visible to the caller.

For example, let's make another method called `UpdateName` which will update
the name of the `Car`.

```go
func (c Car) UpdateName(name string) {
  c.Name = name
}

func main() {
  c := Car{"Tesla", 2021}

  c.UpdateName("Toyota")
  fmt.Println("Car:", c)
}
```

Seems like the name wasn't updated, so now let's switch our receiver to pointer
type and try again.

```go
func (c *Car) UpdateName(name string) {
  c.Name = name
}
```

As expected, methods with pointer receivers can modify the value to which the
receiver points. Such modifications are visible to the caller of the method as
well.

### Constructors

Structures don't have constructors. Instead, you create a function that returns
an instance of the desired type (like a factory):

```go
func NewSaiyan(name string, power int) *Saiyan {
  return &Saiyan{
    Name: name,
    Power: power,
  }
}
```

Our factory doesn't have to return a pointer; this is absolutely valid:

```go
func NewSaiyan(name string, power int) Saiyan {
  return Saiyan{
    Name: name,
    Power: power,
  }
}
```

### more on new function

`new` function which is used to allocate the memory required by a type. The
result of `new(X)` is the same as `&X{}`:

```go
goku := new(Saiyan)
// same as
goku := &Saiyan{}
```

Which one you use is up to you, but you'll find that most people prefer the
latter whenever they have fields to initialize, since it tends to be easier to
read:

```go
goku := new(Saiyan)
goku.Name = "goku"
goku.Power = 9001

//vs

goku := &Saiyan {
  Name: "goku",
  Power: 9000,
}
```

Whichever approach you choose, if you follow the factory pattern above, you can
shield the rest of your code from knowing and worrying about any of the
allocation details.

### Fields of a Structure

In the example that we've seen so far, `Saiyan` has two fields `Name` and
`Power` of types `string` and `int`, respectively. Fields can be of any type --
including other structures and types that we haven't explored yet such as
arrays, maps, interfaces and functions.

```go
type Saiyan struct {
  Name string
  Power int
  Father *Saiyan
}
```

which we'd initialize via:

```go
gohan := &Saiyan{
  Name: "Gohan",
  Power: 1000,
  Father: &Saiyan {
    Name: "Goku",
    Power: 9001,
    Father: nil,
  },
}
```

### Overloading

While overloading isn't specific to structures, it's worth addressing. Simply,
Go doesn't support overloading. For this reason, you'll see (and write) a lot
of functions that look like `Load`, `LoadById`, `LoadByName` and so on.

However, because implicit composition is really just a compiler trick, we can
"overwrite" the functions of a composed type. For example, our `Saiyan`
structure can have its own `Introduce` function:

```go
func (s *Saiyan) Introduce() {
  fmt.Printf("Hi, I'm %s. Ya!\n", s.Name)
}
```

The composed version is always available via `s.Person.Introduce()`.

### Anonymous struct

```go
// explicit
type MyStruct struct {
  Name string
}

func main() {
  var a = MyStruct{"Golang"}
}

// anon
func main() {
  var a = struct {
    Name string
  }{"Golang"}
}
```

## Maps, Arrays and Slices

In Go arrays are fixed. Declaring an array requires that we specify the size,
and once the size is specified, it cannot grow:

```go
// syntax
var a [n]T

// example
var scores [10]int
scores[0] = 339
```

The above array can hold up to 10 scores using indexes `scores[0]` through
`scores[9]`. Attempts to access an out of range index in the array will result
in a compiler or runtime error. We can initialize the array with values:

```go
// syntax
var a [n]T = [n]T{V1, V2, ... Vn}
// example
var arr = [4]int{1, 2, 3, 4}
// shorthand
scores := [4]int{9001, 9333, 212, 33}
```

We can use `len` to get the length of the array. `range` can be used to iterate
over it:

```go
for index, value := range scores { /*...*/ }
```

But the range keyword is quite versatile and can be used in multiple ways.

```go
for i, e := range arr {} // Normal usage of range
for _, e := range arr {} // Omit index with _ and use element
for i := range arr {} // Use index only
for range arr {} // Simply loop over the array
```

All the arrays that we created so far are one-dimensional. We can also create
multi-dimensional arrays in Go.

```go
func main() {
  arr := [2][4]int{
    {1, 2, 3, 4},
    {5, 6, 7, 8},
  }

  for i, e := range arr {
    fmt.Printf("Index: %d, Element: %d\n", i, e)
  }
}
```

We can also let the compiler infer the length of the array by using `...`
ellipses instead of the length.

```go
func main() {
  arr := [...][4]int{
    {1, 2, 3, 4},
    {5, 6, 7, 8},
  }

  for i, e := range arr {
    fmt.Printf("Index: %d, Element: %d\n", i, e)
  }
}
```

Arrays in Go are value types unlike other languages like C, C++, and Java where
arrays are reference types.

This means that when we assign an array to a new variable or pass an array to a
function, the entire array is copied.

So, if we make any changes to this copied array, the original array won't be
affected and will remain unchanged.

```go
package main

import "fmt"

func main() {
  var a = [7]string{"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"}
  var b = a // Copy of a is assigned to b

  b[0] = "Monday"

  fmt.Println(a) // Output: [Mon Tue Wed Thu Fri Sat Sun]
  fmt.Println(b) // Output: [Monday Tue Wed Thu Fri Sat Sun]
}
```

Arrays are efficient but rigid. We often don't know the number of elements
we'll be dealing with upfront. For this, we turn to slices.

In Go, you rarely, if ever, use arrays directly. Instead, you use slices. A
slice is a lightweight structure that wraps and represents a portion of an
array. There are a few ways to create a slice, and we'll go over when to use
which later on. The first is a slight variation on how we created an array:

```go
scores := []int{1,4,293,4,9}
```

Unlike the array declaration, our slice isn't declared with a length within the square brackets. To understand how the two are different, let's see another way to create a slice, using `make`:

```go
scores := make([]int, 10)
```

We use `make` instead of `new` because there's more to creating a slice than just allocating the memory (which is what `new` does). Specifically, we have to allocate the memory for the underlying array and also initialize the slice. Above, we initialize a slice with a length of 10 and a capacity of 10. The length is the size of the slice, the capacity is the size of the underlying array. Using `make` we can specify the two separately:

```go
scores := make([]int, 0, 10)
```

This creates a slice with a length of 0 but with a capacity of 10. (If you're paying attention, you'll note that `make` and `len` _are_ overloaded. Go is a language that, to the frustration of some, makes use of features which aren't exposed for developers to use.)

To better understand the interplay between length and capacity, let's look at some examples:

```go
func main() {
  scores := make([]int, 0, 10)
  scores[7] = 9033
  fmt.Println(scores)
}
```

Our first example crashes. Why? Because our slice has a length of 0. Yes, the underlying array has 10 elements, but we need to explicitly expand our slice in order to access those elements. One way to expand a slice is via `append`:

```go
func main() {
  scores := make([]int, 0, 10)
  scores = append(scores, 5)
  fmt.Println(scores) // prints [5]
}
```

But that changes the intent of our original code. Appending to a slice of length 0 will set the first element. For whatever reason, our crashing code wanted to set the element at index 7. To do this, we can re-slice our slice:

```go
func main() {
  scores := make([]int, 0, 10)
  scores = scores[0:8]
  scores[7] = 9033
  fmt.Println(scores)
}
```

How large can we resize a slice? Up to its capacity which, in this case, is 10. You might be thinking _this doesn't actually solve the fixed-length issue of arrays._ It turns out that `append` is pretty special. If the underlying array is full, it will create a new larger array and copy the values over (this is exactly how dynamic arrays work in PHP, Python, Ruby, JavaScript, ...). This is why, in the example above that used `append`, we had to re-assign the value returned by `append` to our `scores` variable: `append` might have created a new value if the original had no more space.

If I told you that Go grew arrays with a 2x algorithm, can you guess what the following will output?

```go
func main() {
  scores := make([]int, 0, 5)
  c := cap(scores)
  fmt.Println(c)

  for i := 0; i < 25; i++ {
    scores = append(scores, i)

    // if our capacity has changed,
    // Go had to grow our array to accommodate the new data
    if cap(scores) != c {
      c = cap(scores)
      fmt.Println(c)
    }
  }
}
```

The initial capacity of `scores` is 5. In order to hold 25 values, it'll have to be expanded 3 times with a capacity of 10, 20 and finally 40.

As a final example, consider:

```go
func main() {
  scores := make([]int, 5)
  scores = append(scores, 9332)
  fmt.Println(scores)
}
```

Here, the output is going to be `[0, 0, 0, 0, 0, 9332]`. Maybe you thought it would be `[9332, 0, 0, 0, 0]`? To a human, that might seem logical. To a compiler, you're telling it to append a value to a slice that already holds 5 values.

Ultimately, there are four common ways to initialize a slice:

```go
names := []string{"leto", "jessica", "paul"}
checks := make([]bool, 10)
var names []string
scores := make([]int, 0, 20)
```

When do you use which? The first one shouldn't need much of an explanation. You use this when you know the values that you want in the array ahead of time. The second one is useful when you'll be writing into specific indexes of a slice. For example:

```go
func extractPowers(saiyans []*Saiyan) []int {
  powers := make([]int, len(saiyans))
  for index, saiyan := range saiyans {
    powers[index] = saiyan.Power
  }
  return powers
}
```

The third version is a nil slice and is used in conjunction with `append`, when the number of elements is unknown. The last version lets us specify an initial capacity; useful if we have a general idea of how many elements we'll need. Even when you know the size, `append` can be used. It's largely a matter of preference:

```go
func extractPowers(saiyans []*Saiyan) []int {
  powers := make([]int, 0, len(saiyans))
  for _, saiyan := range saiyans {
    powers = append(powers, saiyan.Power)
  }
  return powers
}
```

Slices as wrappers to arrays is a powerful concept. Many languages have the concept of slicing an array. Both JavaScript and Ruby arrays have a `slice` method. You can also get a slice in Ruby by using `[START..END]` or in Python via `[START:END]`. However, in these languages, a slice is actually a new array with the values of the original copied over. If we take Ruby, what's the output of the following?

```ruby
scores = [1,2,3,4,5]
slice = scores[2..4]
slice[0] = 999
puts scores
```

The answer is `[1, 2, 3, 4, 5]`. That's because `slice` is a completely new array with copies of values. Now, consider the Go equivalent:

```go
scores := []int{1,2,3,4,5}
slice := scores[2:4]
slice[0] = 999
fmt.Println(scores)
```

The `[X:Y]` syntax creates a slice of `scores`, starting from index 2 up until (but not including) index 4. However, unlike the Ruby example above, the Go code will produce an output of `[1, 2, 999, 4, 5]`. This is because our `slice` is really just a window into `scores`.

This changes how you code. For example, a number of functions take a position parameter. In JavaScript, if we want to find the first space in a string (yes, slices work on strings too!) after the first five characters, we'd write:

```javascript
haystack = "the spice must flow";
console.log(haystack.indexOf(" ", 5));
```

In Go, we leverage slices:

```go
strings.Index(haystack[5:], " ")
```

We can see from the above example, that `[X:]` is shorthand for _from X to the end_ while `[:X]` is shorthand for _from the start up until X_. Unlike other languages, Go doesn't support negative values. If we want all of the values of a slice except the last, we do:

```go
scores := []int{1, 2, 3, 4, 5}
scores = scores[:len(scores)-1]
```

The above is the start of an efficient way to remove a value from an unsorted slice:

```go
func main() {
  scores := []int{1, 2, 3, 4, 5}
  scores = removeAtIndex(scores, 2)
  fmt.Println(scores) // [1 2 5 4]
}

// won't preserve order
func removeAtIndex(source []int, index int) []int {
  lastIndex := len(source) - 1
  //swap the last value and the value we want to remove
  source[index], source[lastIndex] = source[lastIndex], source[index]
  return source[:lastIndex]
}
```

Finally, now that we know about slices, we can look at another commonly used built-in function: `copy`. `copy` is one of those functions that highlights how slices change the way we code. Normally, a method that copies values from one array to another has 5 parameters: `source`, `sourceStart`, `count`, `destination` and `destinationStart`. With slices, we only need two:

```go
import (
  "fmt"
  "math/rand"
  "sort"
)

func main() {
  scores := make([]int, 100)
  for i := 0; i < 100; i++ {
    scores[i] = int(rand.Int31n(1000))
  }
  sort.Ints(scores)

  worst := make([]int, 5)
  copy(worst, scores[:5])
  fmt.Println(worst)
}
```

Take some time and play with the above code. Try variations. See what happens if you change copy to something like `copy(worst[2:4], scores[:5])`, or what if you try to copy more or less than `5` values into `worst`?

## Maps

Maps in Go are what other languages call hashtables or dictionaries. They work as you expect: you define a key and value, and can get, set and delete values from it. Maps, like slices, are created with the `make` function. Let's look at an example:

```go
func main() {
  lookup := make(map[string]int)
  lookup["goku"] = 9001
  power, exists := lookup["vegeta"]

  // prints 0, false
  // 0 is the default value for an integer
  fmt.Println(power, exists)
}
```

To get the number of keys, we use `len`. To remove a value based on its key, we use `delete`:

```go
// returns 1
total := len(lookup)

// has no return, can be called on a non-existing key
delete(lookup, "goku")
```

Maps grow dynamically. However, we can supply a second argument to `make` to set an initial size:

```go
lookup := make(map[string]int, 100)
```

If you have some idea of how many keys your map will have, defining an initial size can help with performance.

When you need a map as a field of a structure, you define it as:

```go
type Saiyan struct {
  Name string
  Friends map[string]*Saiyan
}
```

One way to initialize the above is via:

```go
goku := &Saiyan{
  Name: "Goku",
  Friends: make(map[string]*Saiyan),
}
goku.Friends["krillin"] = ... //todo load or create Krillin
```

There's yet another way to declare and initialize values in Go. Like `make`, this approach is specific to maps and arrays. We can declare as a composite literal:

```go
lookup := map[string]int{
  "goku": 9001,
  "gohan": 2044,
}
```

We can iterate over a map using a `for` loop combined with the `range` keyword:

```go
for key, value := range lookup {
  ...
}
```

Iteration over maps isn't ordered. Each iteration over a lookup will return the key value pair in a random order.

## Pointers versus Values

We finished Chapter 2 by looking at whether you should assign and pass pointers or values. We'll now have this same conversation with respect to array and map values. Which of these should you use?

```go
a := make([]Saiyan, 10)
//or
b := make([]*Saiyan, 10)
```

Many developers think that passing `b` to, or returning it from, a function is going to be more efficient. However, what's being passed/returned is a copy of the slice, which itself is a reference. So with respect to passing/returning the slice itself, there's no difference.

Where you will see a difference is when you modify the values of a slice or map. At this point, the same logic that we saw in Chapter 2 applies. So the decision on whether to define an array of pointers versus an array of values comes down to how you use the individual values, not how you use the array or map itself.

## Packages

To keep more complicated libraries and systems organized, we need to learn about packages. In Go, package names follow the directory structure of your Go workspace. If we were building a shopping system, we'd probably start with a package name "shopping" and put our source files in `$GOPATH/src/shopping/`.

We don't want to put everything inside this folder though. For example, maybe we want to isolate some database logic inside its own folder. To achieve this, we create a subfolder at `$GOPATH/src/shopping/db`. The package name of the files within this subfolder is simply `db`, but to access it from another package, including the `shopping` package, we need to import `shopping/db`.

In other words, when you name a package, via the `package` keyword, you provide a single value, not a complete hierarchy (e.g., "shopping" or "db"). When you import a package, you specify the complete path.

Let's try it. Inside your Go workspace's `src` folder (which we set up in Getting Started of the Introduction), create a new folder called `shopping` and a subfolder within it called `db`.

Inside of `shopping/db`, create a file called `db.go` and add the following code:

```go
package db

type Item struct {
  Price float64
}

func LoadItem(id int) *Item {
  return &Item{
    Price: 9.001,
  }
}
```

Notice that the name of the package is the same as the name of the folder. Also, obviously, we aren't actually accessing the database. We're just using this as an example to show how to organize code.

Now, create a file called `pricecheck.go` inside of the main `shopping` folder. Its content is:

```go
package shopping

import (
  "shopping/db"
)

func PriceCheck(itemId int) (float64, bool) {
  item := db.LoadItem(itemId)
  if item == nil {
    return 0, false
  }
  return item.Price, true
}
```

It's tempting to think that importing `shopping/db` is somehow special because we're inside the `shopping` package/folder already. In reality, you're importing `$GOPATH/src/shopping/db`, which means you could just as easily import `test/db` so long as you had a package named `db` inside of your workspace's `src/test` folder.

If you're building a package, you don't need anything more than what we've seen. To build an executable, you still need a `main`. The way I prefer to do this is to create a subfolder called `main` inside of `shopping` with a file called `main.go` and the following content:

```go
package main

import (
  "shopping"
  "fmt"
)

func main() {
  fmt.Println(shopping.PriceCheck(4343))
}
```

You can now run your code by going into your `shopping` project and typing:

```console
go run main/main.go
```

### Cyclical Imports

As you start writing more complex systems, you're bound to run into cyclical imports. This happens when package A imports package B but package B imports package A (either directly or indirectly through another package). This is something the compiler won't allow.

Let's change our shopping structure to cause the error.

Move the `Item` definition from `shopping/db/db.go` into `shopping/pricecheck.go`. Your `pricecheck.go` file should now look like:

```go
package shopping

import (
  "shopping/db"
)

type Item struct {
  Price float64
}

func PriceCheck(itemId int) (float64, bool) {
  item := db.LoadItem(itemId)
  if item == nil {
    return 0, false
  }
  return item.Price, true
}
```

If you try to run the code, you'll get a couple of errors from `db/db.go` about `Item` being undefined. This makes sense. `Item` no longer exists in the `db` package; it's been moved to the shopping package. We need to change `shopping/db/db.go` to:

```go
package db

import (
  "shopping"
)

func LoadItem(id int) *shopping.Item {
  return &shopping.Item{
    Price: 9.001,
  }
}
```

Now when you try to run the code, you'll get a dreaded _import cycle not allowed_ error. We solve this by introducing another package which contains shared structures. Your directory structure should look like:

```txt
$GOPATH/src
  - shopping
    pricecheck.go
    - db
      db.go
    - models
      item.go
    - main
      main.go
```

`pricecheck.go` will still import `shopping/db`, but `db.go` will now import `shopping/models` instead of `shopping`, thus breaking the cycle. Since we moved the shared `Item` structure to `shopping/models/item.go`, we need to change `shopping/db/db.go` to reference the `Item` structure from `models` package:

```go
package db

import (
  "shopping/models"
)

func LoadItem(id int) *models.Item {
  return &models.Item{
    Price: 9.001,
  }
}
```

You'll often need to share more than just `models`, so you might have other similar folder named `utilities` and such. The important rule about these shared packages is that they shouldn't import anything from the `shopping` package or any sub-packages. In a few sections, we'll look at interfaces which can help us untangle these types of dependencies.

### Visibility

Go uses a simple rule to define what types and functions are visible outside of a package. If the name of the type or function starts with an uppercase letter, it's visible. If it starts with a lowercase letter, it isn't.

This also applies to structure fields. If a structure field name starts with a lowercase letter, only code within the same package will be able to access them. For example, if our `items.go` file had a function that looked like:

```go
func NewItem() *Item {
  // ...
}
```

it could be called via `models.NewItem()`. But if the function was named `newItem`, we wouldn't be able to access it from a different package.

Go ahead and change the name of the various functions, types and fields from the `shopping` code. For example, if you rename the `Item's` `Price` field to `price`, you should get an error.

### Package Management

The `go` command we've been using to `run` and `build` has a `get` subcommand which is used to fetch third-party libraries. `go get` supports various protocols but for this example, we'll be getting a library from Github, meaning, you'll need `git` installed on your computer.

Assuming you already have git installed, from a shell/command prompt, enter:

```console
go get github.com/mattn/go-sqlite3
```

`go get` fetches the remote files and stores them in your workspace. Go ahead and check your `$GOPATH/src`. In addition to the `shopping` project that we created, you'll now see a `github.com` folder. Within, you'll see a `mattn` folder which contains a `go-sqlite3` folder.

We just talked about how to import packages that live in our workspace. To use our newly gotten `go-sqlite3` package, we'd import it like so:

```go
import (
  "github.com/mattn/go-sqlite3"
)
```

I know this looks like a URL but in reality, it'll simply import the `go-sqlite3` package which it expects to find in `$GOPATH/src/github.com/mattn/go-sqlite3`.

### Dependency Management

`go get` has a couple of other tricks up its sleeve. If we `go get` within a project, it'll scan all the files, looking for `imports` to third-party libraries and will download them. In a way, our own source code becomes a `Gemfile` or `package.json`.

If you call `go get -u` it'll update the packages (or you can update a specific package via `go get -u FULL_PACKAGE_NAME`).

Eventually, you might find `go get` inadequate. For one thing, there's no way to specify a revision, it always points to the master/head/trunk/default. This is an even larger problem if you have two projects needing different versions of the same library.

To solve this, you can use a third-party dependency management tool. They are still young, but two promising ones are [goop](https://github.com/nitrous-io/goop) and [godep](https://github.com/tools/godep). A more complete list is available at the [go-wiki](https://code.google.com/p/go-wiki/wiki/PackageManagementTools).

## Interfaces

Interfaces are types that define a contract but not an implementation. Here's an example:

```go
type Logger interface {
  Log(message string)
}
```

You might be wondering what purpose this could possibly serve. Interfaces help decouple your code from specific implementations. For example, we might have various types of loggers:

```go
type SqlLogger struct { ... }
type ConsoleLogger struct { ... }
type FileLogger struct { ... }
```

Yet by programming against the interface, rather than these concrete implementations, we can easily change (and test) which we use without any impact to our code.

How would you use one? Just like any other type, it could be a structure's field:

```go
type Server struct {
  logger Logger
}
```

or a function parameter (or return value):

```go
func process(logger Logger) {
  logger.Log("hello!")
}
```

In a language like C# or Java, we have to be explicit when a class implements an interface:

```go
public class ConsoleLogger : Logger {
  public void Logger(message string) {
    Console.WriteLine(message)
  }
}
```

In Go, this happens implicitly. If your structure has a function name `Log` with a `string` parameter and no return value, then it can be used as a `Logger`. This cuts down on the verboseness of using interfaces:

```go
type ConsoleLogger struct {}
func (l ConsoleLogger) Log(message string) {
  fmt.Println(message)
}
```

It also tends to promote small and focused interfaces. The standard library is full of interfaces. The `io` package has a handful of popular ones such as `io.Reader`, `io.Writer`, and `io.Closer`. If you write a function that expects a parameter that you'll only be calling `Close()` on, you absolutely should accept an `io.Closer` rather than whatever concrete type you're using.

Interfaces can also participate in composition. And, interfaces themselves can be composed of other interfaces. For example, `io.ReadCloser` is an interface composed of the `io.Reader` interface as well as the `io.Closer` interface.

Finally, interfaces are commonly used to avoid cyclical imports. Since they don't have implementations, they'll have limited dependencies.

## Error Handling

Go's preferred way to deal with errors is through return values, not exceptions. Consider the `strconv.Atoi` function which takes a string and tries to convert it to an integer:

```go
package main

import (
  "fmt"
  "os"
  "strconv"
)

func main() {
  if len(os.Args) != 2 {
    os.Exit(1)
  }

  n, err := strconv.Atoi(os.Args[1])
  if err != nil {
    fmt.Println("not a valid number")
  } else {
    fmt.Println(n)
  }
}
```

You can create your own error type; the only requirement is that it fulfills the contract of the built-in `error` interface, which is:

```go
type error interface {
  Error() string
}
```

More commonly, we can create our own errors by importing the `errors` package and using it in the `New` function:

```go
import (
  "errors"
)


func process(count int) error {
  if count < 1 {
    return errors.New("Invalid count")
  }
  ...
  return nil
}
```

There's a common pattern in Go's standard library of using error variables. For example, the `io` package has an `EOF` variable which is defined as:

```go
var EOF = errors.New("EOF")
```

This is a package variable (it's defined outside of a function) which is publicly accessible (upper-case first letter). Various functions can return this error, say when we're reading from a file or STDIN. If it makes contextual sense, you should use this error, too. As consumers, we can use this singleton:

```go
package main

import (
  "fmt"
  "io"
)

func main() {
  var input int
  _, err := fmt.Scan(&input)
  if err == io.EOF {
    fmt.Println("no more input!")
  }
}
```

As a final note, Go does have `panic` and `recover` functions. `panic` is like throwing an exception while `recover` is like `catch`; they are rarely used.

## Defer

Even though Go has a garbage collector, some resources require that we explicitly release them. For example, we need to `Close()` files after we're done with them. This sort of code is always dangerous. For one thing, as we're writing a function, it's easy to forget to `Close` something that we declared 10 lines up. For another, a function might have multiple return points. Go's solution is the `defer` keyword:

```go
package main

import (
  "fmt"
  "os"
)

func main() {
  file, err := os.Open("a_file_to_read")
  if err != nil {
    fmt.Println(err)
    return
  }
  defer file.Close()
  // read the file
}
```

If you try to run the above code, you'll probably get an error (the file doesn't exist). The point is to show how `defer` works. Whatever you `defer` will be executed after the enclosing function (in this case `main()`) returns, even if it does so violently. This lets you release resources near where it’s initialized and takes care of multiple return points.

## go fmt

Most programs written in Go follow the same formatting rules, namely, a tab is used to indent and braces go on the same line as their statement. The `go fmt` command is easy to use and authoritative (so no one argues over meaningless preferences). When you're inside a project, you can apply the formatting rule to it and all sub-projects via:

```console
go fmt ./...
```

Give it a try. It does more than indent your code; it also aligns field declarations and alphabetically orders imports.

## Initialized If

Go supports a slightly modified if-statement, one where a value can be initiated prior to the condition being evaluated:

```go
if x := 10; count > x {
  ...
}
```

That's a pretty silly example. More realistically, you might do something like:

```go
if err := process(); err != nil {
  return err
}
```

Interestingly, while the values aren't available outside the if-statement, they are available inside any `else if` or `else`.

## Empty Interface and Conversions

In most object-oriented languages, a built-in base class, often named `object`, is the superclass for all other classes. Go, having no inheritance, doesn't have such a superclass. What it does have is an empty interface with no methods: `interface{}`. Since every type implements all 0 of the empty interface's methods, and since interfaces are implicitly implemented, every type fulfills the contract of the empty interface.

 If we wanted to, we could write an `add` function with the following signature:

```go
func add(a interface{}, b interface{}) interface{} {
  ...
}
```

To convert an interface variable to an explicit type, you use `.(TYPE)`:

```go
return a.(int) + b.(int)
```

Note that if the underlying type is not `int`, the above will result in an error. You also have access to a powerful type switch:

```go
switch a.(type) {
  case int:
    fmt.Printf("a is now an int and equals %d\n", a)
  case bool, string:
    // ...
  default:
    // ...
}
```

You'll see and probably use the empty interface more than you might first expect. Admittedly, it won't result in clean code. Converting values back and forth is ugly and dangerous but sometimes, in a static language, it's the only choice.

## Strings and Byte Arrays

Strings and byte arrays are closely related. We can easily convert one to the other:

```go
stra := "the spice must flow"
byts := []byte(stra)
strb := string(byts)
```

In fact, this way of converting is common across various types as well. Some functions explicitly expect an `int32` or an `int64` or their unsigned counterparts. You might find yourself having to do things like:

```go
int64(count)
```

Still, when it comes to bytes and strings, it's probably something you'll end up doing often. Do note that when you use `[]byte(X)` or `string(X)`, you're creating a copy of the data. This is necessary because strings are immutable.

Strings are made of `runes` which are unicode code points. If you take the length of a string, you might not get what you expect. The following prints 3:

`fmt.Println(len("椒"))`

If you iterate over a string using `range`, you'll get runes, not bytes. Of course, when you turn a string into a `[]byte` you'll get the correct data.

## Function Type

Functions are first-class types:

```go
type Add func(a int, b int) int
```

which can then be used anywhere -- as a field type, as a parameter, as a return value.

```go
package main

import (
  "fmt"
)

type Add func(a int, b int) int

func main() {
  fmt.Println(process(func(a int, b int) int{
      return a + b
  }))
}

func process(adder Add) int {
  return adder(1, 2)
}
```

Using functions like this can help decouple code from specific implementations much like we achieve with interfaces.

## Goroutines

A goroutine is similar to a thread, but it is scheduled by Go, not the OS. Code that runs in a goroutine can run concurrently with other code. Let's look at an example:

```go
package main

import (
  "fmt"
  "time"
)

func main() {
  fmt.Println("start")
  go process()
  time.Sleep(time.Millisecond * 10) // this is bad, don't do this!
  fmt.Println("done")
}

func process() {
  fmt.Println("processing")
}
```

There are a few interesting things going on here, but the most important is how we start a goroutine. We simply use the `go` keyword followed by the function we want to execute. If we just want to run a bit of code, such as the above, we can use an anonymous function. Do note that anonymous functions aren't only used with goroutines, however.

```go
go func() {
  fmt.Println("processing")
}()
```

Goroutines are easy to create and have little overhead. Multiple goroutines will end up running on the same underlying OS thread. This is often called an M:N threading model because we have M application threads (goroutines) running on N OS threads. The result is that a goroutine has a fraction of overhead (a few KB) than OS threads. On modern hardware, it's possible to have millions of goroutines.

Furthermore, the complexity of mapping and scheduling is hidden. We just say _this code should run concurrently_ and let Go worry about making it happen.

If we go back to our example, you'll notice that we had to `Sleep` for a few milliseconds. That's because the main process exits before the goroutine gets a chance to execute (the process doesn't wait until all goroutines are finished before exiting). To solve this, we need to coordinate our code.

## Synchronization

Creating goroutines is trivial, and they are so cheap that we can start many; however, concurrent code needs to be coordinated. To help with this problem, Go provides `channels`. Before we look at `channels`, I think it's important to understand a little bit about the basics of concurrent programming.

Writing concurrent code requires that you pay specific attention to where and how you read and write values. In some ways, it's like programming without a garbage collector -- it requires that you think about your data from a new angle, always watchful for possible danger. Consider:

```go
package main

import (
  "fmt"
  "time"
)

var counter = 0

func main() {
  for i := 0; i < 20; i++ {
    go incr()
  }
  time.Sleep(time.Millisecond * 10)
}

func incr() {
  counter++
  fmt.Println(counter)
}
```

What do you think the output will be?

If you think the output is `1, 2, ... 20` you're both right and wrong. It's true that if you run the above code, you'll sometimes get that output. However, the reality is that the behavior is undefined. Why? Because we potentially have multiple (two in this case) goroutines writing to the same variable, `counter`, at the same time. Or, just as bad, one goroutine would be reading `counter` while another writes to it.

Is that really a danger? Yes, absolutely. `counter++` might seem like a simple line of code, but it actually gets broken down into multiple assembly statements -- the exact nature is dependent on the platform that you're running. If you run this example, you'll see that very often the numbers are printed in a weird order, and/or numbers are duplicated/missing. There are worse possibilities too, such as system crashes or accessing an arbitrary piece of data and incrementing it!

The only concurrent thing you can safely do to a variable is to read from it. You can have as many readers as you want, but writes need to be synchronized. There are various ways to do this, including using some truly atomic operations that rely on special CPU instructions. However, the most common approach is to use a mutex:

```go
package main

import (
  "fmt"
  "time"
  "sync"
)

var (
  counter = 0
  lock sync.Mutex
)

func main() {
  for i := 0; i < 20; i++ {
    go incr()
  }
  time.Sleep(time.Millisecond * 10)
}

func incr() {
  lock.Lock()
  defer lock.Unlock()
  counter++
  fmt.Println(counter)
}
```

A mutex serializes access to the code under lock. The reason we simply define our lock as `lock sync.Mutex` is because the default value of a `sync.Mutex` is unlocked.

Seems simple enough? The example above is deceptive. There's a whole class of serious bugs that can arise when doing concurrent programming. First of all, it isn't always so obvious what code needs to be protected. While it might be tempting to use coarse locks (locks that cover a large amount of code), that undermines the very reason we're doing concurrent programming in the first place. We generally want fine locks; else, we end up with a ten-lane highway that suddenly turns into a one-lane road.

The other problem has to do with deadlocks. With a single lock, this isn't a problem, but if you're using two or more locks around the same code, it's dangerously easy to have situations where goroutineA holds lockA but needs access to lockB, while goroutineB holds lockB but needs access to lockA.

It actually _is_ possible to deadlock with a single lock, if we forget to release it. This isn't as dangerous as a multi-lock deadlock (because those are _really_ tough to spot), but just so you can see what happens, try running:

```go
package main

import (
  "time"
  "sync"
)

var (
  lock sync.Mutex
)

func main() {
  go func() { lock.Lock() }()
  time.Sleep(time.Millisecond * 10)
  lock.Lock()
}
```

There's more to concurrent programming than what we've seen so far. For one thing, there's another common mutex called a read-write mutex. This exposes two locking functions: one to lock for reading and one to lock for writing. This distinction allows multiple simultaneous readers while ensuring that writing is exclusive. In Go, `sync.RWMutex` is such a lock. In addition to the `Lock` and `Unlock` methods of a `sync.Mutex`, it also exposes `RLock` and `RUnlock` methods; where `R` stands for _Read_. While read-write mutexes are commonly used, they place an additional burden on developers: we must now pay attention to not only when we're accessing data, but also how.

Furthermore, part of concurrent programming isn't so much about serializing access across the narrowest possible piece of code; it's also about coordinating multiple goroutines. For example, sleeping for 10 milliseconds isn't a particularly elegant solution. What if a goroutine takes more than 10 milliseconds? What if it takes less and we're just wasting cycles? Also, what if instead of just waiting for goroutines to finish, we want to tell one _hey, I have new data for you to process!_?

These are all things that are doable without `channels`. Certainly for simpler cases, I believe you **should** use primitives such as `sync.Mutex` and `sync.RWMutex`, but as we'll see in the next section, `channels` aim at making concurrent programming cleaner and less error-prone.

## Channels

The challenge with concurrent programming stems from sharing data. If your goroutines share no data, you needn't worry about synchronizing them. That isn't an option for all systems, however. In fact, many systems are built with the exact opposite goal in mind: to share data across multiple requests. An in-memory cache or a database, are good examples of this. This is becoming an increasingly common reality.

Channels help make concurrent programming saner by taking shared data out of the picture. A channel is a communication pipe between goroutines which is used to pass data. In other words, a goroutine that has data can pass it to another goroutine via a channel. The result is that, at any point in time, only one goroutine has access to the data.

A channel, like everything else, has a type. This is the type of data that we'll be passing through our channel. For example, to create a channel which can be used to pass an integer around, we'd do:

```go
c := make(chan int)
```

The type of this channel is `chan int`. Therefore, to pass this channel to a function, our signature looks like:

```go
func worker(c chan int) { ... }
```

Channels support two operations: receiving and sending. We send to a channel by doing:

```go
CHANNEL <- DATA
```

and receive from one by doing

```go
VAR := <-CHANNEL
```

The arrow points in the direction that data flows. When sending, the data flows into the channel. When receiving, the data flows out of the channel.

The final thing to know before we look at our first example is that receiving and sending to and from a channel is blocking. That is, when we receive from a channel, execution of the goroutine won't continue until data is available. Similarly, when we send to a channel, execution won't continue until the data is received.

Consider a system with incoming data that we want to handle in separate goroutines. This is a common requirement. If we did our data-intensive processing on the goroutine which accepts the incoming data, we'd risk timing out clients. First, we'll write our worker. This could be a simple function, but I'll make it part of a structure since we haven't seen goroutines used like this before:

```go
type Worker struct {
  id int
}

func (w Worker) process(c chan int) {
  for {
    data := <-c
    fmt.Printf("worker %d got %d\n", w.id, data)
  }
}
```

Our worker is simple. It waits until data is available then "processes" it. Dutifully, it does this in a loop, forever waiting for more data to process.

To use this, the first thing we'd do is start some workers:

```go
c := make(chan int)
for i := 0; i < 5; i++ {
  worker := &Worker{id: i}
  go worker.process(c)
}
```

And then we can give them some work:

```go
for {
  c <- rand.Int()
  time.Sleep(time.Millisecond * 50)
}
```

Here's the complete code to make it run:

```go
package main

import (
  "fmt"
  "time"
  "math/rand"
)

func main() {
  c := make(chan int)
  for i := 0; i < 5; i++ {
    worker := &Worker{id: i}
    go worker.process(c)
  }

  for {
    c <- rand.Int()
    time.Sleep(time.Millisecond * 50)
  }
}

type Worker struct {
  id int
}

func (w *Worker) process(c chan int) {
  for {
    data := <-c
    fmt.Printf("worker %d got %d\n", w.id, data)
  }
}
```

We don't know which worker is going to get what data. What we do know, what Go guarantees, is that the data we send to a channel will only be received by a single receiver.

Notice that the only shared state is the channel, which we can safely receive from and send to concurrently. Channels provide all of the synchronization code we need and also ensure that, at any given time, only one goroutine has access to a specific piece of data.

### Buffered Channels

Given the above code, what happens if we have more data coming in than we can handle? You can simulate this by changing the worker to sleep after it has received data:

```go
for {
  data := <-c
  fmt.Printf("worker %d got %d\n", w.id, data)
  time.Sleep(time.Millisecond * 500)
}
```

What's happening is that our main code, the one that accepts the user's incoming data (which we just simulated with a random number generator) is blocking as it sends to the channel because no receiver is available.

In cases where you need high guarantees that the data is being processed, you probably will want to start blocking the client. In other cases, you might be willing to loosen those guarantees. There are a few popular strategies to do this. The first is to buffer the data. If no worker is available, we want to temporarily store the data in some sort of queue. Channels have this buffering capability built-in. When we created our channel with `make`, we can give our channel a length:

```go
c := make(chan int, 100)
```

You can make this change, but you'll notice that the processing is still choppy. Buffered channels don't add more capacity; they merely provide a queue for pending work and a good way to deal with a sudden spike. In our example, we're continuously pushing more data than our workers can handle.

Nevertheless, we can get a sense what the buffered channel is, in fact, buffering by looking at the channel's `len`:

```go
for {
  c <- rand.Int()
  fmt.Println(len(c))
  time.Sleep(time.Millisecond * 50)
}
```

You can see that it grows and grows until it fills up, at which point sending to our channel start to block again.

### Select

Even with buffering, there comes a point where we need to start dropping messages. We can't use up an infinite amount of memory hoping a worker frees up. For this, we use Go's `select`.

Syntactically, `select` looks a bit like a switch. With it, we can provide code for when the channel isn't available to send to. First, let's remove our channel's buffering so that we can clearly see how `select` works:

```go
c := make(chan int)
```

Next, we change our `for` loop:

```go
for {
  select {
  case c <- rand.Int():
    //optional code here
  default:
    //this can be left empty to silently drop the data
    fmt.Println("dropped")
  }
  time.Sleep(time.Millisecond * 50)
}
```

We're pushing out 20 messages per second, but our workers can only handle 10 per second; thus, half the messages get dropped.

This is only the start of what we can accomplish with `select`. A main purpose of select is to manage multiple channels. Given multiple channels, `select` will block until the first one becomes available. If no channel is available, `default` is executed if one is provided. A channel is randomly picked when multiple are available.

It's hard to come up with a simple example that demonstrates this behavior as it's a fairly advanced feature. The next section might help illustrate this though.

### Timeout

We've looked at buffering messages as well as simply dropping them. Another popular option is to timeout. We're willing to block for some time, but not forever. This is also something easy to achieve in Go. Admittedly, the syntax might be hard to follow but it's such a neat and useful feature that I couldn't leave it out.

To block for a maximum amount of time, we can use the `time.After` function. Let's look at it then try to peek beyond the magic. To use this, our sender becomes:

```go
for {
  select {
  case c <- rand.Int():
  case <-time.After(time.Millisecond * 100):
    fmt.Println("timed out")
  }
  time.Sleep(time.Millisecond * 50)
}
```

`time.After` returns a channel, so we can `select` from it. The channel is written to after the specified time expires. That's it. There's nothing more magical than that. If you're curious, here's what an implementation of `after` could look like:

```go
func after(d time.Duration) chan bool {
  c := make(chan bool)
  go func() {
    time.Sleep(d)
    c <- true
  }()
  return c
}
```

Back to our `select`, there are a couple of things to play with. First, what happens if you add the `default` case back? Can you guess? Try it. If you aren't sure what's going on, remember that `default` fires immediately if no channel is available.

Also, `time.After` is a channel of type `chan time.Time`. In the above example, we simply discard the value that was sent to the channel. If you want though, you can receive it:

```go
case t := <-time.After(time.Millisecond * 100):
  fmt.Println("timed out at", t)
```

Pay close attention to our `select`. Notice that we're sending to `c` but receiving from `time.After`. `select` works the same regardless of whether we're receiving from, sending to, or any combination of channels:

- The first available channel is chosen.
- If multiple channels are available, one is randomly picked.
- If no channel is available, the default case is executed.
- If there's no default, select blocks.

Finally, it's common to see a `select` inside a `for`. Consider:

```go
for {
  select {
  case data := <-c:
    fmt.Printf("worker %d got %d\n", w.id, data)
  case <-time.After(time.Millisecond * 10):
    fmt.Println("Break time")
    time.Sleep(time.Second)
  }
}
```

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

- Type cast with `type()`
- Inference `:=`

- Array: static linear list of data `name := [3]int` or `name := [size]int{init, init2}` or `name := [...]int{2,3,4}`

- Slice: reference type to array `name := []int` or `name := []int{2,3,4}` or with make([]int, len()), append() can be used. slices can use : syntax.

- Maps: key value pair

```go
var m map[string]int
m2 := make(map[string]int)
m2["apple"] = 3

m3 := map[string]int{
    "banana": 10,
    "orange": 8,
}

m := map[string]int{"a": 1, "b": 2}
fmt.Println(m["a"])

m["c"] = 3
delete(m, "b")
fmt.Println(m)
```

- Make(): memory allocator: built-in function used to allocate and initialize slices, maps, and channels

```go
make(t Type, size ...IntegerType) Type // t type, s size

ch := make(chan int, 3) // Buffered channel with capacity 3
ch <- 10
fmt.Println(<-ch) // Output: 10
```

- **Structs**

```go
p1 := Person{"Alice", 30}

p2 := Person{Name: "Bob", Age: 25}

p3 := new(Person) // Equivalent to &Person{}
p3.Name = "Charlie"
p3.Age = 40

fmt.Println(p1.Name) // Output: Alice

type Car struct {
    Brand string
}

func (c Car) Drive() {
    fmt.Println(c.Brand, "is driving!")
}

func main() {
    car := Car{"Tesla"}
    car.Drive() // Output: Tesla is driving!
}

type Employee struct {
    Person
    Position string
}

e := Employee{Person{"Dave", 35}, "Manager"}
fmt.Println(e.Name, e.Position) // Output: Dave Manager

```

- **Type assertions**

```go
value, ok := interfaceValue.(ConcreteType)

var x interface{} = "hello"
str, ok := x.(string)
if ok {
    fmt.Println(str) // Output: hello
} else {
    fmt.Println("Type assertion failed")
}


var x interface{} = 42
str := x.(string) // This will panic because x is not a string
fmt.Println(str)


func identify(i interface{}) {
    switch v := i.(type) {
    case int:
        fmt.Println("Integer:", v)
    case string:
        fmt.Println("String:", v)
    default:
        fmt.Println("Unknown type")
    }
}

func main() {
    identify(42)      // Output: Integer: 42
    identify("hello") // Output: String: hello
}
```

- Error, Panic, Recover

```go

// new error
return error.new("hello");

package main

import "fmt"

func main() {
    defer func() {
        if r := recover(); r != nil {
            fmt.Println("Recovered from panic:", r)
        }
    }()
    
    panic("Unexpected error")
    fmt.Println("This will not execute")
}

```

- Go modules, Interfaces, Context, Goroutines, Channels, Buffer, Mutex,
  Select, Schedular, Pointers, Generics

```go
/*

go mod init <>
go mod tidy

type name interface {}

context:

go routines:

channels:

buffer:

mutex:

select:


The Go runtime schedules goroutines on OS threads.
Uses work-stealing to balance workload across threads.

Key Concepts

GOMAXPROCS → Number of CPU cores used for scheduling (runtime.GOMAXPROCS())
Preemptive Scheduling → Allows goroutines to be interrupted for fairness.
*/
```

- Testing
- Marshalling and Unmarshalling JSON
- urfave/cli
- ORM: GORM
- Web framework: Gin, Echo, Chi

```go
/*
appending _test.go to filenames (example_test.go) turns them into a test files.
it is handled by "testing" package.

// Function to test
func Add(a, b int) int {
    return a + b
}

// Test function
func TestAdd(t *testing.T) {
    result := Add(2, 3)
    expected := 5

    if result != expected {
        t.Errorf("Expected %d but got %d", expected, result)
    }
}

// TABLE DRIVEN TESTING

func TestAddTableDriven(t *testing.T) {
    tests := []struct {
        a, b, expected int
    }{
        {1, 2, 3},
        {0, 5, 5},
        {-1, -1, -2},
    }

    for _, test := range tests {
        result := Add(test.a, test.b)
        if result != test.expected {
            t.Errorf("Add(%d, %d) = %d; want %d", test.a, test.b, result, test.expected)
        }
    }
}


BENCHMARKING
func BenchmarkAdd(b *testing.B) {
    for i := 0; i < b.N; i++ {
        Add(2, 3)
    }
}


run `go test -bench .`

JSON MARSHALLING
package main

import (
    "encoding/json"
    "fmt"
)

type Person struct {
    Name string `json:"name"`
    Age  int    `json:"age"`
}

func main() {
    p := Person{Name: "Alice", Age: 30}
    jsonData, _ := json.Marshal(p)
    fmt.Println(string(jsonData)) // {"name":"Alice","age":30}
}


UNMARSHALLING
func main() {
    jsonData := `{"name":"Bob","age":25}`
    var p Person
    json.Unmarshal([]byte(jsonData), &p)
    fmt.Println(p.Name, p.Age) // Bob 25
}


HANDLING UNKNOWN FIELDS
func main() {
    jsonData := `{"name":"Charlie","age":40,"city":"New York"}`
    var data map[string]interface{}
    json.Unmarshal([]byte(jsonData), &data)
    fmt.Println(data["name"], data["city"]) // Charlie New York
}


URFAVE/CLI

package main

import (
    "fmt"
    "log"
    "os"

    "github.com/urfave/cli/v2"
)

func main() {
    app := &cli.App{
        Name:  "greet",
        Usage: "A simple CLI tool",
        Commands: []*cli.Command{
            {
                Name:  "hello",
                Usage: "Prints a greeting",
                Action: func(c *cli.Context) error {
                    fmt.Println("Hello, World!")
                    return nil
                },
            },
        },
    }

    if err := app.Run(os.Args); err != nil {
        log.Fatal(err)
    }
}


GORM

package main

import (
    "gorm.io/driver/sqlite"
    "gorm.io/gorm"
    "log"
)

type User struct {
    ID    uint   `gorm:"primaryKey"`
    Name  string
    Email string
}

func main() {
    db, err := gorm.Open(sqlite.Open("test.db"), &gorm.Config{})
    if err != nil {
        log.Fatal(err)
    }

    // Migrate the schema
    db.AutoMigrate(&User{})

    // Create a new user
    db.Create(&User{Name: "Alice", Email: "alice@example.com"})

    // Read a user
    var user User
    db.First(&user, "name = ?", "Alice")
    log.Println(user)
}


GIN FRAMEWORK

package main

import (
    "github.com/gin-gonic/gin"
)

func main() {
    r := gin.Default()
    r.GET("/hello", func(c *gin.Context) {
        c.JSON(200, gin.H{"message": "Hello, World!"})
    })
    r.Run(":8080")
}


ECHO FRAMEWORK
package main

import (
    "github.com/labstack/echo/v4"
    "net/http"
)

func main() {
    e := echo.New()

    e.GET("/hello", func(c echo.Context) error {
        return c.JSON(http.StatusOK, map[string]string{"message": "Hello, Echo!"})
    })

    e.Start(":8080")
}
*/
```

- Logging: log/slog, Zerolog
- Realtime communication: Melody, Centrifugo
- API Clients: REST -> Heimdall, GRequests | GraphQL -> gqlgen, graphql-go
- Auth (JWT, OAuth)
- benchmarking, profiling and debugging
- Microservices: protocol buffers, Watermill

```go
/*
SLOG
package main

import (
    "log/slog"
    "os"
)

func main() {
    logger := slog.New(slog.NewJSONHandler(os.Stdout, nil))
    logger.Info("Application started", "version", "1.0.0")
}

ZEROLOG
package main

import (
    "github.com/rs/zerolog/log"
    "os"
)

func main() {
    log.Logger = log.Output(os.Stdout)
    log.Info().Msg("Application started")
    log.Error().Str("module", "main").Msg("Something went wrong")
}


WEBSOCKET WITH MELODY
package main

import (
    "github.com/gin-gonic/gin"
    "github.com/olahol/melody"
)

func main() {
    r := gin.Default()
    m := melody.New()

    r.GET("/ws", func(c *gin.Context) {
        m.HandleRequest(c.Writer, c.Request)
    })

    m.HandleMessage(func(s *melody.Session, msg []byte) {
        m.Broadcast(msg)
    })

    r.Run(":5000")
}


CENTRIFUGO
go install github.com/centrifugal/centrifugo@latest

centrifugo genconfig
centrifugo serve


HEIMDALL HTTP
package main

import (
    "fmt"
    "github.com/gojek/heimdall/v7/httpclient"
    "io/ioutil"
)

func main() {
    client := httpclient.NewClient()
    res, err := client.Get("https://jsonplaceholder.typicode.com/posts/1", nil)
    if err != nil {
        fmt.Println(err)
        return
    }

    body, _ := ioutil.ReadAll(res.Body)
    fmt.Println(string(body))
}


JWT AUTH

package main

import (
    "fmt"
    "github.com/golang-jwt/jwt/v5"
    "time"
)

var secretKey = []byte("supersecret")

func createJWT() (string, error) {
    token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
        "user": "alice",
        "exp":  time.Now().Add(time.Hour * 1).Unix(),
    })
    return token.SignedString(secretKey)
}

func main() {go run main.go


    token, _ := createJWT()
    fmt.Println("JWT Token:", token)
}


OAUTH2

config := &oauth2.Config{
    ClientID:     "your-client-id",
    ClientSecret: "your-client-secret",
    Endpoint:     google.Endpoint,
    RedirectURL:  "http://localhost:8080/callback",
}


PROFILING
import "github.com/pkg/profile"

func main() {
    defer profile.Start(profile.CPUProfile).Stop()
}

go run main.go
go tool pprof cpu.pprof


PROTOBUFS

go install google.golang.org/protobuf/cmd/protoc-gen-go@latest

service.proto
-------------

syntax = "proto3";

service Greeter {
    rpc SayHello (HelloRequest) returns (HelloResponse);
}

message HelloRequest {
    string name = 1;
}

message HelloResponse {
    string message = 1;
}


COMMAND: protoc --go_out=. --go-grpc_out=. service.proto


WATERMILL: EVENT DRIVEN MICROSERVICES

go get github.com/ThreeDotsLabs/watermill


package main

import (
    "fmt"
    "log"

    "github.com/ThreeDotsLabs/watermill"
    "github.com/ThreeDotsLabs/watermill/message"
    "github.com/ThreeDotsLabs/watermill/pubsub/gochannel"
)

func main() {
    logger := watermill.NewStdLogger(false, false)
    pubSub := gochannel.NewGoChannel(gochannel.Config{}, logger)

    subscriber, _ := pubSub.Subscribe("example_topic")
    go func() {
        for msg := range subscriber {
            fmt.Println("Received:", string(msg.Payload))
            msg.Ack()
        }
    }()

    pubSub.Publish("example_topic", message.NewMessage(watermill.NewUUID(), []byte("Hello, World!")))
}
*/
```


# TODO

A slice consists of three things:

- A pointer reference to an underlying array.
- The length of the segment of the array that the slice contains.
- And, the capacity, which is the maximum size up to which the segment can grow.

Just like `len` function, we can determine the capacity of a slice using the built-in `cap` function. Here's an example:

```go
package main

import "fmt"

func main() {
	a := [5]int{20, 15, 5, 30, 25}

	s := a[1:4]

	// Output: Array: [20 15 5 30 25], Length: 5, Capacity: 5
	fmt.Printf("Array: %v, Length: %d, Capacity: %d\n", a, len(a), cap(a))

	// Output: Slice [15 5 30], Length: 3, Capacity: 4
	fmt.Printf("Slice: %v, Length: %d, Capacity: %d", s, len(s), cap(s))
}
```

Don't worry, we are going to discuss everything shown here in detail.

### Declaration

Let's see how we can declare a slice.

```go
var s []T
```

As we can see, we don't need to specify any length. Let's declare a slice of integers and see how it works.

```go
func main() {
	var s []string

	fmt.Println(s)
	fmt.Println(s == nil)
}
```

```bash
$ go run main.go
[]
true
```

So, unlike arrays, the zero value of a slice is `nil`.

### Initialization

There are multiple ways to initialize our slice. One way is to use the built-in `make` function.

```go
make([]T, len, cap) []T
```

```go
func main() {
	var s = make([]string, 0, 0)

	fmt.Println(s)
}
```

```bash
$ go run main.go
[]
```

Similar to arrays, we can use the slice literal to initialize our slice.

```go
func main() {
	var s = []string{"Go", "TypeScript"}

	fmt.Println(s)
}
```

```bash
$ go run main.go
[Go TypeScript]
```

Another way is to create a slice from an array. Since a slice is a segment of an array, we can create a slice from index `low` to `high` as follows.

```go
a[low:high]
```

```go
func main() {
	var a = [4]string{
		"C++",
		"Go",
		"Java",
		"TypeScript",
	}

	s1 := a[0:2] // Select from 0 to 2
	s2 := a[:3]  // Select first 3
	s3 := a[2:]  // Select last 2

	fmt.Println("Array:", a)
	fmt.Println("Slice 1:", s1)
	fmt.Println("Slice 2:", s2)
	fmt.Println("Slice 3:", s3)
}
```

```bash
$ go run main.go
Array: [C++ Go Java TypeScript]
Slice 1: [C++ Go]
Slice 2: [C++ Go Java]
Slice 3: [Java TypeScript]
```

_Missing low index implies 0 and missing high index implies the length of the underlying array (`len(a)`)._

The thing to note here is we can create a slice from other slices too and not just arrays.

```go
var a = []string{
	"C++",
	"Go",
	"Java",
	"TypeScript",
}
```

### Iteration

We can iterate over a slice in the same way you iterate over an array, by using the for loop with either `len` function or `range` keyword.

### Functions

So now, let's talk about built-in slice functions provided in Go.

**copy**

The `copy()` function copies elements from one slice to another. It takes 2 slices, a destination, and a source. It also returns the number of elements copied.

```go
func copy(dst, src []T) int
```

Let's see how we can use it.

```go
func main() {
	s1 := []string{"a", "b", "c", "d"}
	s2 := make([]string, len(s1))

	e := copy(s2, s1)

	fmt.Println("Src:", s1)
	fmt.Println("Dst:", s2)
	fmt.Println("Elements:", e)
}
```

```bash
$ go run main.go
Src: [a b c d]
Dst: [a b c d]
Elements: 4
```

As expected, our 4 elements from the source slice were copied to the destination slice.

**append**

Now, let's look at how we can append data to our slice using the built-in `append` function which appends new elements at the end of a given slice.

It takes a slice and a variable number of arguments. It then returns a new slice containing all the elements.

```go
append(slice []T, elems ...T) []T
```

Let's try it in an example by appending elements to our slice.

```go
func main() {
	s1 := []string{"a", "b", "c", "d"}

	s2 := append(s1, "e", "f")

	fmt.Println("s1:", s1)
	fmt.Println("s2:", s2)
}
```

```bash
$ go run main.go
s1: [a b c d]
s2: [a b c d e f]
```

As we can see, the new elements were appended and a new slice was returned.

But if the given slice doesn't have sufficient capacity for the new elements then a new underlying array is allocated with a bigger capacity.

All the elements from the underlying array of the existing slice are copied to this new array, and then the new elements are appended.

### Properties

Finally, let's discuss some properties of slices.

Slices are reference types, unlike arrays.

This means modifying the elements of a slice will modify the corresponding elements in the referenced array.

```go
package main

import "fmt"

func main() {
	a := [7]string{"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"}

	s := a[0:2]

	s[0] = "Sun"

	fmt.Println(a) // Output: [Sun Tue Wed Thu Fri Sat Sun]
	fmt.Println(s) // Output: [Sun Tue]
}
```

Slices can be used with variadic types as well.

```go
package main

import "fmt"

func main() {
	values := []int{1, 2, 3}
	sum := add(values...)
	fmt.Println(sum)
}

func add(values ...int) int {
	sum := 0
	for _, v := range values {
		sum += v
	}

	return sum
}
```

# Maps

So, Go provides a built-in map type, and we'll learn how to use it.

But, the question is what are maps? And why do we need them?

![maps](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-II/maps/maps.png)

Well, A map is an unordered collection of key-value pairs. It maps keys to values. The keys are unique within a map while the values may not be.

It is used for fast lookups, retrieval, and deletion of data based on keys. It is one of the most used data structures.

## Declaration

Let's start with the declaration.

A map is declared using the following syntax:

```go
var m map[K]V
```

Where `K` is the key type and `V` is the value type.

For example, here's how we can declare a map of `string` keys to `int` values.

```go
func main() {
	var m map[string]int

	fmt.Println(m)
}
```

```bash
$ go run main.go
nil
```

As we can see, the zero value of a map is `nil`.

A `nil` map has no keys. Moreover, any attempt to add keys to a `nil` map will result in a runtime error.

## Initialization

There are multiple ways to initialize a map.

**make function**

We can use the built-in `make` function, which allocates memory for referenced data types and initializes their underlying data structures.

```go
func main() {
	var m = make(map[string]int)

	fmt.Println(m)
}
```

```bash
$ go run main.go
map[]
```

**map literal**

Another way is using a map literal.

```go
func main() {
	var m = map[string]int{
		"a": 0,
    "b": 1,
	}

	fmt.Println(m)
}
```

_Note that the trailing comma is required._

```bash
$ go run main.go
map[a:0 b:1]
```

As always, we can use our custom types as well.

```go
type User struct {
	Name string
}

func main() {
	var m = map[string]User{
		"a": User{"Peter"},
		"b": User{"Seth"},
	}

	fmt.Println(m)
}
```

We can even remove the value type and Go will figure it out!

```go
var m = map[string]User{
	"a": {"Peter"},
	"b": {"Seth"},
}
```

```bash
$ go run main.go
map[a:{Peter} b:{Seth}]
```

## Add

Now, let's see how we can add a value to our map.

```go
func main() {
	var m = map[string]User{
		"a": {"Peter"},
		"b": {"Seth"},
	}

	m["c"] = User{"Steve"}

	fmt.Println(m)
}
```

```bash
$ go run main.go
map[a:{Peter} b:{Seth} c:{Steve}]
```

## Retrieve

We can also retrieve our values from the map using the key.

```go
...
c := m["c"]
fmt.Println("Key c:", c)
```

```bash
$ go run main.go
key c: {Steve}
```

**What if we use a key that is not present in the map?**

```go
...
d := m["d"]
fmt.Println("Key d:", d)
```

Yes, you guessed it! we will get the zero value of the map's value type.

```bash
$ go run main.go
Key c: {Steve}
Key d: {}
```

## Exists

When you retrieve the value assigned to a given key, it returns an additional boolean value as well. The boolean variable will be `true` if the key exists, and `false` otherwise.

Let's try this in an example:

```go
...
c, ok := m["c"]
fmt.Println("Key c:", c, ok)

d, ok := m["d"]
fmt.Println("Key d:", d, ok)
```

```bash
$ go run main.go
Key c: {Steve} Present: true
Key d: {} Present: false
```

## Update

We can also update the value for a key by simply re-assigning a key.

```go
...
m["a"] = "Roger"
```

```bash
$ go run main.go
map[a:{Roger} b:{Seth} c:{Steve}]
```

## Delete

Or, we can delete the key using the built-in `delete` function.

Here's how the syntax looks:

```go
...
delete(m, "a")
```

The first argument is the map, and the second is the key we want to delete.

The `delete()` function doesn't return any value. Also, it doesn't do anything if the key doesn't exist in the map.

```bash
$ go run main.go
map[a:{Roger} c:{Steve}]
```

## Iteration

Similar to arrays or slices, we can iterate over maps with the `range` keyword.

```go
package main

import "fmt"

func main() {
	var m = map[string]User{
		"a": {"Peter"},
		"b": {"Seth"},
	}

	m["c"] = User{"Steve"}

	for key, value := range m {
		fmt.Println("Key: %s, Value: %v", key, value)
	}
}
```

```bash
$ go run main.go
Key: c, Value: {Steve}
Key: a, Value: {Peter}
Key: b, Value: {Seth}
```

Note that a map is an unordered collection, and therefore the iteration order of a map is not guaranteed to be the same every time we iterate over it.

## Properties

Lastly, let's talk about map properties.

Maps are reference types, which means when we assign a map to a new variable, they both refer to the same underlying data structure.

Therefore, changes done by one variable will be visible to the other.

```go
package main

import "fmt"

type User struct {
	Name string
}

func main() {
	var m1 = map[string]User{
		"a": {"Peter"},
		"b": {"Seth"},
	}

	m2 := m1
	m2["c"] = User{"Steve"}

	fmt.Println(m1) // Output: map[a:{Peter} b:{Seth} c:{Steve}]
	fmt.Println(m2) // Output: map[a:{Peter} b:{Seth} c:{Steve}]
}
```

# Interfaces

In this section, let's talk about the interfaces.

## What is an interface?

So, an interface in Go is an **abstract type** that is defined using a set of method signatures. The interface defines the **behavior** for similar types of objects.

_Here, **behavior** is a key term that we will discuss shortly._

Let's take a look at an example to understand this better.

One of the best real-world examples of interfaces is the power socket. Imagine that we need to connect different devices to the power socket.

![no-interface](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-III/interfaces/no-interface.png)

Let's try to implement this. Here are the device types we will be using.

```go
type mobile struct {
	brand string
}

type laptop struct {
	cpu string
}

type toaster struct {
	amount int
}

type kettle struct {
	quantity string
}

type socket struct{}
```

Now, let's define a `Draw` method on a type, let's say `mobile`. Here we will simply print the properties of the type.

```go
func (m mobile) Draw(power int) {
	fmt.Printf("%T -> brand: %s, power: %d", m, m.brand, power)
}
```

Great, now we will define the `Plug` method on the `socket` type which accepts our `mobile` type as an argument.

```go
func (socket) Plug(device mobile, power int) {
	device.Draw(power)
}
```

Let's try to _"connect"_ or _"plug in"_ the `mobile` type to our `socket` type in the `main` function.

```go
package main

import "fmt"

func main() {
	m := mobile{"Apple"}

	s := socket{}
	s.Plug(m, 10)
}
```

And if we run this we'll see the following.

```bash
$ go run main.go
main.mobile -> brand: Apple, power: 10
```

This is interesting, but let's say now we want to connect our `laptop` type.

```go
package main

import "fmt"

func main() {
	m := mobile{"Apple"}
	l := laptop{"Intel i9"}

	s := socket{}

	s.Plug(m, 10)
	s.Plug(l, 50) // Error: cannot use l as mobile value in argument
}
```

As we can see, this will throw an error.

**What should we do now? Define another method? Such as `PlugLaptop`?**

Sure, but then every time we add a new device type we will need to add a new method to the socket type as well and that's not ideal.

This is where the `interface` comes in. Essentially, we want to define a **contract** that, in the future, must be implemented.

We can simply define an interface such as `PowerDrawer` and use it in our `Plug` function to allow any device that satisfies the criteria, which is that the type must have a `Draw` method matching the signature that the interface requires.

And anyways, the socket doesn't need to know anything about our device and can simply call the `Draw` method.

![interface](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-III/interfaces/interface.png)

Now let's try to implement our `PowerDrawer` interface. Here's how it will look.

The convention is to use **"-er"** as a suffix in the name. And as we discussed earlier, an interface should only describe the **expected behavior**. Which in our case is the `Draw` method.

![interface-implementation](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-III/interfaces/interface-implementation.png)

```go
type PowerDrawer interface {
	Draw(power int)
}
```

Now, we need to update our `Plug` method to accept a device that implements the `PowerDrawer` interface as an argument.

```go
func (socket) Plug(device PowerDrawer, power int) {
	device.Draw(power)
}
```

And to satisfy the interface, we can simply add `Draw` methods to all the device types.

```go
type mobile struct {
	brand string
}

func (m mobile) Draw(power int) {
	fmt.Printf("%T -> brand: %s, power: %d\n", m, m.brand, power)
}

type laptop struct {
	cpu string
}

func (l laptop) Draw(power int) {
	fmt.Printf("%T -> cpu: %s, power: %d\n", l, l.cpu, power)
}

type toaster struct {
	amount int
}

func (t toaster) Draw(power int) {
	fmt.Printf("%T -> amount: %d, power: %d\n", t, t.amount, power)
}

type kettle struct {
	quantity string
}

func (k kettle) Draw(power int) {
	fmt.Printf("%T -> quantity: %s, power: %d\n", k, k.quantity, power)
}
```

Now, we can connect all our devices to the socket with the help of our interface!

```go
func main() {
	m := mobile{"Apple"}
	l := laptop{"Intel i9"}
	t := toaster{4}
	k := kettle{"50%"}

	s := socket{}

	s.Plug(m, 10)
	s.Plug(l, 50)
	s.Plug(t, 30)
	s.Plug(k, 25)
}
```

And it works just as we expected.

```bash
$ go run main.go
main.mobile -> brand: Apple, power: 10
main.laptop -> cpu: Intel i9, power: 50
main.toaster -> amount: 4, power: 30
main.kettle -> quantity: Half Empty, power: 25
```

**But why is this considered such a powerful concept?**

Well, an interface can help us decouple our types. For example, because we have the interface, we don't need to update our `socket` implementation. We can just define a new device type with a `Draw` method.

Unlike other languages, Go Interfaces are implemented **implicitly**, so we don't need something like an `implements` keyword. This means that a type satisfies an interface automatically when it has _"all the methods"_ of the interface.

## Empty Interface

Next, let's talk about the empty interface. An empty interface can take on a value of any type.

Here's how we declare it.

```go
var x interface{}
```

**But why do we need it?**

Empty interfaces can be used to handle values of unknown types.

Some examples are:

- Reading heterogeneous data from an API.
- Variables of an unknown type, like in the `fmt.Println` function.

To use a value of type empty `interface{}`, we can use _type assertion_ or a _type switch_ to determine the type of the value.

## Type Assertion

A _type assertion_ provides access to an interface value's underlying concrete value.

For example:

```go
func main() {
	var i interface{} = "hello"

	s := i.(string)
	fmt.Println(s)
}
```

This statement asserts that the interface value holds a concrete type and assigns the underlying type value to the variable.

We can also test whether an interface value holds a specific type.

A type assertion can return two values:

- The first one is the underlying value.
- The second is a boolean value that reports whether the assertion succeeded.

```go
s, ok := i.(string)
fmt.Println(s, ok)
```

This can help us test whether an interface value holds a specific type or not.

In a way, this is similar to how we read values from a map.

And If this is not the case then, `ok` will be false and the value will be the zero value of the type, and no panic will occur.

```go
f, ok := i.(float64)
fmt.Println(f, ok)
```

But if the interface does not hold the type, the statement will trigger a panic.

```go
f = i.(float64)
fmt.Println(f) // Panic!
```

```bash
$ go run main.go
hello
hello true
0 false
panic: interface conversion: interface {} is string, not float64
```

## Type Switch

Here, a `switch` statement can be used to determine the type of a variable of type empty `interface{}`.

```go
var t interface{}
t = "hello"

switch t := t.(type) {
case string:
	fmt.Printf("string: %s\n", t)
case bool:
	fmt.Printf("boolean: %v\n", t)
case int:
	fmt.Printf("integer: %d\n", t)
default:
	fmt.Printf("unexpected: %T\n", t)
}
```

And if we run this, we can verify that we have a `string` type.

```bash
$ go run main.go
string: hello
```

## Properties

Let's discuss some properties of interfaces.

### Zero value

The zero value of an interface is `nil`.

```go
package main

import "fmt"

type MyInterface interface {
	Method()
}

func main() {
	var i MyInterface

	fmt.Println(i) // Output: <nil>
}
```

### Embedding

We can embed interfaces like structs. For example:

```go
type interface1 interface {
    Method1()
}

type interface2 interface {
    Method2()
}

type interface3 interface {
    interface1
    interface2
}
```

### Values

Interface values are comparable.

```go
package main

import "fmt"

type MyInterface interface {
	Method()
}

type MyType struct{}

func (MyType) Method() {}

func main() {
	t := MyType{}
	var i MyInterface = MyType{}

	fmt.Println(t == i)
}
```

### Interface Values

Under the hood, an interface value can be thought of as a tuple consisting of a value and a concrete type.

```go
package main

import "fmt"

type MyInterface interface {
	Method()
}

type MyType struct {
	property int
}

func (MyType) Method() {}

func main() {
	var i MyInterface

	i = MyType{10}

	fmt.Printf("(%v, %T)\n", i, i) // Output: ({10}, main.MyType)
}
```

With that, we covered interfaces in Go.

It's a really powerful feature, but remember, _"Bigger the interface, the weaker the abstraction"_ - Rob Pike.

# Errors

In this tutorial, let's talk about error handling.

Notice how I said errors and not exceptions as there is no exception handling in Go.

Instead, we can just return a built-in `error` type which is an interface type.

```go
type error interface {
    Error() string
}
```

We will circle back to this shortly. First, let's try to understand the basics.

So, let's declare a simple `Divide` function which, as the name suggests, will divide integer `a` by `b`.

```go
func Divide(a, b int) int {
	return a/b
}
```

Great. Now, we want to return an error, let's say, to prevent the division by zero. This brings us to error construction.

## Constructing Errors

There are multiple ways to do this, but we will look at the two most common ones.

### `errors` package

The first is by using the `New` function provided by the `errors` package.

```go
package main

import "errors"

func main() {}

func Divide(a, b int) (int, error) {
	if b == 0 {
		return 0, errors.New("cannot divide by zero")
	}

	return a/b, nil
}
```

Notice, how we return an `error` with the result. And if there is no error we simply return `nil` as it is the zero value of an error because after all, it's an interface.

But how do we handle it? So, for that, let's call the `Divide` function in our `main` function.

```go
package main

import (
	"errors"
	"fmt"
)

func main() {
	result, err := Divide(4, 0)

	if err != nil {
		fmt.Println(err)
		// Do something with the error
		return
	}

	fmt.Println(result)
	// Use the result
}

func Divide(a, b int) (int, error) {...}
```

```bash
$ go run main.go
cannot divide by zero
```

As you can see, we simply check if the error is `nil` and build our logic accordingly. This is considered quite idiomatic in Go and you will see this being used a lot.

Another way to construct our errors is by using the `fmt.Errorf` function.

This function is similar to `fmt.Sprintf` and it lets us format our error. But instead of returning a string, it returns an error.

It is often used to add some context or detail to our errors.

```go
...
func Divide(a, b int) (int, error) {
	if b == 0 {
		return 0, fmt.Errorf("cannot divide %d by zero", a)
	}

	return a/b, nil
}
```

And it should work similarly.

```bash
$ go run main.go
cannot divide 4 by zero
```

### Sentinel Errors

Another important technique in Go is defining expected Errors so they can be checked explicitly in other parts of the code. These are sometimes referred to as sentinel errors.

```go
package main

import (
	"errors"
	"fmt"
)

var ErrDivideByZero = errors.New("cannot divide by zero")

func main() {...}

func Divide(a, b int) (int, error) {
	if b == 0 {
		return 0, ErrDivideByZero
	}

	return a/b, nil
}
```

In Go, it is considered conventional to prefix the variable with `Err`. For example, `ErrNotFound`.

**But what's the point?**

So, this becomes useful when we need to execute a different branch of code if a certain kind of error is encountered.

For example, now we can check explicitly which error occurred using the `errors.Is` function.

```go
package main

import (
	"errors"
	"fmt"
)

func main() {
	result, err := Divide(4, 0)

	if err != nil {
		switch {
    case errors.Is(err, ErrDivideByZero):
        fmt.Println(err)
				// Do something with the error
    default:
        fmt.Println("no idea!")
    }

		return
	}

	fmt.Println(result)
	// Use the result
}

func Divide(a, b int) (int, error) {...}
```

```bash
$ go run main.go
cannot divide by zero
```

## Custom Errors

This strategy covers most of the error handling use cases. But sometimes we need additional functionalities such as dynamic values inside of our errors.

Earlier, we saw that `error` is just an interface. So basically, anything can be an `error` as long as it implements the `Error()` method which returns an error message as a string.

So, let's define our custom `DivisionError` struct which will contain an error code and a message.

```go
package main

import (
	"errors"
	"fmt"
)

type DivisionError struct {
	Code int
	Msg  string
}

func (d DivisionError) Error() string {
	return fmt.Sprintf("code %d: %s", d.Code, d.Msg)
}

func main() {...}

func Divide(a, b int) (int, error) {
	if b == 0 {
		return 0, DivisionError{
			Code: 2000,
			Msg:  "cannot divide by zero",
		}
	}

	return a/b, nil
}
```

Here, we will use `errors.As` instead of `errors.Is` function to convert the error to the correct type.

```go
func main() {
	result, err := Divide(4, 0)

	if err != nil {
		var divErr DivisionError

		switch {
		case errors.As(err, &divErr):
			fmt.Println(divErr)
			// Do something with the error
		default:
			fmt.Println("no idea!")
		}

		return
	}

	fmt.Println(result)
	// Use the result
}

func Divide(a, b int) (int, error) {...}
```

```bash
$ go run main.go
code 2000: cannot divide by zero
```

**But what's the difference between `errors.Is` and `errors.As`?**

The difference is that this function checks whether the error has a specific type, unlike the [`Is`](https://pkg.go.dev/errors#Is) function, which examines if it is a particular error object.

We can also use type assertions but it's not preferred.

```go
func main() {
	result, err := Divide(4, 0)

	if e, ok := err.(DivisionError); ok {
		fmt.Println(e.Code, e.Msg) // Output: 2000 cannot divide by zero
		return
	}

	fmt.Println(result)
}
```

Lastly, I will say that error handling in Go is quite different compared to the traditional `try/catch` idiom in other languages. But it is very powerful as it encourages the developer to actually handle the error in an explicit way, which improves readability as well.

# Panic and Recover

So earlier, we learned that the idiomatic way of handling abnormal conditions in a Go program is using errors. While errors are sufficient for most cases, there are some situations where the program cannot continue.

In those cases, we can use the built-in `panic` function.

## Panic

```go
func panic(interface{})
```

The panic is a built-in function that stops the normal execution of the current `goroutine`. When a function calls `panic`, the normal execution of the function stops immediately and the control is returned to the caller. This is repeated until the program exits with the panic message and stack trace.

_Note: We will discuss `goroutines` later in the course._

So, let's see how we can use the `panic` function.

```go
package main

func main() {
	WillPanic()
}

func WillPanic() {
	panic("Woah")
}
```

And if we run this, we can see `panic` in action.

```bash
$ go run main.go
panic: Woah

goroutine 1 [running]:
main.WillPanic(...)
        .../main.go:8
main.main()
        .../main.go:4 +0x38
exit status 2
```

As expected, our program printed the panic message, followed by the stack trace, and then it was terminated.

So, the question is, what to do when an unexpected panic happens?

## Recover

Well, it is possible to regain control of a panicking program using the built-in `recover` function, along with the `defer` keyword.

```go
func recover() interface{}
```

Let's try an example by creating a `handlePanic` function. And then, we can call it using `defer`.

```go
package main

import "fmt"

func main() {
	WillPanic()
}

func handlePanic() {
	data := recover()
	fmt.Println("Recovered:", data)
}

func WillPanic() {
	defer handlePanic()

	panic("Woah")
}
```

```bash
$ go run main.go
Recovered: Woah
```

As we can see, our panic was recovered and now our program can continue execution.

Lastly, I will mention that `panic` and `recover` can be considered similar to the `try/catch` idiom in other languages. But one important factor is that we should avoid panic and recover and use [errors](https://karanpratapsingh.com/courses/go/errors) when possible.

If so, then this brings us to the question, when should we use `panic`?

## Use Cases

There are two valid use cases for `panic`:

- **An unrecoverable error**

Which can be a situation where the program cannot simply continue its execution.

For example, reading a configuration file which is important to start the program, as there is nothing else to do if the file read itself fails.

- **Developer error**

This is the most common situation. For example, dereferencing a pointer when the value is `nil` will cause a panic.

# Testing

In this tutorial, we will talk about testing in Go. So, let's start using a simple example.

We have created a `math` package that contains an `Add` function Which as the name suggests, adds two integers.

```go
package math

func Add(a, b int) int {
	return a + b
}
```

It's being used in our `main` package like this.

```go
package main

import (
	"example/math"
	"fmt"
)

func main() {
	result := math.Add(2, 2)
	fmt.Println(result)
}

```

And, if we run this, we should see the result.

```bash
$ go run main.go
4
```

Now, we want to test our `Add` function. So, in Go, we declare test files with `_test` suffix in the file name. So for our `add.go`, we will create a test as `add_test.go`. Our project structure should look like this.

```bash
.
├── go.mod
├── main.go
└── math
    ├── add.go
    └── add_test.go
```

We will start by using a `math_test` package, and importing the `testing` package from the standard library. That's right! Testing is built into Go, unlike many other languages.

But wait...why do we need to use `math_test` as our package, can't we just use the same `math` package?

Well yes, we can write our test in the same package if we wanted, but I personally think doing this in a separate package helps us write tests in a more decoupled way.

Now, we can create our `TestAdd` function. It will take an argument of type `testing.T` which will provide us with helpful methods.

```go
package math_test

import "testing"

func TestAdd(t *testing.T) {}
```

Before we add any testing logic, let's try to run it. But this time, we cannot use `go run` command, instead, we will use the `go test` command.

```bash
$ go test ./math
ok      example/math 0.429s
```

Here, we will have our package name which is `math`, but we can also use the relative path `./...` to test all packages.

```bash
$ go test ./...
?       example [no test files]
ok      example/math 0.348s
```

And if Go doesn't find any test in a package, it will let us know.

Perfect, let's write some test code. To do this, we will check our result with an expected value and if they do not match, we can use the `t.Fail` method to fail the test.

```go
package math_test

import "testing"

func TestAdd(t *testing.T) {
	got := math.Add(1, 1)
	expected := 2

	if got != expected {
		t.Fail()
	}
}
```

Great! Our test seems to have passed.

```bash
$ go test math
ok      example/math    0.412s
```

Let's also see what happens if we fail the test, for that, we can simply change our expected result.

```go
package math_test

import "testing"

func TestAdd(t *testing.T) {
	got := math.Add(1, 1)
	expected := 3

	if got != expected {
		t.Fail()
	}
}
```

```bash
$ go test ./math
ok      example/math    (cached)
```

If you see this, don't worry. For optimization, our tests are cached. We can use the `go clean` command to clear our cache and then re-run the test.

```bash
$ go clean -testcache
$ go test ./math
--- FAIL: TestAdd (0.00s)
FAIL
FAIL    example/math    0.354s
FAIL
```

So, this is what a test failure will look like.

## Table driven tests

This brings us to table-driven tests. But what exactly are they?

So earlier, we had function arguments and expected variables which we compared to determine if our tests passed or fail. But what if we defined all that in a slice and iterate over that? This will make our tests a little bit more flexible and help us run multiple cases easily.

Don't worry, we will learn this by example. So we will start by defining our `addTestCase` struct.

```go
package math_test

import (
	"example/math"
	"testing"
)

type addTestCase struct {
	a, b, expected int
}

var testCases = []addTestCase{
	{1, 1, 3},
	{25, 25, 50},
	{2, 1, 3},
	{1, 10, 11},
}

func TestAdd(t *testing.T) {

	for _, tc := range testCases {
		got := math.Add(tc.a, tc.b)

		if got != tc.expected {
			t.Errorf("Expected %d but got %d", tc.expected, got)
		}
	}
}
```

Notice, how we declared `addTestCase` with a lower case. That's right we don't want to export it as it's not useful outside our testing logic. Let's run our test.

```bash
$ go run main.go
--- FAIL: TestAdd (0.00s)
    add_test.go:25: Expected 3 but got 2
FAIL
FAIL    example/math    0.334s
FAIL
```

Seems like our tests broke, let's fix them by updating our test cases.

```go
var testCases = []addTestCase{
	{1, 1, 2},
	{25, 25, 50},
	{2, 1, 3},
	{1, 10, 11},
}
```

Perfect, it's working!

```bash
$ go run main.go
ok      example/math    0.589s
```

## Code coverage

Finally, let's talk about code coverage. When writing tests, it is often important to know how much of your actual code the tests cover. This is generally referred to as code coverage.

To calculate and export the coverage for our test, we can simply use the `-coverprofile` argument with the `go test` command.

```bash
$ go test ./math -coverprofile=coverage.out
ok      example/math    0.385s  coverage: 100.0% of statements
```

Seems like we have great coverage. Let's also check the report using the `go tool cover` command which gives us a detailed report.

```bash
$ go tool cover -html=coverage.out
```

![coverage](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-III/testing/coverage.png)

As we can see, this is a much more readable format. And best of all, it is built right into standard tooling.

## Fuzz testing

Lastly, let's look at fuzz testing which was introduced in Go version 1.18.

Fuzzing is a type of automated testing that continuously manipulates inputs to a program to find bugs.

Go fuzzing uses coverage guidance to intelligently walk through the code being fuzzed to find and report failures to the user.

Since it can reach edge cases that humans often miss, fuzz testing can be particularly valuable for finding bugs and security exploits.

Let's try an example:

```go
func FuzzTestAdd(f *testing.F) {
	f.Fuzz(func(t *testing.T, a, b int) {
		math.Add(a , b)
	})
}
```

If we run this, we'll see that it'll automatically create test cases. Because our `Add` function is quite simple, tests will pass.

```bash
$ go test -fuzz FuzzTestAdd example/math
fuzz: elapsed: 0s, gathering baseline coverage: 0/192 completed
fuzz: elapsed: 0s, gathering baseline coverage: 192/192 completed, now fuzzing with 8 workers
fuzz: elapsed: 3s, execs: 325017 (108336/sec), new interesting: 11 (total: 202)
fuzz: elapsed: 6s, execs: 680218 (118402/sec), new interesting: 12 (total: 203)
fuzz: elapsed: 9s, execs: 1039901 (119895/sec), new interesting: 19 (total: 210)
fuzz: elapsed: 12s, execs: 1386684 (115594/sec), new interesting: 21 (total: 212)
PASS
ok      foo 12.692s
```

But if we update our `Add` function with a random edge case such that the program will panic if `b + 10` is greater than `a`.

```go
func Add(a, b int) int {
	if a > b + 10 {
		panic("B must be greater than A")
	}

	return a + b
}
```

And if we re-run the test, this edge case will be caught by fuzz testing.

```bash
$ go test -fuzz FuzzTestAdd example/math
warning: starting with empty corpus
fuzz: elapsed: 0s, execs: 0 (0/sec), new interesting: 0 (total: 0)
fuzz: elapsed: 0s, execs: 1 (25/sec), new interesting: 0 (total: 0)
--- FAIL: FuzzTestAdd (0.04s)
    --- FAIL: FuzzTestAdd (0.00s)
        testing.go:1349: panic: B is greater than A
```

I think this is a really cool feature of Go 1.18. You can learn more about fuzz testing from the [official Go blog](https://go.dev/doc/fuzz).

# Generics

In this section, we will learn about Generics which is a much awaited feature that was released with Go version 1.18.

## What are Generics?

Generics means parameterized types. Put simply, generics allow programmers to write code where the type can be specified later because the type isn't immediately relevant.

Let's take a look at an example to understand this better.

For our example, we have simple sum functions for different types such as `int`, `float64`, and `string`. Since method overriding is not allowed in Go we usually have to create new functions.

```go
package main

import "fmt"

func sumInt(a, b int) int {
	return a + b
}

func sumFloat(a, b float64) float64 {
	return a + b
}

func sumString(a, b string) string {
	return a + b
}

func main() {
	fmt.Println(sumInt(1, 2))
	fmt.Println(sumFloat(4.0, 2.0))
	fmt.Println(sumString("a", "b"))
}
```

As we can see, apart from the types, these functions are pretty similar.

Let's see how we can define a generic function.

```go
func fnName[T constraint]() {
	...
}
```

Here, `T` is our type parameter and `constraint` will be the interface that allows any type implementing the interface.

I know, I know, this is confusing. So, let's start building our generic `sum` function.

Here, we will use `T` as our type parameter with an empty `interface{}` as our constraint.

```go
func sum[T interface{}](a, b T) T {
	fmt.Println(a, b)
}
```

Also, starting with Go 1.18 we can use `any`, which is pretty much equivalent to the empty interface.

```go
func sum[T any](a, b T) T {
	fmt.Println(a, b)
}
```

With type parameters, comes the need to pass type arguments, which can make our code verbose.

```go
sum[int](1, 2) // explicit type argument
sum[float64](4.0, 2.0)
sum[string]("a", "b")
```

Luckily, Go 1.18 comes with **type inference** which helps us to write code that calls generic functions without explicit types.

```go
sum(1, 2)
sum(4.0, 2.0)
sum("a", "b")
```

Let's run this and see if it works.

```bash
$ go run main.go
1 2
4 2
a b
```

Now, let's update the `sum` function to add our variables.

```go
func sum[T any](a, b T) T {
	return a + b
}
```

```go
fmt.Println(sum(1, 2))
fmt.Println(sum(4.0, 2.0))
fmt.Println(sum("a", "b"))
```

But now if we run this, we will get an error that operator `+` is not defined in the constraint.

```bash
$ go run main.go
./main.go:6:9: invalid operation: operator + not defined on a (variable of type T constrained by any)
```

While constraint of type `any` generally works it does not support operators.

So let's define our own custom constraint using an interface. Our interface should define a type set containing `int`, `float`, and `string`.

![typeset](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-III/generics/typeset.png)

Here's how our `SumConstraint` interface looks.

```go
type SumConstraint interface {
	int | float64 | string
}

func sum[T SumConstraint](a, b T) T {
	return a + b
}

func main() {
	fmt.Println(sum(1, 2))
	fmt.Println(sum(4.0, 2.0))
	fmt.Println(sum("a", "b"))
}
```

And this should work as expected.

```bash
$ go run main.go
3
6
ab
```

We can also use the `constraints` package which defines a set of useful constraints to be used with type parameters.

```go
type Signed interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64
}

type Unsigned interface {
	~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | ~uintptr
}

type Integer interface {
	Signed | Unsigned
}

type Float interface {
	~float32 | ~float64
}

type Complex interface {
	~complex64 | ~complex128
}

type Ordered interface {
	Integer | Float | ~string
}
```

For that, we will need to install the `constraints` package.

```bash
$ go get golang.org/x/exp/constraints
go: added golang.org/x/exp v0.0.0-20220414153411-bcd21879b8fd
```

```go
import (
	"fmt"

	"golang.org/x/exp/constraints"
)

func sum[T constraints.Ordered](a, b T) T {
	return a + b
}

func main() {
	fmt.Println(sum(1, 2))
	fmt.Println(sum(4.0, 2.0))
	fmt.Println(sum("a", "b"))
}
```

Here we are using the `Ordered` constraint.

```go
type Ordered interface {
	Integer | Float | ~string
}
```

`~` is a new token added to Go and the expression `~string` means the set of all types whose underlying type is `string`.

And it still works as expected.

```bash
$ go run main.go
3
6
ab
```

Generics is an amazing feature because it permits writing abstract functions that can drastically reduce code duplication in certain cases.

## When to use generics

So, when to use generics? We can take the following use cases as an example:

- Functions that operate on arrays, slices, maps, and channels.
- General purpose data structures like stack or linked list.
- To reduce code duplication.

Lastly, I will add that while generics are a great addition to the language, they should be used sparingly.

And, it is advised to start simple and only write generic code once we have written very similar code at least 2 or 3 times.

# Concurrency

In this lesson, we will learn about concurrency which is one of the most powerful features of Go.

So, let's start by asking What is _"concurrency"_?

## What is Concurrency

Concurrency, by definition, is the ability to break down a computer program or algorithm into individual parts, which can be executed independently.

The final outcome of a concurrent program is the same as that of a program that has been executed sequentially.

Using concurrency, we can achieve the same results in lesser time, thus increasing the overall performance and efficiency of our programs.

## Concurrency vs Parallelism

![concurrency-vs-parallelism](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/concurrency/concurrency-vs-parallelism.png)

A lot of people confuse concurrency with parallelism because they both somewhat imply executing code simultaneously, but they are two completely different concepts.

Concurrency is the task of running and managing multiple computations at the same time, while parallelism is the task of running multiple computations simultaneously.

A simple quote from Rob Pike pretty much sums it up.

_"Concurrency is about dealing with lots of things at once. Parallelism is about doing lots of things at once"_

But concurrency in Go is more than just syntax. In order to harness the power of Go, we need to first understand how Go approaches concurrent execution of code. Go relies on a concurrency model called CSP (Communicating Sequential Processes).

## Communicating Sequential Processes (CSP)

[Communicating Sequential Processes](https://dl.acm.org/doi/10.1145/359576.359585) (CSP) is a model put forth by [Tony Hoare](https://en.wikipedia.org/wiki/Tony_Hoare) in 1978 which describes interactions between concurrent processes. It made a breakthrough in Computer Science, especially in the field of concurrency.

Languages like Go and Erlang have been highly inspired by the concept of communicating sequential processes (CSP).

Concurrency is hard, but CSP allows us to give a better structure to our concurrent code and provides a model for thinking about concurrency in a way that makes it a little easier. Here, processes are independent and they communicate by sharing channels between them.

![csp](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/concurrency/csp.png)

_We'll learn how Golang implements it using goroutines and channels later in the course._

## Basic Concepts

Now, let's get familiar with some basic concurrency concepts.

### Data Race

A data race occurs when processes have to access the same resource concurrently.

_For example, one process reads while another simultaneously writes to the exact same resource._

### Race Conditions

A race condition occurs when the timing or order of events affects the correctness of a piece of code.

### Deadlocks

A deadlock occurs when all processes are blocked while waiting for each other and the program cannot proceed further.

**Coffman Conditions**

There are four conditions, known as the Coffman conditions, all of them must be satisfied for a deadlock to occur.

- Mutual Exclusion

A concurrent process holds at least one resource at any one time making it non-sharable.

_In the diagram below, there is a single instance of Resource 1 and it is held by Process 1 only._

![mutual-exclusion](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/concurrency/mutual-exclusion.png)

- Hold and wait

A concurrent process holds a resource and is waiting for an additional resource.

_In the diagram given below, Process 2 holds Resource 2 and Resource 3 and is requesting the Resource 1 which is held by Process 1._

![hold-and-wait](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/concurrency/hold-and-wait.png)

- No preemption

A resource held by a concurrent process cannot be taken away by the system. It can only be freed by the process holding it.

_In the diagram below, Process 2 cannot preempt Resource 1 from Process 1. It will only be released when Process 1 relinquishes it voluntarily after its execution is complete._

![no-preemption](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/concurrency/no-preemption.png)

- Circular wait

A process is waiting for the resource held by the second process, which is waiting for the resource held by the third process, and so on, till the last process is waiting for a resource held by the first process. Hence, forming a circular chain.

_In the diagram below, Process 1 is allocated Resource2 and it is requesting Resource 1. Similarly, Process 2 is allocated Resource 1 and it is requesting Resource 2. This forms a circular wait loop._

![circular-wait](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/concurrency/circular-wait.png)

### Livelocks

Livelocks are processes that are actively performing concurrent operations, but these operations do nothing to move the state of the program forward.

### Starvation

Starvation happens when a process is deprived of necessary resources and is unable to complete its function.

Starvation can happen because of deadlocks or inefficient scheduling algorithms for processes. In order to solve starvation, we need to employ better resource-allotment algorithms that make sure that every process gets its fair share of resources.

# Goroutines

In this lesson, we will learn about Goroutines.

But before we start our discussion, I wanted to share an important Go proverb.

_"Don't communicate by sharing memory, share memory by communicating."_ - Rob Pike

## What is a goroutine?

A _goroutine_ is a lightweight thread of execution that is managed by the Go runtime and essentially let us write asynchronous code in a synchronous manner.

It is important to know that they are not actual OS threads and the main function itself runs as a goroutine.

A single thread may run thousands of goroutines in them by using the Go runtime scheduler which uses cooperative scheduling. This implies that if the current goroutine is blocked or has been completed, the scheduler will move the other goroutines to another OS thread. Hence, we achieve efficiency in scheduling where no routine is blocked forever.

We can turn any function into a goroutine by simply using the `go` keyword.

```go
go fn(x, y, z)
```

Before we write any code, it is important to briefly discuss the fork-join model.

## Fork-Join Model

Go uses the idea of the fork-join model of concurrency behind goroutines. The fork-join model essentially implies that a child process splits from its parent process to run concurrently with the parent process. After completing its execution, the child process merges back into the parent process. The point where it joins back is called the **join point**.

![fork-join](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/goroutines/fork-join.png)

Now, let's write some code and create our own goroutine.

```go
package main

import "fmt"

func speak(arg string) {
	fmt.Println(arg)
}

func main() {
	go speak("Hello World")
}
```

Here the `speak` function call is prefixed with the `go` keyword. This will allow it to run as a separate goroutine. And that's it, we just created our first goroutine. It's that simple!

Great, let's run this:

```bash
$ go run main.go
```

Interesting, it seems like our program did not run completely as it's missing some output. This is because our main goroutine exited and did not wait for the goroutine that we created.

What if we make our program wait using the `time.Sleep` function?

```go
func main() {
	...
	time.Sleep(1 * time.Second)
}
```

```bash
$ go run main.go
Hello World
```

There we go, we can see our complete output now.

**Okay, so this works but it's not ideal. So how do we improve this?**

Well, the most tricky part about using goroutines is knowing when they will stop. It is important to know that goroutines run in the same address space, so access to shared memory must be synchronized.

# Channels

In this lesson, we will learn about Channels.

## So what are channels?

Well, simply defined a channel is a communications pipe between goroutines. Things go in one end and come out another in the same order until the channel is closed.

![channel](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/channels/channel.png)

As we learned earlier, channels in Go are based on Communicating Sequential Processes (CSP).

## Creating a channel

Now that we understand what channels are, let's see how we can declare them.

```go
var ch chan T
```

Here, we prefix our type `T` which is the data type of the value we want to send and receive with the keyword `chan` which stands for a channel.

Let's try printing the value of our channel `c` of type `string`.

```go
func main() {
	var ch chan string

	fmt.Println(c)
}
```

```bash
$ go run main.go
<nil>
```

As we can see, the zero value of a channel is `nil` and if we try to send data over the channel our program will panic.

So, similar to slices we can initialize our channel using the built-in `make` function.

```go
func main() {
	ch := make(chan string)

	fmt.Println(c)
}
```

And if we run this, we can see our channel was initialized.

```bash
$ go run main.go
0x1400010e060
```

## Sending and Receiving data

Now that we have a basic understanding of channels, let us implement our earlier example using channels to learn how we can use them to communicate between our goroutines.

```go
package main

import "fmt"

func speak(arg string, ch chan string) {
	ch <- arg // Send
}

func main() {
	ch := make(chan string)

	go speak("Hello World", ch)

	data := <-ch // Receive
	fmt.Println(data)
}
```

Notice how we can send data using the `channel<-data` and receive data using the `data := <-channel` syntax.

```bash
$ go run main.go
Hello World
```

Perfect, our program ran as we expected.

## Buffered Channels

We also have buffered channels that accept a limited number of values without a corresponding receiver for those values.

![buffered-channel](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/channels/buffered-channel.png)

This _buffer length_ or _capacity_ can be specified using the second argument to the `make` function.

```go
func main() {
	ch := make(chan string, 2)

	go speak("Hello World", ch)
	go speak("Hi again", ch)

	data1 := <-ch
	fmt.Println(data1)

	data2 := <-ch
	fmt.Println(data2)
}
```

Because this channel is buffered, we can send these values into the channel without a corresponding concurrent receive. This means _sends_ to a buffered channel block only when the buffer is full and _receives_ block when the buffer is empty.

By default, a channel is unbuffered and has a capacity of 0, hence, we omit the second argument of the `make` function.

Next, we have directional channels.

## Directional channels

When using channels as function parameters, we can specify if a channel is meant to only send or receive values. This increases the type-safety of our program as by default a channel can both send and receive values.

![directional-channels](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/channels/directional-channels.png)

In our example, we can update our `speak` function's second argument such that it can only send a value.

```go
func speak(arg string, ch chan<- string) {
	ch <- arg // Send Only
}
```

Here, `chan<-` can only be used for sending values and will panic if we try to receive values.

## Closing channels

Also, just like any other resource, once we're done with our channel, we need to close it. This can be achieved using the built-in `close` function.

Here, we can just pass our channel to the `close` function.

```go
func main() {
	ch := make(chan string, 2)

	go speak("Hello World", ch)
	go speak("Hi again", ch)

	data1 := <-ch
	fmt.Println(data1)

	data2 := <-ch
	fmt.Println(data2)

	close(ch)
}
```

Optionally, receivers can test whether a channel has been closed by assigning a second parameter to the receive expression.

```go
func main() {
	ch := make(chan string, 2)

	go speak("Hello World", ch)
	go speak("Hi again", ch)

	data1 := <-ch
	fmt.Println(data1)

	data2, ok := <-ch
	fmt.Println(data2, ok)

	close(ch)
}
```

if `ok` is `false` then there are no more values to receive and the channel is closed.

_In a way, this is similar to how we check if a key exists or not in a map._

## Properties

Lastly, let's discuss some properties of channels:

- A send to a `nil` channel blocks forever.

```go
var c chan string
c <- "Hello, World!" // Panic: all goroutines are asleep - deadlock!
```

- A receive from a `nil` channel blocks forever.

```go
var c chan string
fmt.Println(<-c) // Panic: all goroutines are asleep - deadlock!
```

- A send to a closed channel causes a panic.

```go
var c = make(chan string, 1)
c <- "Hello, World!"
close(c)
c <- "Hello, Panic!" // Panic: send on closed channel
```

- A receive from a closed channel returns the zero value immediately.

```go
var c = make(chan int, 2)
c <- 5
c <- 4
close(c)
for i := 0; i < 4; i++ {
    fmt.Printf("%d ", <-c) // Output: 5 4 0 0
}
```

- Range over channels.

We can also use `for` and `range` to iterate over values received from a channel.

```go
package main

import "fmt"

func main() {
	ch := make(chan string, 2)

	ch <- "Hello"
	ch <- "World"

	close(ch)

	for data := range ch {
		fmt.Println(data)
	}
}
```

# Select

In this tutorial, we will learn about the `select` statement in Go.

The `select` statement blocks the code and waits for multiple channel operations simultaneously.

A `select` blocks until one of its cases can run, then it executes that case. It chooses one at random if multiple are ready.

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	one := make(chan string)
	two := make(chan string)

	go func() {
		time.Sleep(time.Second * 2)
		one <- "One"
	}()

	go func() {
		time.Sleep(time.Second * 1)
		two <- "Two"
	}()

	select {
	case result := <-one:
		fmt.Println("Received:", result)
	case result := <-two:
		fmt.Println("Received:", result)
	}

	close(one)
	close(two)
}
```

Similar to `switch`, `select` also has a default case that runs if no other case is ready. This will help us send or receive without blocking.

```go
func main() {
	one := make(chan string)
	two := make(chan string)

	for x := 0; x < 10; x++ {
		go func() {
			time.Sleep(time.Second * 2)
			one <- "One"
		}()

		go func() {
			time.Sleep(time.Second * 1)
			two <- "Two"
		}()
	}

	for x := 0; x < 10; x++ {
		select {
		case result := <-one:
			fmt.Println("Received:", result)
		case result := <-two:
			fmt.Println("Received:", result)
		default:
			fmt.Println("Default...")
			time.Sleep(200 * time.Millisecond)
		}
	}

	close(one)
	close(two)
}
```

It's also important to know that an empty `select {}` blocks forever.

```go
func main() {
	...
	select {}

	close(one)
	close(two)
}
```

# Sync Package

As we learned earlier, goroutines run in the same address space, so access to shared memory must be synchronized. The [`sync`](https://go.dev/pkg/sync) package provides useful primitives.

## WaitGroup

A WaitGroup waits for a collection of goroutines to finish. The main goroutine calls `Add` to set the number of goroutines to wait for. Then each of the goroutines runs and calls `Done` when finished. At the same time, `Wait` can be used to block until all goroutines have finished.

### Usage

We can use the `sync.WaitGroup` using the following methods:

- `Add(delta int)` takes in an integer value which is essentially the number of goroutines that the `WaitGroup` has to wait for. This must be called before we execute a goroutine.
- `Done()` is called within the goroutine to signal that the goroutine has successfully executed.
- `Wait()` blocks the program until all the goroutines specified by `Add()` have invoked `Done()` from within.

### Example

Let's take a look at an example.

```go
package main

import (
	"fmt"
	"sync"
)

func work() {
	fmt.Println("working...")
}

func main() {
	var wg sync.WaitGroup

	wg.Add(1)
	go func() {
		defer wg.Done()
		work()
	}()

	wg.Wait()
}
```

If we run this, we can see our program runs as expected.

```bash
$ go run main.go
working...
```

We can also pass the `WaitGroup` to the function directly.

```go
func work(wg *sync.WaitGroup) {
	defer wg.Done()
	fmt.Println("working...")
}

func main() {
	var wg sync.WaitGroup

	wg.Add(1)

	go work(&wg)

	wg.Wait()
}
```

But is important to know that a `WaitGroup` **must not be copied** after first use. And if it's explicitly passed into functions, it should be done by a _pointer._ This is because it can affect our counter which will disrupt the logic of our program.

Let's also increase the number of goroutines by calling the `Add` method to wait for 4 goroutines.

```go
func main() {
	var wg sync.WaitGroup

	wg.Add(4)

	go work(&wg)
	go work(&wg)
	go work(&wg)
	go work(&wg)

	wg.Wait()
}
```

And as expected, all our goroutines were executed.

```bash
$ go run main.go
working...
working...
working...
working...
```

## Mutex

A Mutex is a mutual exclusion lock that prevents other processes from entering a critical section of data while a process occupies it to prevent race conditions from happening.

### What's a critical section?

So, a critical section can be a piece of code that must not be run by multiple threads at once because the code contains shared resources.

### Usage

We can use `sync.Mutex` using the following methods:

- `Lock()` acquires or holds the lock.
- `Unlock()` releases the lock.
- `TryLock()` tries to lock and reports whether it succeeded.

### Example

Let's take a look at an example, we will create a `Counter` struct and add an `Update` method which will update the internal value.

```go
package main

import (
	"fmt"
	"sync"
)

type Counter struct {
	value int
}

func (c *Counter) Update(n int, wg *sync.WaitGroup) {
	defer wg.Done()
	fmt.Printf("Adding %d to %d\n", n, c.value)
	c.value += n
}

func main() {
	var wg sync.WaitGroup

	c := Counter{}

	wg.Add(4)

	go c.Update(10, &wg)
	go c.Update(-5, &wg)
	go c.Update(25, &wg)
	go c.Update(19, &wg)

	wg.Wait()
	fmt.Printf("Result is %d", c.value)
}
```

Let's run this and see what happens.

```bash
$ go run main.go
Adding -5 to 0
Adding 10 to 0
Adding 19 to 0
Adding 25 to 0
Result is 49
```

That doesn't look accurate, seems like our value is always zero but we somehow got the correct answer.

Well, this is because, in our example, multiple goroutines are updating the `value` variable. And as you must have guessed, this is not ideal.

This is the perfect use case for Mutex. So, let's start by using `sync.Mutex` and wrap our critical section in between `Lock()` and `Unlock()` methods.

```go
package main

import (
	"fmt"
	"sync"
)

type Counter struct {
	m     sync.Mutex
	value int
}

func (c *Counter) Update(n int, wg *sync.WaitGroup) {
	c.m.Lock()
	defer wg.Done()
	fmt.Printf("Adding %d to %d\n", n, c.value)
	c.value += n
	c.m.Unlock()
}

func main() {
	var wg sync.WaitGroup

	c := Counter{}

	wg.Add(4)

	go c.Update(10, &wg)
	go c.Update(-5, &wg)
	go c.Update(25, &wg)
	go c.Update(19, &wg)

	wg.Wait()
	fmt.Printf("Result is %d", c.value)
}
```

```bash
$ go run main.go
Adding -5 to 0
Adding 19 to -5
Adding 25 to 14
Adding 10 to 39
Result is 49
```

Looks like we solved our issue and the output looks correct as well.

_Note: Similar to WaitGroup a Mutex **must not be copied** after first use._

## RWMutex

An RWMutex is a reader/writer mutual exclusion lock. The lock can be held by an arbitrary number of readers or a single writer.

In other words, readers don't have to wait for each other. They only have to wait for writers holding the lock.

`sync.RWMutex` is thus preferable for data that is mostly read, and the resource that is saved compared to a `sync.Mutex` is time.

### Usage

Similar to `sync.Mutex`, we can use `sync.RWMutex` using the following methods:

- `Lock()` acquires or holds the lock.
- `Unlock()` releases the lock.
- `RLock()` acquires or holds the read lock.
- `RUnlock()` releases the read lock.

_Notice how RWMutex has additional `RLock` and `RUnlock` methods compared to Mutex._

### Example

Let's add a `GetValue` method which will read the counter value. We will also change `sync.Mutex` to `sync.RWMutex`.

Now, we can simply use the `RLock` and `RUnlock` methods so that readers don't have to wait for each other.

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

type Counter struct {
	m     sync.RWMutex
	value int
}

func (c *Counter) Update(n int, wg *sync.WaitGroup) {
	defer wg.Done()

	c.m.Lock()
	fmt.Printf("Adding %d to %d\n", n, c.value)
	c.value += n
	c.m.Unlock()
}

func (c *Counter) GetValue(wg *sync.WaitGroup) {
	defer wg.Done()

	c.m.RLock()
	defer c.m.RUnlock()
	fmt.Println("Get value:", c.value)
	time.Sleep(400 * time.Millisecond)
}

func main() {
	var wg sync.WaitGroup

	c := Counter{}

	wg.Add(4)

	go c.Update(10, &wg)
	go c.GetValue(&wg)
	go c.GetValue(&wg)
	go c.GetValue(&wg)

	wg.Wait()
}
```

```bash
$ go run main.go
Get value: 0
Adding 10 to 0
Get value: 10
Get value: 10
```

_Note: Both `sync.Mutex` and `sync.RWMutex` implements the `sync.Locker` interface._

```go
type Locker interface {
    Lock()
    Unlock()
}
```

## Cond

The `sync.Cond` condition variable can be used to coordinate those goroutines that want to share resources. When the state of shared resources changes, it can be used to notify goroutines blocked by a mutex.

Each Cond has an associated lock (often a `*Mutex` or `*RWMutex`), which must be held when changing the condition and when calling the Wait method.

### But why do we need it?

One scenario can be when one process is receiving data, and other processes must wait for this process to receive data before they can read the correct data.

If we simply use a [channel](https://karanpratapsingh.com/courses/go/channels) or mutex, only one process can wait and read the data. There is no way to notify other processes to read the data. Thus, we can `sync.Cond` to coordinate shared resources.

### Usage

`sync.Cond` comes with the following methods:

- `NewCond(l Locker)` returns a new Cond.
- `Broadcast()` wakes all goroutines waiting on the condition.
- `Signal()` wakes one goroutine waiting on the condition if there is any.
- `Wait()` atomically unlocks the underlying mutex lock.

### Example

Here is an example that demonstrates the interaction of different goroutines using the `Cond`.

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

var done = false

func read(name string, c *sync.Cond) {
	c.L.Lock()
	for !done {
		c.Wait()
	}
	fmt.Println(name, "starts reading")
	c.L.Unlock()
}

func write(name string, c *sync.Cond) {
	fmt.Println(name, "starts writing")
	time.Sleep(time.Second)

	c.L.Lock()
	done = true
	c.L.Unlock()

	fmt.Println(name, "wakes all")
	c.Broadcast()
}

func main() {
	var m sync.Mutex
	cond := sync.NewCond(&m)

	go read("Reader 1", cond)
	go read("Reader 2", cond)
	go read("Reader 3", cond)
	write("Writer", cond)

	time.Sleep(4 * time.Second)
}
```

```bash
$ go run main.go
Writer starts writing
Writer wakes all
Reader 2 starts reading
Reader 3 starts reading
Reader 1 starts reading
```

As we can see, the readers were suspended using the `Wait` method until the writer used the `Broadcast` method to wake up the process.

## Once

Once ensures that only one execution will be carried out even among several goroutines.

### Usage

Unlike other primitives, `sync.Once` only has a single method:

- `Do(f func())` calls the function `f` **only once**. If `Do` is called multiple times, only the first call will invoke the function `f`.

### Example

This seems pretty straightforward, let's take an example:

```go
package main

import (
	"fmt"
	"sync"
)

func main() {
	var count int

	increment := func() {
		count++
	}

	var once sync.Once

	var increments sync.WaitGroup
	increments.Add(100)

	for i := 0; i < 100; i++ {
		go func() {
			defer increments.Done()
			once.Do(increment)
		}()
	}

	increments.Wait()
	fmt.Printf("Count is %d\n", count)
}
```

```bash
$ go run main.go
Count is 1
```

As we can see, even when we ran 100 goroutines, the count only got incremented once.

## Pool

Pool is s a scalable pool of temporary objects and is also concurrency safe. Any stored value in the pool can be deleted at any time without receiving notification. In addition, under high load, the object pool can be dynamically expanded, and when it is not used or the concurrency is not high, the object pool will shrink.

_The key idea is the reuse of objects to avoid repeated creation and destruction, which will affect the performance._

### But why do we need it?

Pool's purpose is to cache allocated but unused items for later reuse, relieving pressure on the garbage collector. That is, it makes it easy to build efficient, thread-safe free lists. However, it is not suitable for all free lists.

The appropriate use of a Pool is to manage a group of temporary items silently shared among and potentially reused by concurrent independent clients of a package. Pool provides a way to spread the cost of allocation overhead across many clients.

_It is important to note that Pool also has its performance cost. It is much slower to use `sync.Pool` than simple initialization. Also, a Pool must not be copied after first use._

### Usage

`sync.Pool` gives us the following methods:

- `Get()` selects an arbitrary item from the Pool, removes it from the Pool, and returns it to the caller.
- `Put(x any)` adds the item to the pool.

### Example

Now, let's look at an example.

First, we will create a new `sync.Pool`, where we can optionally specify a function to generate a value when we call, `Get`, otherwise it will return a `nil` value.

```go
package main

import (
	"fmt"
	"sync"
)

type Person struct {
	Name string
}

var pool = sync.Pool{
	New: func() any {
		fmt.Println("Creating a new person...")
		return &Person{}
	},
}

func main() {
	person := pool.Get().(*Person)
	fmt.Println("Get object from sync.Pool for the first time:", person)

	fmt.Println("Put the object back in the pool")
	pool.Put(person)

	person.Name = "Gopher"
	fmt.Println("Set object property name:", person.Name)

	fmt.Println("Get object from pool again (it's updated):", pool.Get().(*Person))
	fmt.Println("There is no object in the pool now (new one will be created):", pool.Get().(*Person))
}
```

And if we run this, we'll see an interesting output:

```bash
$ go run main.go
Creating a new person...
Get object from sync.Pool for the first time: &{}
Put the object back in the pool
Set object property name: Gopher
Get object from pool again (it's updated): &{Gopher}
Creating a new person...
There is no object in the pool now (new one will be created): &{}
```

_Notice how we did [type assertion](https://karanpratapsingh.com/courses/go/interfaces#type-assertion) when we call `Get`._

It can be seen that the `sync.Pool` is strictly a temporary object pool, which is suitable for storing some temporary objects that will be shared among goroutines.

## Map

Map is like the standard `map[any]any` but is safe for concurrent use by multiple goroutines without additional locking or coordination. Loads, stores, and deletes are spread over constant time.

### But why do we need it?

The Map type is _specialized_. Most code should use a plain Go map instead, with separate locking or coordination, for better type safety and to make it easier to maintain other invariants along with the map content.

The Map type is optimized for two common use cases:

- When the entry for a given key is only ever written once but read many times, as in caches that only grow.
- When multiple goroutines read, write, and overwrite entries for disjoint sets of keys. In these two cases, the use of a `sync.Map` may significantly reduce lock contention compared to a Go map paired with a separate `Mutex` or `RWMutex`.

_The zero Map is empty and ready for use. A Map must not be copied after first use._

### Usage

`sync.Map` gives us the following methods:

- `Delete()` deletes the value for a key.
- `Load(key any)` returns the value stored in the map for a key, or nil if no value is present.
- `LoadAndDelete(key any)` deletes the value for a key, returning the previous value if any. The loaded result reports whether the key was present.
- `LoadOrStore(key, value any)` returns the existing value for the key if present. Otherwise, it stores and returns the given value. The loaded result is true if the value was loaded, and false if stored.
- `Store(key, value any)` sets the value for a key.
- `Range(f func(key, value any) bool)` calls `f` sequentially for each key and value present in the map. If `f` returns false, the range stops the iteration.

_Note: Range does not necessarily correspond to any consistent snapshot of the Map's contents._

### Example

Let's look at an example. Here, we will launch a bunch of goroutines that will add and retrieve values from our map concurrently.

```go
package main

import (
	"fmt"
	"sync"
)

func main() {
	var wg sync.WaitGroup
	var m sync.Map

	wg.Add(10)
	for i := 0; i <= 4; i++ {
		go func(k int) {
			v := fmt.Sprintf("value %v", k)

			fmt.Println("Writing:", v)
			m.Store(k, v)
			wg.Done()
		}(i)
	}

	for i := 0; i <= 4; i++ {
		go func(k int) {
			v, _ := m.Load(k)
			fmt.Println("Reading: ", v)
			wg.Done()
		}(i)
	}

	wg.Wait()
}
```

As expected, our store and retrieve operation will be safe for concurrent use.

```bash
$ go run main.go
Reading: <nil>
Writing: value 0
Writing: value 1
Writing: value 2
Writing: value 3
Writing: value 4
Reading: value 0
Reading: value 1
Reading: value 2
Reading: value 3
```

## Atomic

Package atomic provides low-level atomic memory primitives for integers and pointers that are useful for implementing synchronization algorithms.

### Usage

`atomic` package provides [several functions](https://pkg.go.dev/sync/atomic#pkg-functions) that do the following 5 operations for `int`, `uint`, and `uintptr` types:

- Add
- Load
- Store
- Swap
- Compare and Swap

### Example

We won't be able to cover all of the functions here. So, let's take a look at the most commonly used function like `AddInt32` to get an idea.

```go
package main

import (
  "fmt"
	"sync"
	"sync/atomic"
)

func add(w *sync.WaitGroup, num *int32) {
	defer w.Done()
	atomic.AddInt32(num, 1)
}

func main() {
	var n int32 = 0
	var wg sync.WaitGroup

	wg.Add(1000)
	for i := 0; i < 1000; i = i + 1 {
		go add(&wg, &n)
	}

	wg.Wait()

	fmt.Println("Result:", n)
}
```

Here, `atomic.AddInt32` guarantees that the result of `n` will be 1000 as the instruction execution of atomic operations cannot be interrupted.

```bash
$ go run main.go
Result: 1000
```

# Advanced Concurrency Patterns

In this tutorial, we will discuss some advanced concurrency patterns in Go. Often, these patterns are used in combination in the real world.

## Generator

![generator](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/advanced-concurrency-patterns/generator.png)

Then generator Pattern is used to generate a sequence of values which is used to produce some output.

In our example, we have a `generator` function that simply returns a channel from which we can read the values.

This works on the fact that _sends_ and _receives_ block until both the sender and receiver are ready. This property allowed us to wait until the next value is requested.

```go
package main

import "fmt"

func main() {
	ch := generator()

	for i := 0; i < 5; i++ {
		value := <-ch
		fmt.Println("Value:", value)
	}
}

func generator() <-chan int {
	ch := make(chan int)

	go func() {
		for i := 0; ; i++ {
			ch <- i
		}
	}()

	return ch
}
```

If we run this, we'll notice that we can consume values that were produced on demand.

```bash
$ go run main.go
Value: 0
Value: 1
Value: 2
Value: 3
Value: 4
```

_This is a similar behavior as `yield` in JavaScript and Python._

## Fan-in

![fan-in](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/advanced-concurrency-patterns/fan-in.png)

The fan-in pattern combines multiple inputs into one single output channel. Basically, we multiplex our inputs.

In our example, we create the inputs `i1` and `i2` using the `generateWork` function. Then we use our [variadic function](https://karanpratapsingh.com/courses/go/functions#variadic-functions) `fanIn` to combine values from these inputs to a single output channel from which we can consume values.

_Note: order of input will not be guaranteed._

```go
package main

import (
	"fmt"
	"sync"
)

func main() {
	i1 := generateWork([]int{0, 2, 6, 8})
	i2 := generateWork([]int{1, 3, 5, 7})

	out := fanIn(i1, i2)

	for value := range out {
		fmt.Println("Value:", value)
	}
}

func fanIn(inputs ...<-chan int) <-chan int {
	var wg sync.WaitGroup
	out := make(chan int)

	wg.Add(len(inputs))

	for _, in := range inputs {
		go func(ch <-chan int) {
			for {
				value, ok := <-ch

				if !ok {
					wg.Done()
					break
				}

				out <- value
			}
		}(in)
	}

	go func() {
		wg.Wait()
		close(out)
	}()

	return out
}

func generateWork(work []int) <-chan int {
	ch := make(chan int)

	go func() {
		defer close(ch)

		for _, w := range work {
			ch <- w
		}
	}()

	return ch
}
```

```bash
$ go run main.go
Value: 0
Value: 1
Value: 2
Value: 6
Value: 8
Value: 3
Value: 5
Value: 7
```

## Fan-out

![fan-out](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/advanced-concurrency-patterns/fan-out.png)

Fan-out patterns allow us to essentially split our single input channel into multiple output channels. This is a useful pattern to distribute work items into multiple uniform actors.

In our example, we break the input channel into 4 different output channels. For a dynamic number of outputs, we can merge outputs into a shared _"aggregate"_ channel and use `select`.

_Note: fan-out pattern is different from pub/sub._

```go
package main

import "fmt"

func main() {
	work := []int{1, 2, 3, 4, 5, 6, 7, 8}
	in := generateWork(work)

	out1 := fanOut(in)
	out2 := fanOut(in)
	out3 := fanOut(in)
	out4 := fanOut(in)

	for range work {
		select {
		case value := <-out1:
			fmt.Println("Output 1 got:", value)
		case value := <-out2:
			fmt.Println("Output 2 got:", value)
		case value := <-out3:
			fmt.Println("Output 3 got:", value)
		case value := <-out4:
			fmt.Println("Output 4 got:", value)
		}
	}
}

func fanOut(in <-chan int) <-chan int {
	out := make(chan int)

	go func() {
		defer close(out)

		for data := range in {
			out <- data
		}
	}()

	return out
}

func generateWork(work []int) <-chan int {
	ch := make(chan int)

	go func() {
		defer close(ch)

		for _, w := range work {
			ch <- w
		}
	}()

	return ch
}
```

As we can see, our work has been split between multiple goroutines.

```bash
$ go run main.go
Output 1 got: 1
Output 2 got: 3
Output 4 got: 4
Output 1 got: 5
Output 3 got: 2
Output 3 got: 6
Output 3 got: 7
Output 1 got: 8
```

## Pipeline

![pipeline](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/advanced-concurrency-patterns/pipeline.png)

The pipeline pattern is a series of _stages_ connected by channels, where each stage is a group of goroutines running the same function.

In each stage, the goroutines:

- Receive values from _upstream_ via _inbound_ channels.
- Perform some function on that data, usually producing new values.
- Send values _downstream_ via _outbound_ channels.

Each stage has any number of inbound and outbound channels, except the first and last stages, which have only outbound or inbound channels, respectively. The first stage is sometimes called the _source_ or _producer_; the last stage is the _sink_ or _consumer_.

By using a pipeline, we separate the concerns of each stage, which provides numerous benefits such as:

- Modify stages independent of one another.
- Mix and match how stages are combined independently of modifying the stage.

In our example, we have defined three stages, `filter`, `square`, and `half`.

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	in := generateWork([]int{0, 1, 2, 3, 4, 5, 6, 7, 8})

	out := filter(in) // Filter odd numbers
	out = square(out) // Square the input
	out = half(out)   // Half the input

	for value := range out {
		fmt.Println(value)
	}
}

func filter(in <-chan int) <-chan int {
	out := make(chan int)

	go func() {
		defer close(out)

		for i := range in {
			if i%2 == 0 {
				out <- i
			}
		}
	}()

	return out
}

func square(in <-chan int) <-chan int {
	out := make(chan int)

	go func() {
		defer close(out)

		for i := range in {
			value := math.Pow(float64(i), 2)
			out <- int(value)
		}
	}()

	return out
}

func half(in <-chan int) <-chan int {
	out := make(chan int)

	go func() {
		defer close(out)

		for i := range in {
			value := i / 2
			out <- value
		}
	}()

	return out
}

func generateWork(work []int) <-chan int {
	ch := make(chan int)

	go func() {
		defer close(ch)

		for _, w := range work {
			ch <- w
		}
	}()

	return ch
}
```

Seem like our input was processed correctly by the pipeline in a concurrent manner.

```bash
$ go run main.go
0
2
8
18
32
```

## Worker Pool

![worker-pool](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/advanced-concurrency-patterns/worker-pool.png)

The worker pool is a really powerful pattern that lets us distributes the work across multiple workers (goroutines) concurrently.

In our example, we have a `jobs` channel to which we will send our jobs and a `results` channel where our workers will send the results once they've finished doing the work.

After that, we can launch our workers concurrently and simply receive the results from the `results` channel.

_Ideally, `totalWorkers` should be set to `runtime.NumCPU()` which gives us the number of logical CPUs usable by the current process._

```go
package main

import (
	"fmt"
	"sync"
)

const totalJobs = 4
const totalWorkers = 2

func main() {
	jobs := make(chan int, totalJobs)
	results := make(chan int, totalJobs)

	for w := 1; w <= totalWorkers; w++ {
		go worker(w, jobs, results)
	}

	// Send jobs
	for j := 1; j <= totalJobs; j++ {
		jobs <- j
	}

	close(jobs)

	// Receive results
	for a := 1; a <= totalJobs; a++ {
		<-results
	}

	close(results)
}

func worker(id int, jobs <-chan int, results chan<- int) {
	var wg sync.WaitGroup

	for j := range jobs {
		wg.Add(1)

		go func(job int) {
			defer wg.Done()

			fmt.Printf("Worker %d started job %d\n", id, job)

			// Do work and send result
			result := job * 2
			results <- result

			fmt.Printf("Worker %d finished job %d\n", id, job)
		}(j)
	}

	wg.Wait()
}
```

As expected, our jobs were distributed among our workers.

```bash
$ go run main.go
Worker 2 started job 4
Worker 2 started job 1
Worker 1 started job 3
Worker 2 started job 2
Worker 2 finished job 1
Worker 1 finished job 3
Worker 2 finished job 2
Worker 2 finished job 4
```

## Queuing

![queuing](https://raw.githubusercontent.com/karanpratapsingh/portfolio/master/public/static/courses/go/chapter-IV/advanced-concurrency-patterns/queuing.png)

Queuing pattern allows us to process `n` number of items at a time.

In our example, we use a buffered channel to simulate a queue behavior. We simply send an [empty struct](https://karanpratapsingh.com/courses/go/structs#properties) to our `queue` channel and wait for it to be released by the previous process so that we can continue.

This is because _sends_ to a buffered channel block only when the buffer is full and _receives_ block when the buffer is empty.

Here, we have total work of 10 items and we have a limit of 2. This means we can process 2 items at a time.

_Notice how our `queue` channel is of type `struct{}` as an empty struct occupies zero bytes of storage._

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

const limit = 2
const work = 10

func main() {
	var wg sync.WaitGroup

	fmt.Println("Queue limit:", limit)
	queue := make(chan struct{}, limit)

	wg.Add(work)

	for w := 1; w <= work; w++ {
		process(w, queue, &wg)
	}

	wg.Wait()

	close(queue)
	fmt.Println("Work complete")
}

func process(work int, queue chan struct{}, wg *sync.WaitGroup) {
	queue <- struct{}{}

	go func() {
		defer wg.Done()

		time.Sleep(1 * time.Second)
		fmt.Println("Processed:", work)

		<-queue
	}()
}
```

If we run this, we will notice that it briefly pauses when every 2nd item (which is our limit) is processed as our queue waits to be dequeued.

```bash
$ go run main.go
Queue limit: 2
Processed: 1
Processed: 2
Processed: 4
Processed: 3
Processed: 5
Processed: 6
Processed: 8
Processed: 7
Processed: 9
Processed: 10
Work complete
```

## Additional patterns

Some additional patterns that might be useful to know:

- Tee channel
- Bridge channel
- Ring buffer channel
- Bounded parallelism

# Context

In concurrent programs, it's often necessary to preempt operations because of timeouts, cancellations, or failure of another portion of the system.

The `context` package makes it easy to pass request-scoped values, cancellation signals, and deadlines across API boundaries to all the goroutines involved in handling a request.

## Types

Let's discuss some core types of the `context` package.

### Context

The `Context` is an `interface` type that is defined as follows:

```go
type Context interface {
	Deadline() (deadline time.Time, ok bool)
	Done() <-chan struct{}
	Err() error
	Value(key any) any
}
```

The `Context` type has the following methods:

- `Done() <- chan struct{}` returns a channel that is closed when the context is canceled or times out. Done may return `nil` if the context can never be canceled.
- `Deadline() (deadline time.Time, ok bool)` returns the time when the context will be canceled or timed out. Deadline returns `ok` as `false` when no deadline is set.
- `Err() error` returns an error that explains why the Done channel was closed. If Done is not closed yet, it returns `nil`.
- `Value(key any) any` returns the value associated with the key or `nil` if none.

### CancelFunc

A `CancelFunc` tells an operation to abandon its work and it does not wait for the work to stop. If it is called by multiple goroutines simultaneously, after the first call, subsequent calls to a `CancelFunc` does nothing.

```go
type CancelFunc func()
```

## Usage

Let's discuss functions that are exposed by the `context` package:

### Background

Background returns a non-nil, empty `Context`. It is never canceled, has no values, and has no deadline.

_It is typically used by the main function, initialization, and tests, and as the top-level Context for incoming requests._

```go
func Background() Context
```

### TODO

Similar to the `Background` function `TODO` function also returns a non-nil, empty `Context`.

However, it should only be used when we are not sure what context to use or if the function has not been updated to receive a context. This means we plan to add context to the function in the future.

```go
func TODO() Context
```

### WithValue

This function takes in a context and returns a derived context where the value `val` is associated with `key` and flows through the context tree with the context.

This means that once you get a context with value, any context that derives from this gets this value.

_It is not recommended to pass in critical parameters using context values, instead, functions should accept those values in the signature making it explicit._

```go
func WithValue(parent Context, key, val any) Context
```

**Example**

Let's take a simple example to see how we can add a key-value pair to the context.

```go
package main

import (
	"context"
	"fmt"
)

func main() {
	processID := "abc-xyz"

	ctx := context.Background()
	ctx = context.WithValue(ctx, "processID", processID)

	ProcessRequest(ctx)
}

func ProcessRequest(ctx context.Context) {
	value := ctx.Value("processID")
	fmt.Printf("Processing ID: %v", value)
}
```

And if we run this, we'll see the `processID` being passed via our context.

```bash
$ go run main.go
Processing ID: abc-xyz
```

### WithCancel

This function creates a new context from the parent context and derived context and the cancel function. The parent can be a `context.Background` or a context that was passed into the function.

Canceling this context releases resources associated with it, so the code should call cancel as soon as the operations running in this context is completed.

_Passing around the `cancel` function is not recommended as it may lead to unexpected behavior._

```go
func WithCancel(parent Context) (ctx Context, cancel CancelFunc)
```

### WithDeadline

This function returns a derived context from its parent that gets canceled when the deadline exceeds or the cancel function is called.

For example, we can create a context that will automatically get canceled at a certain time in the future and pass that around in child functions. When that context gets canceled because of the deadline running out, all the functions that got the context gets notified to stop work and return.

```go
func WithDeadline(parent Context, d time.Time) (Context, CancelFunc)
```

### WithTimeout

This function is just a wrapper around the `WithDeadline` function with the added timeout.

```go
func WithTimeout(parent Context, timeout time.Duration) (Context, CancelFunc) {
	return WithDeadline(parent, time.Now().Add(timeout))
}
```

## Example

Let's look at an example to solidify our understanding of the context.

In the example below, we have a simple HTTP server that handles a request.

```go
package main

import (
	"fmt"
	"net/http"
	"time"
)

func handleRequest(w http.ResponseWriter, req *http.Request) {
	fmt.Println("Handler started")
	context := req.Context()

	select {
	// Simulating some work by the server, waits 5 seconds and then responds.
	case <-time.After(5 * time.Second):
		fmt.Fprintf(w, "Response from the server")

	// Handling request cancellation
	case <-context.Done():
		err := context.Err()
		fmt.Println("Error:", err)
	}

	fmt.Println("Handler complete")
}

func main() {
	http.HandleFunc("/request", handleRequest)

	fmt.Println("Server is running...")
	http.ListenAndServe(":4000", nil)
}
```

Let's open two terminals. In terminal one we'll run our example.

```bash
$ go run main.go
Server is running...
Handler started
Handler complete
```

In the second terminal, we will simply make a request to our server. And if we wait for 5 seconds, we get a response back.

```bash
$ curl localhost:4000/request
Response from the server
```

Now, let's see what happens if we cancel the request before it completes.

_Note: we can use `ctrl + c` to cancel the request midway._

```bash
$ curl localhost:4000/request
^C
```

And as we can see, we're able to detect the cancellation of the request because of the request context.

```bash
$ go run main.go
Server is running...
Handler started
Error: context canceled
Handler complete
```

I'm sure you can already see how this can be immensely useful.

For example, we can use this to cancel any resource-intensive work if it's no longer needed or has exceeded the deadline or a timeout.

# Next Steps

Congratulations, you've finished the course!

Now that you know the fundamentals of Go, here are some additional things for you to try:

- [Build a REST API with Go - For Beginners](https://www.youtube.com/watch?v=bFYZrEuEDLE)
- [Connecting to PostgreSQL using GORM](https://www.youtube.com/watch?v=Yk5ZjKq4qDQ)
- [Web Scraping with Go](https://www.youtube.com/watch?v=sU_BwzOxl54)
- [Dockerize your Go app](https://www.youtube.com/watch?v=zUc2LihXjlw)
- [DevOps Roadmap](https://www.youtube.com/watch?v=np_seazJL3Q)

I hope this course was a great learning experience. I would love to hear feedback from you.

Wishing you all the best for further learning!

# References

Here are the resources that were referenced while creating this course.

- [The Go Programming Language](https://www.gopl.io)
- [Official Go documentation](https://go.dev/doc)
- [Official Go blog](https://go.dev/blog)
- [A Tour of Go](https://go.dev/tour)
- [Learn Go with Tests](https://quii.gitbook.io/learn-go-with-tests)
