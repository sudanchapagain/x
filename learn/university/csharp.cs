using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Dynamic;
using System.Linq;
using System.Net;
using System.Threading.Tasks;
using System.IO;

/*
 * Resources
 * =========
 * - <https://docs.microsoft.com/dotnet/csharp/language-reference/>
 * - <https://dotnet.microsoft.com/learn>
 * - <https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/inside-a-program/coding-conventions>
 * - <http://www.dotnetperls.com/>
 * - <http://manning.com/skeet2>
 * - <http://shop.oreilly.com/product/0636920024064.do>
 * - <http://shop.oreilly.com/product/9780596519254.do>

 * INTRODUCTION
 * ============
 * C# is a modern, object-oriented programming language developed by Microsoft,
 * designed for building a variety of applications that run on the .NET Framework.
 * The .NET Framework is a software development platform that provides essential tools
 * and libraries to create robust, scalable, and secure applications.

 * WHY C#
 * ======
 * C# is fully object-oriented
 * C# enforces strict type safety
 * Common Language Runtime (CLR) manages memory automatically using a Garbage Collector (GC).
 * Originally tied to Windows, .NET has expanded with .NET Core (now .NET 5+) for cross-platform development.

 * EXECUTION MODEL
 * ===============
 * C# code is compiled into an intermediate language (IL), executed by the CLR at runtime.
 * The CLR provides Just-In-Time (JIT) Compilation, Exception Handling, Security Management,
 * Automatic Memory Management (Garbage Collection)

 * .NET Framework vs. Other Frameworks
 * ===================================
 * - .NET Framework (Windows-based, older technology),
 * - .NET Core (Cross-platform, high-performance, now merged into .NET 5+),
 * - Mono/Xamarin (For mobile and embedded systems),
 * - Unity (For game development)

 * .NET STANDARD 2.0
 * =================
 * - .NET Standard defines a common API set for all .NET implementations.
 * - Ensures code portability across different .NET runtimes.
 * - Enables developers to write libraries that work on .NET Framework, .NET Core, Xamarin, etc.

 * Applied Technologies in .NET
 * ============================
 * - ASP.NET (Web development)
 * - Entity Framework (Database management)
 * - WPF & WinForms (Desktop applications)
 * - Blazor (Web UI using C#)
 * - ML.NET (Machine learning)
 * - MAUI (Cross-platform UI framework)

 * DESKTOP
 * ======
 * - MAUI: Multi-platform App UI. Cross-platform (Windows, macOS, iOS, Android). Uses native platform controls via handlers.
 *   React native but in C# and uses XML

 * - Avalonia: Uses a custom rendering engine, declarative XAML-like UI. Cross-platform (Windows, Linux, macOS).
 *   Flutter but C# and uses XML

 * - WPF: DirectX-based UI rendering. Windows Only. uses XAML for UI
 *   Java Swing but microsoft

 * - Windows Forms: GDI+ for rendering, event driven. Windows only. Classic Win32, procedural UI logic
 *   some outdated thing idk
 
 * THE WEB
 * =======
 * - ASP.NET core: Middleware-based, request pipeline, DI built-in. MVC (Model-View-Controller), Web API, Minimal APIs.
 *   Springboot but microsoft

 * - Blazor: Component-based UI (similar to React). Runs either entirely on the browser (WASM), or Uses SignalR to update UI from the server.
 *   Weird react by microsoft

 * - Razor pages: Page-based MVC without controllers. Simplified MVC. Web, ASP.NET Core.
 *   page based PHP in .NET

 * - SignalR: Real-time WebSocket communication with fallback (Long Polling, SSE). Pub-Sub model, server-to-client messaging.
 *   WebSockets++

 * CONSOLE APPLICATION
 * ===================
 * using System;
 * class Program
 * {
 *     static void Main()
 *     {
 *         Console.WriteLine("Hello, World!");
 *     }
 * }

 * Windows Forms GUI Application
 * =============================
 * using System;
 * using System.Windows.Forms;
 * class Program
 * {
 *     static void Main()
 *     {
 *         Application.Run(new Form { Text = "Hello, GUI!" });
 *     }
 * }

 * Identifiers and Keywords
 * ========================
 * Identifiers are names given to variables, methods, classes, etc. Must start with a
 * letter or underscore and cannot be a keyword. C# has reserved words that cannot be
 * used as identifiers unless prefixed with @.

 * LIST OF RESERVED KEYWORDS
 * =========================
 * abstract, as, base, bool, break, byte, case, catch, char, checked, class,
 * const, continue, decimal, default, delegate, do, double, else, enum, event,
 * explicit, extern, false, finally, fixed, float, for, foreach, goto, if,
 * implicit, in, int, interface, internal, is, lock, long, namespace, new, null,
 * object, operator, out, override, params, private, protected, public, readonly,
 * ref, return, sbyte, sealed, short, sizeof, stackalloc, static, string, struct,
 * switch, this, throw, true, try, typeof, uint, ulong, unchecked, unsafe, ushort,
 * using, virtual, void, volatile, while.

 * Contextual keywords
 * ===================
 * A contextual keyword is used to provide a specific meaning in the code, but it
 * isn't a reserved word in C#. Some contextual keywords, such as partial and where,
 * have special meanings in two or more contexts.

 * add, allows, alias, and, ascending, args, async, await, by, descending, dynamic,
 * equals, field, file, from, get, global, group, init, into, join, let,
 * managed (function pointer calling convention), nameof, nint, not, notnull, nuint,
 * on, or, orderby, partial (type), partial (member), record, remove, required, scoped
 * select, set, unmanaged (function pointer calling convention),
 * unmanaged (generic type constraint), value, var, when (filter condition),
 * where (generic type constraint)where (query clause), with, yield.

 * EXAMPLE:
 * ========
 * int myNumber = 10;  // Valid
 * int @class = 5;     // Valid (using @ to escape keyword)
 * int class = 5;      // Invalid (class is a keyword)

 * COMMENTS
 * ========
 * single: //
 * double /* and close with * followed by /
 * triple for xml comments: /// <summary> contents </summary>

 * DATA TYPES
 * ==========
 * Common value types
 * int -> 4 bytes -> int x = 10;
 * double -> 8 bytes -> double pi = 3.14;
 * char -> 2 bytes -> char letter = 'A';
 * bool -> 1 byte -> bool isTrue = true;
 * 
 * Reference Types
 * string, object, dynamic, arrays, and user-defined classes.
 * string message = "Hello, C#";  // Reference type
 * int number = 42;               // Value type

 * EXPRESSIONS AND OPERATORS
 * +, -, *, /, %
 * ==, !=, <, >, <=, >=
 * &&, ||, !
 * =, +=, -=, *=, /=

 * STRINGS AND CHARACTERS
 * ======================
 * Strings are sequences of characters, while char represents a single character.
 * `string greeting = "Hello, World!";`
 * `char letter = 'A';`
 * 
 * String manipulation:
 * string name = "John";
 * string message = $"Hello, {name}!";  // String interpolation
 * Console.WriteLine(message);

 * ARRAYS
 * ======
 * Arrays store multiple values of the same type.
 * Declaring and initializing:
 * int[] numbers = {1, 2, 3, 4, 5};
 * string[] names = new string[3] { "Alice", "Bob", "Charlie" };
 * Accessing:
 * Console.WriteLine(numbers[0]);  // Output: 1

 * VARIABLES AND PARAMETER
 * =======================
 * Variables store data and must be declared before use.
 * int age = 25;        // Integer variable
 * double pi = 3.14;    // Double variable
 * bool isReady = true; // Boolean variable
 *
 * Parameters allow passing data into methods.
 * void Greet(string name) {
 *      Console.WriteLine($"Hello, {name}!");
 * }
 * Greet("Alice");

 * STATEMENTS
 * ==========
 * C# supports various types of statements:
 * 
 * DECLARATION STATEMENT
 *   int number = 10;
 *   const double Pi = 3.14159;
 * 
 * EXPRESSION STATEMENT
 *   Console.WriteLine("Hello, C#");
 *
 * SELECTION STATEMENT (if, else, switch)
 *   if (age >= 18)
 *      Console.WriteLine("Adult");
 *   else
 *      Console.WriteLine("Minor");
 *
 *   switch (expression) { case x: break; case y: break; default: break; }
 *
 * ITERATION STATEMENT
 * 
 * for (int i = 0; i < 5; i++) { Console.WriteLine(i); }
 * int count = 0; while (count < 5) { Console.WriteLine(count); count++; }
 * foreach (string name in names) { Console.WriteLine(name); }
 *
 * JUMP STATEMENT: Continue, break, return
 * for (int i = 1; i <= 5; i++) { if (i == 3) continue; }

 * NAMESPACES
 * ==========
 * Namespaces organize code and prevent name conflicts.
 * 
 * namespace MyNamespace {
 *      class MyClass {
 *          static void Main() {
 *              Console.WriteLine("Inside MyNamespace");
 *          }
 *      }
 * }
 *
 * using keyword can be used to reference namespaces: using System;
 */

namespace Learning
{
    public class Program
    {
        public static void Syntax()
        {
            Console.WriteLine("Hello World");
            Console.WriteLine("Integer: " + 10 + " Double: " + 3.14 + " Boolean: " + true);
            Console.Write("Hello ");
            Console.Write("World");

            // Sbyte - Signed 8-bit integer (-128 <= sbyte <= 127)
            sbyte fooSbyte = 100;

            // Byte - Unsigned 8-bit integer (0 <= byte <= 255)
            byte fooByte = 100;

            // Short - 16-bit integer
            // Signed - (-32,768 <= short <= 32,767)
            // Unsigned - (0 <= ushort <= 65,535)
            short fooShort = 10000;
            ushort fooUshort = 10000;

            // Integer - 32-bit integer
            int fooInt = 1; // (-2,147,483,648 <= int <= 2,147,483,647)
            uint fooUint = 1; // (0 <= uint <= 4,294,967,295)

            // Long - 64-bit integer
            long fooLong = 100000L; // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
            ulong fooUlong = 100000L; // (0 <= ulong <= 18,446,744,073,709,551,615)
            // Numbers default to being int or uint depending on size.
            // L is used to denote that this variable value is of type long or ulong

            // Double - Double-precision 64-bit IEEE 754 Floating Point
            double fooDouble = 123.4; // Precision: 15-16 digits

            // Float - Single-precision 32-bit IEEE 754 Floating Point
            float fooFloat = 234.5f; // Precision: 7 digits
            // f is used to denote that this variable value is of type float

            // Decimal - a 128-bits data type, with more precision than other floating-point types,
            // suited for financial and monetary calculations
            decimal fooDecimal = 150.3m;

            // Boolean - true & false
            bool fooBoolean = true; // or false

            // Char - A single 16-bit Unicode character
            char fooChar = 'A';

            // Strings -- unlike the previous base types which are all value types,
            // a string is a reference type. That is, you can set it to null
            string fooString = "\"escape\" quotes and add \n (new lines) and \t (tabs)";
            Console.WriteLine(fooString);

            // You can access each character of the string with an indexer:
            char charFromString = fooString[1]; // => 'e'
            // Strings are immutable: you can't do fooString[1] = 'X';

            // Compare strings with current culture, ignoring case
            string.Compare(fooString, "x", StringComparison.CurrentCultureIgnoreCase);

            // Formatting, based on sprintf
            string fooFs = string.Format("Check Check, {0} {1}, {0} {1:0.0}", 1, 2);

            // Dates & Formatting
            DateTime fooDate = DateTime.Now;
            Console.WriteLine(fooDate.ToString("hh:mm, dd MMM yyyy"));

            // Verbatim String
            // You can use the @ symbol before a string literal to escape all characters in the string
            string path = "C:\\Users\\User\\Desktop";
            string verbatimPath = @"C:\Users\User\Desktop";
            Console.WriteLine(path == verbatimPath); // => true

            // You can split a string over two lines with the @ symbol. To escape " use ""
            string bazString = @"Here's some stuff
                on a new line! ""Wow!"", the masses cried";

            // Use const or read-only to make a variable immutable
            // const values are calculated at compile time
            const int HoursWorkPerWeek = 9001;

            // Arrays - zero indexed
            // The array size must be decided upon declaration
            // The format for declaring an array is
            // <datatype>[] <var name> = new <datatype>[<array size>];
            int[] intArray = new int[10];

            // Another way to declare & initialize an array
            int[] y = { 9000, 1000, 1337 };

            // Indexing an array - Accessing an element
            Console.WriteLine("intArray @ 0: " + intArray[0]);
            // Arrays are mutable.
            intArray[1] = 1;

            // Lists
            // Lists are used more frequently than arrays as they are more flexible
            // The format for declaring a list is
            // List<datatype> <var name> = new List<datatype>();
            List<int> intList = new List<int>();
            List<string> stringList = new List<string>();
            List<int> z = new List<int> { 9000, 1000, 1337 }; // initialize
            // The <> are for generics - Check out the cool stuff section

            // Lists don't default to a value;
            // A value must be added before accessing the index
            intList.Add(1);
            Console.WriteLine("intList at 0: " + intList[0]);

            // Other data structures to check out:
            // Stack/Queue
            // Dictionary (an implementation of a hash map)
            // HashSet
            // Read-only Collections
            // Tuple (.NET 4+)

            Console.WriteLine("\n->Operators");

            int i1 = 1, i2 = 2; // Shorthand for multiple declarations

            // Arithmetic is straightforward
            Console.WriteLine(i1 + i2 - i1 * 3 / 7); // => 3

            // Modulo
            Console.WriteLine("11%3 = " + (11 % 3)); // => 2

            // Comparison operators
            Console.WriteLine("3 == 2? " + (3 == 2)); // => false
            Console.WriteLine("3 != 2? " + (3 != 2)); // => true
            Console.WriteLine("3 > 2? " + (3 > 2)); // => true
            Console.WriteLine("3 < 2? " + (3 < 2)); // => false
            Console.WriteLine("2 <= 2? " + (2 <= 2)); // => true
            Console.WriteLine("2 >= 2? " + (2 >= 2)); // => true

            // Bitwise operators!
            /*
            ~       Unary bitwise complement
            <<      Signed left shift
            >>      Signed right shift
            &       Bitwise AND
            ^       Bitwise exclusive OR
            |       Bitwise inclusive OR
            */

            // Incrementing
            int i = 0;
            Console.WriteLine("\n->Inc/Decrement");
            Console.WriteLine(i++); //Prints "0", i = 1. Post-Increment
            Console.WriteLine(++i); //Prints "2", i = 2. Pre-Increment
            Console.WriteLine(i--); //Prints "2", i = 1. Post-Decrement
            Console.WriteLine(--i); //Prints "0", i = 0. Pre-Decrement

            Console.WriteLine("\n->Control Structures");

            // If statements are C-like
            int j = 10;
            if (j == 10)
            {
                Console.WriteLine("I get printed");
            }
            else if (j > 10)
            {
                Console.WriteLine("I don't");
            }
            else
            {
                Console.WriteLine("I also don't");
            }

            // Ternary operators
            // A simple if/else can be written as follows
            // <condition> ? <true> : <false>
            int toCompare = 17;
            string isTrue = toCompare == 17 ? "True" : "False";

            // While loop
            int fooWhile = 0;
            while (fooWhile < 100)
            {
                // Iterated 100 times, fooWhile 0->99
                fooWhile++;
            }

            // Do While Loop
            int fooDoWhile = 0;
            do
            {
                // Start iteration 100 times, fooDoWhile 0->99
                if (false)
                    continue; // skip the current iteration

                fooDoWhile++;

                if (fooDoWhile == 50)
                    break; // breaks from the loop completely
            } while (fooDoWhile < 100);

            // for loop structure => for(<start_statement>; <conditional>; <step>)
            for (int fooFor = 0; fooFor < 10; fooFor++)
            {
                // Iterated 10 times, fooFor 0->9
            }

            // For Each Loop
            // foreach loop structure => foreach(<iteratorType> <iteratorName> in <enumerable>)
            // The foreach loop loops over any object implementing IEnumerable or IEnumerable<T>
            // All the collection types (Array, List, Dictionary...) in the .NET framework
            // implement one or both of these interfaces.
            // (The ToCharArray() could be removed, because a string also implements IEnumerable)
            foreach (char character in "Hello World".ToCharArray())
            {
                // Iterated over all the characters in the string
            }

            // Switch Case
            // A switch works with byte, short, char, and int data types.
            // It also works with enumerated types (discussed in Enum Types),
            // the String class, and a few special classes that wrap
            // primitive types: Character, Byte, Short, and Integer.
            int month = 3;
            string monthString;
            switch (month)
            {
                case 1:
                    monthString = "January";
                    break;
                case 2:
                    monthString = "February";
                    break;
                case 3:
                    monthString = "March";
                    break;
                // You can assign more than one case to an action
                // But you can't add an action without a break before another case
                // (if you want to do this, you would have to explicitly add a goto case x)
                case 6:
                case 7:
                case 8:
                    monthString = "Summer time!!";
                    break;
                default:
                    monthString = "Some other month";
                    break;
            }

            // Converting data

            // Convert String To Integer
            // this will throw a FormatException on failure
            int.Parse("123"); // returns an integer version of "123"

            // TryParse will default to the type's default value on failure
            // in this case 0
            int tryInt;
            if (int.TryParse("123", out tryInt)) // Function is boolean
                Console.WriteLine(tryInt); // 123

            // Convert Integer To String
            // The Convert class has a number of methods to facilitate conversions
            tryInt.ToString();

            // Casting
            // Cast decimal 15 to an int
            // and then implicitly cast to long
            long x = (int)15M;
        }

        // CLASSES
        class Person
        {
            public string Name;
            public int Age;

            public void Greet()
            {
                Console.WriteLine($"Hello, my name is {Name} and I am {Age} years old.");
            }
        }

        // Person p = new Person { Name = "Sudan", Age = 21 };
        // p.Greet();

        // CONSTRUCTORS
        class Car
        {
            public string Model;
            public int Year;

            public Car(string model, int year)
            {
                Model = model;
                Year = year;
            }
        }

        // DE-CONSTRUCTORS
        class Cars
        {
            public string Model;
            public int Year;

            public Cars(string model, int year)
            {
                Model = model;
                Year = year;
            }

            public void Deconstruct(out string model, out int year)
            {
                model = Model;
                year = Year;
            }
        }

        // Car cars = new Cars("Toyota", 2022);
        // (string model, int year) = cars;
        // Console.WriteLine($"{model}, {year}");

        // THIS REFERENCE
        class Employee
        {
            private string name;

            public Employee(string name)
            {
                this.name = name; // 'this' differentiates the field from the parameter
            }
        }

        // PROPERTIES
        class Product
        {
            private string name;

            public string Name
            {
                get { return name; }
                set { name = value; }
            }
        }

        // AUTO IMPLEMENTED PROPERTIES
        class Products
        {
            public string Name { get; set; }
        }

        // INDEXERS: access objects like array
        class MyCollection
        {
            private int[] arr = new int[10];

            public int this[int index]
            {
                get { return arr[index]; }
                set { arr[index] = value; }
            }
        }

        // MyCollection col = new MyCollection();
        // col[0] = 42;
        // Console.WriteLine(col[0]);

        // STATIC CONSTRUCTORS AND CLASSES
        class Config
        {
            public static string AppName;

            static Config()
            {
                AppName = "MyApp";
            }
        }

        static class MathUtils
        {
            public static int Square(int x) => x * x;
        }

        // FINALIZERS (Destructors)
        // A finalizer (~ClassName) is used to clean up resources before an object is destroyed.
        class Resource
        {
            ~Resource()
            {
                Console.WriteLine("Finalizer called");
            }
        }

        // DYNAMIC BINDING
        // dynamic bypasses compile-time type checking.
        // dynamic value = 10;
        // value = "Hello";  // Allowed at runtime
        // Console.WriteLine(value);

        // OPERATOR OVERLOADING
        class Point
        {
            public int X, Y;

            public Point(int x, int y)
            {
                X = x;
                Y = y;
            }

            public static Point operator +(Point a, Point b)
            {
                return new Point(a.X + b.X, a.Y + b.Y);
            }
        }

        // Point p1 = new Point(1, 2);
        // Point p2 = new Point(3, 4);
        // Point p3 = p1 + p2;
        // Console.WriteLine($"({p3.X}, {p3.Y})");

        // INHERITANCE
        // Inheritance allows a class to derive from another class.
        class Animals
        {
            public void Speak() => Console.WriteLine("Animal sound");
        }

        class Dogs : Animals
        {
        }

        // ABSTRACT CLASSES AND METHODS
        // Abstract classes cannot be instantiated and can have abstract methods.
        abstract class Shape
        {
            public abstract double GetArea();
        }

        class Circle : Shape
        {
            public double Radius { get; set; }
            public override double GetArea() => Math.PI * Radius * Radius;
        }

        // BASE KEYWORD
        // The base keyword calls the parent class's constructor or method.
        class Animal
        {
            public Animal(string name) => Console.WriteLine($"Animal: {name}");
        }

        class Dog : Animal
        {
            public Dog(string name) : base(name)
            {
            }
        }

        // METHOD OVERLOADING
        // Method overloading allows multiple methods with the same name but different parameters.
        class MathOperations
        {
            public int Add(int a, int b) => a + b;
            public double Add(double a, double b) => a + b;
        }

        // OBJECT TYPE
        // The object type is the base of all types in C#.
        // object obj = "Hello";
        // Console.WriteLine(obj);

        // STRUCTS
        // Structs are value types and used for lightweight objects.
        struct Pointo
        {
            public int X, Y;

            public Pointo(int x, int y)
            {
                X = x;
                Y = y;
            }
        }

        // ACCESS MODIFIERS
        /*
            | Modifier           | Description                                                 |
            | ------------------ | ----------------------------------------------------------- |
            | public	         | Accessible from anywhere                                    |
            | private            | Accessible only within the class                            |
            | protected          | Accessible in the class and derived classes                 |
            | internal           | Accessible within the same assembly                         |
            | protected internal | Accessible within the assembly or derived classes           |
            | private protected  | Accessible in the same assembly but only in derived classes |
        */

        // INTERFACES
        // Interfaces define a contract for classes to implement.
        interface IShape
        {
            double GetArea();
        }

        class Rectangle : IShape
        {
            public double Width, Height;
            public double GetArea() => Width * Height;
        }

        // ENUMS
        // Enums define a set of named constants.
        enum Days
        {
            Sunday,
            Monday,
            Tuesday,
            Wednesday,
            Thursday,
            Friday,
            Saturday
        };
        // Days today = Days.Monday;
        // Console.WriteLine(today);

        // GENERICS
        // It allow defining type-safe classes and methods.
        class Box<T>
        {
            public T Value;
        }

        Box<int> intBox = new Box<int> { Value = 42 };
        Box<string> strBox = new Box<string> { Value = "Hello" };

        // DELEGATE
        // A delegate is a type that represents references to methods with a specific signature.
        // It allows methods to be passed as parameters.
        // delegate void MessageDelegate(string message); // Delegate declaration
        //
        // class Programs
        // {
        //     static void PrintMessage(string msg) => Console.WriteLine(msg);
        //
        //     static void Main()
        //     {
        //         MessageDelegate del = PrintMessage; // Assign method to delegate
        //         del("Hello, Delegates!"); // Invoke the delegate
        //     }
        // }

        // MULTICAST DELEGATES
        // A delegate can reference multiple methods.
        // void Method1(string msg) => Console.WriteLine($"Method1: {msg}");
        // void Method2(string msg) => Console.WriteLine($"Method2: {msg}");
        // MessageDelegate del = Method1;
        // del += Method2;  // Multicast delegate
        // del("Hello!");

        // EVENTS
        // An event is a special type of delegate used for notifications.
        class Publisher
        {
            public delegate void Notify(); // Declare delegate

            public event Notify OnPublish; // Declare event

            public void Publish()
            {
                Console.WriteLine("Publishing event...");
                OnPublish?.Invoke(); // Trigger event if handlers exist
            }
        }

        class Subscriber
        {
            public void HandleEvent() => Console.WriteLine("Event received!");

            static void Main()
            {
                Publisher pub = new Publisher();
                Subscriber sub = new Subscriber();
                pub.OnPublish += sub.HandleEvent; // Subscribe to event
                pub.Publish(); // Fire event
            }
        }

        // LAMBDA EXPRESSION
        // A lambda expression is a concise way to write anonymous methods.
        // SYNTAX
        // (param1, param2) => expression;
        // WITH DELEGATES
        // Func<int, int, int> add = (a, b) => a + b;
        // Console.WriteLine(add(5, 3));  // Output: 8
        // FILTERING A LIST WITH LAMBDA
        // List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };
        // var evenNumbers = numbers.Where(n => n % 2 == 0);
        // foreach (var num in evenNumbers) {
        //     Console.WriteLine(num);
        // }

        // EXCEPTION HANDLING
        // try
        // {
        //     int result = 10 / 0;  // Throws a DivideByZeroException
        // }
        // catch (DivideByZeroException ex)
        // {
        //     Console.WriteLine($"Error: {ex.Message}");
        // }
        // finally
        // {
        //     Console.WriteLine("Execution completed.");
        // }

        // CUSTOM
        // class CustomException : Exception
        // {
        //     public CustomException(string message) : base(message) { }
        // }
        //
        // class Program
        // {
        //     static void Main()
        //     {
        //         try
        //         {
        //             throw new CustomException("This is a custom exception!");
        //         }
        //         catch (CustomException ex)
        //         {
        //             Console.WriteLine($"Caught: {ex.Message}");
        //         }
        //     }
        // }

        // LINQ
        // LINQ provides SQL-like querying capabilities in C#.
        // int[] numbers = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        //
        // var evenNumbers = from num in numbers
        //         where num % 2 == 0
        //         select num;
        //
        // foreach (var num in evenNumbers)
        //         Console.WriteLine(num);

        // LINQ WITH OBJECTS
        // class Student
        // {
        //     public string Name { get; set; }
        //     public int Age { get; set; }
        // }
        //
        // List<Student> students = new List<Student>
        // {
        //     new Student { Name = "Alice", Age = 22 },
        //     new Student { Name = "Bob", Age = 19 }
        // };
        //
        // var adultStudents = students.Where(s => s.Age >= 21);
        //
        // foreach (var student in adultStudents)
        //      Console.WriteLine(student.Name);

        // Working with Databases (ADO.NET & Entity Framework)
        // C# provides ADO.NET and Entity Framework for database interactions.
        // using System;
        // using System.Data.SqlClient;
        //
        // class Program
        // {
        //     static void Main()
        //     {
        //         string connString = "Server=myServer;Database=myDB;User Id=myUser;Password=myPassword;";
        //         using (SqlConnection conn = new SqlConnection(connString))
        //         {
        //             conn.Open();
        //             Console.WriteLine("Connected to Database!");
        //         }
        //     }
        // }

        // Entity Framework (EF) Example
        // EF allows working with databases using ORM (Object-Relational Mapping).
        // using Microsoft.EntityFrameworkCore;
        //
        // public class Student
        // {
        //     public int Id { get; set; }
        //     public string Name { get; set; }
        // }
        //
        // public class SchoolContext : DbContext
        // {
        //     public DbSet<Student> Students { get; set; }
        //
        //     protected override void OnConfiguring(DbContextOptionsBuilder options)
        //         => options.UseSqlServer("Server=myServer;Database=myDB;User Id=myUser;Password=myPassword;");
        // }
        //
        // // Adding a Student
        // using (var context = new SchoolContext())
        // {
        //     context.Students.Add(new Student { Name = "Alice" });
        //     context.SaveChanges();
        // }

        // ASP.NET
        // ASP.NET is a framework for building web applications.
        /*
         * 1. Install .NET SDK
         * 2. Create a new ASP.NET project with `dotnet new webapi -n MyWebApp; cd MyWebApp; dotnet run`
         * 3. create a controller
         *      using Microsoft.AspNetCore.Mvc;
         *      [Route("api/[controller]")]
         *      [ApiController]
         *      public class HelloController : ControllerBase
         *      {
         *          [HttpGet]
         *          public string Get() => "Hello, ASP.NET!";
         *      }
         * 4. Run `dotnet run` and Access API at http://localhost:5000/api/hello
         *
         * ASP.NET Razor Pages (Minimal Web App)
         * =====================================
         * var builder = WebApplication.CreateBuilder(args);
         * var app = builder.Build();
         * app.MapGet("/", () => "Hello, Web!");
         * app.Run();
         */

        // TODO
        // Tuples – Lightweight data structures that hold multiple values.
        // Pattern Matching – Enhancements in modern C# versions (switch expressions, type patterns, etc.).
        // Nullable Reference Types – Introduced in C# 8.0 to improve null safety.
        // Asynchronous Programming (async/await) – Essential for working with modern applications, especially web and cloud-based applications.
        // Reflection – Inspecting metadata at runtime.
        // Attributes – Metadata that can be applied to classes, methods, properties, etc.
        // Memory Management & Performance:
        //
        // Garbage Collection (GC)
        // Span<T> and Memory<T> for performance optimizations.
        // Threading and Parallel Programming:
        // Task Parallel Library (TPL)
        // PLINQ (Parallel LINQ)
        // File Handling and Streams – Reading and writing files.
        // Interop and Unsafe Code – Using unsafe keyword, pointers, and calling unmanaged code.
        // Records (C# 9+) – Immutable types for better data modeling.
        // Dependency Injection (DI) – Common in modern .NET applications.
        // YIELD
    }
}
