/*
   Default Functions
    FUNCTIONS
    ------------

    fun functionName(){ }
    fun functionPara(parameter: Type){ }
    fun FunctionPara(parameter: Type): returnType{ }
    fun shorthandSingle-expressionFunctions(parameter: Type) = x*x
    fun defaultValuePara(parameter: Type = defaultValue){ }



    Function calling:
    -----------------

    functionName()
    functionPara(something)
*/

fun main(args: Array<String>) {
    var result = findVolume(2, 3)
    print(result)
}

fun findVolume(length: Int, breadth: Int, height: Int = 10): Int {
    return length * breadth * height
}

fun sayHi() { // a Unit function
    println("Hello")
}

// Function with parameter
fun sayHello(name: String) {
    require(name.isNotBlank()) // require is a way to test args.
                               // isNotBlank tests if string is not empty
    println("Hello, $name!")
}

// Function with default arguments
fun sayFriendlyHello(name: String = "Friend") {
    print("Hello, $name!")
}

// Function with mix of regular and default arguments
fun createCat(name: String = "Kitty", age: Int, isSpayed: Boolean = false) {
    print("$name / $age / $isSpayed")
}

createCat(age = 1) // using just the non-default argument
createCat("Fluffy", 2, true) // one way to call a function

// Calling a function with named arguments
createCat(age = 2, isSpayed = true, name = "Fluffy")

// Function with parameters and return value
fun total(x: Int, y: Int): Int {
    return x + y
}

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

// a function as a single expression
fun product(x: Int, y: Int) = x * y

fun main(args: Array<String>) {
    var largeValue = max(4, 6)
    println("The greater number is $largeValue")
}

fun max(a: Int, b: Int): Int =
    if (a > b) {
        println("$a is greater")
        a
    } else {
        println("$b is greater")
        b
    }

// function overloading
// NOTE: Return type alone is not enough to overload.
fun sum(a: Int, b: Int): Int = a + b
fun sum(a: Double, b: Double): Double = a + b

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

// lambda
//         | specify type here |
val adder: (Int, Int) -> Int = {
    x, y -> x + y
}

val adder = {
    // or here
    x: Int, y: Int -> x + y
}

// lambda with single parameter: it keyword
val square: (Int) -> Int = { it * it }
// or
val sqr = { num: Int -> num * num }

val summer: (Int, Int) -> Int = { xi: Int, yi: Int -> xi + yi }
val muller = { xa: Int, ya: Int -> xa * ya }

// passing a lambda to a function
val addWithLambda = doMath(adder, 2, 3)

// lambda with a receiver
val f: String.() -> Int {
    length
}
val num = "hello".f() // 5

// Anonymous function
// explicit syntax. less used.

doMath(
    fun(a: Int, b: Int): Int {
        return a + b
    },
    2,
    3
)

// a function that accepts another function
fun doMath(mathOperation: (Int, Int) -> Int, a: Int, b: Int): Int {
    return mathOperation(a, b)
}

// calling a function that accepts another function
//               +---- this is callable reference. used to reference an
//               |     existing declaration as a value instead of calling it.
//               |     <empty>::callable_ref because current scope
//               |     or else
//               |     <classInstance>::callable_ref
//               |
//               | top-level functions -> ::total, member functions -> String::length
//               + constructors -> ::MyClass,      properties -> ::x
//               v
val add = doMath(::total, 2, 3)
val multiply = doMath(::product, 2, 3)

// we can pass lambda as function
val res = doMath({ x, y -> x + y }, 2, 3)
// we can use trailing lambda
val result = doMath(2, 3) { x, y -> x + y }
// or just store lambda in variable then pass variable
val add = { a: Int, b: Int -> a + b }
doMath(add, 2, 3)

// Higher Order Function
// means functions can be used as values
// i.e. Function which accepts function as parameter or returns a function
// or both.

//                          |< function as input >|
fun operate(a: Int, b: Int, op: (Int, Int) -> Int): Int {
    return op(a, b)
}

fun main() {
    val res = operate(2, 3) {
        x, y -> x + y
    }

    // ^ this is another kotlin feature where {} scope is passed as final
    // argument. this is known as trailing lambda
}

val myLambdaPrinter = { num: Int -> println("<--$num-->") }
printSum(8, 3, myLambdaPrinter)

// Now we want a function we take two number and print those sum by lambdaPrinter
// so we make function here which take two number and also lambda Function as parameter
fun printSum(a: Int, b: Int, lambdaPrinter: (Int) -> Unit) {
    lambdaPrinter(a + b)
}
// printSum is high level function, which take lambda as parameter

// lambda function can be pass as parameter
val myLambdaPrinter = { num: Int -> println("<--$num-->") }

// higher order (higher level) function which takes parameter of function
fun printSum(a: Int, b: Int, lambdaPrinter: (Int) -> Unit) {
    lambdaPrinter(a + b)
}
printSum(4, 7, myLambdaPrinter)

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // vararg
    // Allows a function to accept a variable number of arguments.
    fun printAll(vararg items: String) { // internally vararg becomes array
        for (i in items) { println(i) }
    }
    printAll("a", "b", "c")
    // You can also pass an existing array using the spread operator:
    val arr = arrayOf("x", "y")
    printAll(*arr)

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // Extension Functions

    // add the "fizzbuzz()" function to the Int class
    fun Int.fizzBuzz(): String {
        return when {
            this % 3 == 0 -> "fizz"
            this % 5 == 0 -> "buzz"
            this % 15 == 0 -> "fizzbuzz"
            else -> this.toString()
        }
    }
    println(6.fizzBuzz()) // prints "fizz"
    println(8.fizzBuzz()) // print '8'

    // add the 'absValue' property to the Int class
    val Int.absValue: Int
    get() = abs(this)
    println((-3).absValue) // print '3'

    fun String.exclaim(): String {
        return this + "!"
    }

    val mes = "Hello"
    println(message.exclaim())

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

// Trailing Lambda

// If the last parameter of a function is a function type (a lambda),
// you can place the lambda outside the parentheses.
fun greet(name: String, block: () -> Unit) {
    println("Hello $name")
    block()
}

greet("Sudan") {
    println("Welcome!")
}

// ^ instead of

greet("Sudan", {
    println("Welcome!")
})

// if the lambda is the only param, you can even omit the parentheses
run {
    println("Running")
}

// since kotlin allows optional args, trailing lambda can make use of it
fun greet(name: String, block: () -> Unit = {}) {
    println("Hello $name")
    block()
}

greet("Sudan") // uses default empty lambda
greet("Sudan") {
    println("Welcome")
}

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

// Operator Overloading
// Use operator keyword.

data class Point(val x: Int, val y: Int)
operator fun Point.plus(other: Point) = Point(x + other.x, y + other.y)
val p = Point(1, 2) + Point(3, 4)

// + plus Addition
// - minus Subtraction
// * times Multiplication
// / div Division
// % rem Remainder
data class Vector2D(val x: Double, val y: Double) {
    operator fun plus(other: Vector2D): Vector2D {
        return Vector2D(x + other.x, y + other.y)
    }

    operator fun minus(other: Vector2D): Vector2D {
        return Vector2D(x - other.x, y - other.y)
    }

    operator fun times(scalar: Double): Vector2D {
        return Vector2D(x * scalar, y * scalar)
    }

    operator fun div(scalar: Double): Vector2D {
        return Vector2D(x / scalar, y / scalar)
    }
}

fun main() {
    val v1 = Vector2D(3.0, 4.0)
    val v2 = Vector2D(1.0, 2.0)

    val sum = v1 + v2
    println("v1 + v2 = Vector2D(${sum.x}, ${sum.y})") // Output: v1 + v2 = Vector2D(4.0, 6.0)

    val diff = v1 - v2
    println("v1 - v2 = Vector2D(${diff.x}, ${diff.y})") // Output: v1 - v2 = Vector2D(2.0, 2.0)

    val scaled = v1 * 2.0
    println("v1 * 2.0 = Vector2D(${scaled.x}, ${scaled.y})") // Output: v1 * 2.0 = Vector2D(6.0, 8.0)

    val divided = v1 / 2.0
    println("v1 / 2.0 = Vector2D(${divided.x}, ${divided.y})") // Output: v1 / 2.0 = Vector2D(1.5, 2.0)
}

// +a unaryPlus Unary plus
// -a unaryMinus Unary minus
// !a not Logical NOT
// ++a, a++ inc Increment
// --a, a-- dec Decrement
data class Counter(var value: Int) {
    operator fun unaryMinus(): Counter {
        return Counter(-value)
    }

    operator fun inc(): Counter {
        return Counter(value + 1)
    }

    operator fun dec(): Counter {
        return Counter(value - 1)
    }
}

fun main() {
    var counter = Counter(5)

    val negated = -counter
    println("Negated counter: ${negated.value}") // Output: Negated counter: -5

    val incremented = counter++
    println("After increment: ${counter.value}") // Output: After increment: 6
    println("Incremented value: ${incremented.value}") // Output: Incremented value: 5

    val preIncremented = ++counter
    println("After pre-increment: ${counter.value}") // Output: After pre-increment: 7
    println("Pre-incremented value: ${preIncremented.value}") // Output: Pre-incremented value: 7
}

// a == b equals Equality (No need to mark with operator)
// a > b compareTo Greater than
// a < b compareTo Less than
// a >= b compareTo Greater than or equal to
// a <= b compareTo Less than or equal to
data class Product(val name: String, val price: Double) : Comparable<Product> {
    override operator fun compareTo(other: Product): Int {
        return price.compareTo(other.price)
    }
}

fun main() {
    val product1 = Product("Laptop", 1200.0)
    val product2 = Product("Smartphone", 800.0)

    println("product1 > product2: ${product1 > product2}") // Output: product1 > product2: true
    println("product1 < product2: ${product1 < product2}") // Output: product1 < product2: false
    println("product1 >= product2: ${product1 >= product2}") // Output: product1 >= product2: true
}

// a[i] get Access by index
// a[i] = b set Set by index
// a() invoke Function invocation
class Matrix(private val data: Array<Array<Int>>) {
    operator fun get(row: Int, col: Int): Int {
        return data[row][col]
    }

    operator fun set(row: Int, col: Int, value: Int) {
        data[row][col] = value
    }
}

class Calculator {
    operator fun invoke(a: Int, b: Int): Int {
        return a + b
    }
}

fun main() {
    val matrix = Matrix(arrayOf(
        arrayOf(1, 2, 3),
        arrayOf(4, 5, 6),
        arrayOf(7, 8, 9)
    ))

    println("Element at (1,2): ${matrix[1, 2]}") // Output: Element at (1,2): 6

    matrix[0, 0] = 99
    println("Element at (0,0) after modification: ${matrix[0, 0]}") // Output: Element at (0,0) after modification: 99

    val calculator = Calculator()
    val result = calculator(10, 20)
    println("Calculator result: $result") // Output: Calculator result: 30
}

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

// Inline Functions
// Inserts function body at call site for optimization.

inline fun inlineExample(action: () -> Unit) {
    action()
}

fun main() {
    inlineExample {
        println("this is example")
    }
}

// Infix Functions
// Allows calling a function without dot and parentheses.
// Must be member or extension function with single parameter.

infix fun Int.add(x: Int) = this + x

val result = 5 add 3

// Consider DSL-like APIs.
val map = mapOf(
    "name" to "Sam",
    "age" to 30
)
// The `to` is a infix extension function

infix fun <A, B> A.to(that: B): Pair<A, B>

fun main(args: Array<String>) {
    val x: Int = 6
    val y: Int = 10
    val greaterVal = x findGreaterValue y // x.findGreaterValue(y)
    println(greaterVal)
}

infix fun Int.findGreaterValue(other: Int): Int {
    if (this > other) return this
    else return other
}
// 1. infix functions are extension functions but not vice versa
// 2. infix functions just have one parameter

import java.math.BigInteger

/*
*   Tailrec Function : Recursive Functions
*       -> Prevents Stackoverflow Exception
*
*   Fibonacci Series
*       0  1  1  2  3  5  8  13  21 ......
*/
fun main(args: Array<String>) {
    println(getFibonacciNumber(10000, BigInteger("1"), BigInteger("0")))
}

// prevents stack overflow & performance is optimized

// The recursive call must be in the tail position (the last operation)
// The function cannot be used in a try-catch-finally block
// It cannot be open (non-final) in JVM

tailrec fun getFibonacciNumber(n: Int, a: BigInteger, b: BigInteger): BigInteger {
    if (n == 0)
        return  b
    else
        return getFibonacciNumber(n - 1, a + b, a)
}

// INCORRECT - Not in tail position
tailrec fun incorrect(n: Int): Int {
    return if (n <= 0) 0 else 1 + incorrect(n - 1)  // Addition happens after recursion
}

// CORRECT - In tail position
tailrec fun correct(n: Int, acc: Int = 0): Int {
    return if (n <= 0) acc else correct(n - 1, acc + 1)
}


// This won't be optimized
tailrec fun notOptimized(n: Int): Int {
    return try {
        if (n <= 0) 0 else notOptimized(n - 1)  // Won't be properly tail-recursive
    } catch (e: Exception) {
        -1
    }
}

// Closures
fun main(args: Array<String>) {
    val program = TheProgram()
    var result = 0
    program.addTwoNumbers(2, 7) {x, y -> result = x + y}
    println(result)
}

class TheProgram {
    fun addTwoNumbers(a: Int, b: Int, action: (Int, Int) -> Unit) {
        action(a, b)
    }
}
