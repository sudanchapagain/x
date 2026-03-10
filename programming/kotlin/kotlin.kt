/*****************************************************************************/
//    Official Documentation: <https://kotlinlang.org/docs/home.html>
//    Do look at this too: <https://kotlinlang.org/docs/idioms.html>
/*****************************************************************************/

// NOTE: this file cannot be compiled.

fun main(args: Array<String>) {
    // val is immuteable, var is muteable
    val x: Int = 2
    var y = 4 // type is inferred
    val z = 3
    y = 5
    println(x + y + z)

    println("Enter your name")
    val nameVar = readLine()
    println("Enter your age")
    val userInput: String? = readLine()
    val ageVar: Int = userInput?.toIntOrNull() ?: 0
    println("Your name is $nameVar and your age is $ageVar")

    // Strings are immutable.
    // the length and elements cannot be changed after their creation.

    // A String can be simply declared within double quote (" ") which is
    // escaped string or triple quote(""" """) which is raw string.
    // list of escape characters supported in Kotlin:
    // \t - Inserts tab
    // \b - Inserts backspace
    // \n - Inserts newline
    // \r - Inserts carriage return
    // \' - Inserts single quote character
    // \" - Inserts double quote character
    // \\ - Inserts backslash
    // \$ - Inserts dollar character

    val str1 = "Hello, World" // escaped String
    val str2 = """Welcome""" // raw String

    // Properties of String

    // length: Int -> It returns the length of string sequence.
    // indices: IntRange -> It returns the ranges of valid character indices
    //          from current char sequence.
    // lastIndex: Int -> It returns the index of last character from char sequence.

    var testingStr = "Kotlin"
    println(testingStr.length)
    println(testingStr.lastIndex)
    println(testingStr.indices)

    // strings equality comparisons are done on the basis of
    // structural equality (==) and  // It's work on value
    // referential equality (===).   // It's work on address

    // * In structural equality two objects have separate instances in memory
    //   but contain same value.
    // * Referential equality specifies that two different references point
    //   the same instance in memory.

    // Structural equality (==)
    val strr1 = "Hello, World!"
    val strr2 = "Hello, World!"

    println(strr1 == strr2) // true
    println(strr1 != strr2) // false

    // Referential equality (===)
    val str3 = "Hello, World!"
    val str4 = "Hello, World!"
    val str5 = str3

    println(str3 === str4) // true
    println(str3 !== str4) // false
    println(str3 === str5) // true

    var nullable: String? = "Kotlin"
    println(nullable?.length)

    nullable = null
    println(nullable?.length ?: "string is null")

    if (nullable is String) {
        println("it is string")
    }

    var nuller: Any = "kotlin"
    val caster = nuller as String
    println(caster)

    // STRING FUNCTIONS

    // compareTo(other: String): Int
    // It returns zero if current is equals to specified other object.
    val tester: String = "Alfred"
    println(tester.compareTo("Alfred"))
    println(tester.compareTo("Alfred"))

    // plus(other: Any?): String
    // It returns the concatenate string with the string representation of the
    // given other string
    println(tester.plus(" Henk"))

    // subSequence(startIndex: Int,endIndex: Int): CharSequence
    // It returns the new character sequence from current character sequence,
    // from startIndex to endIndex.
    println(tester.subSequence(2, 4))

    // CharSequence.contains(other: CharSequence):Boolean
    // It returns true if the character sequence contains the other specified
    // character sequence.
    println(tester.contains("ema")) // true
    println(tester.contains("Ema", true)) // true
    println(tester.contains("Ema", false)) // false

    // CharSequence.count(): Int
    // It returns the length of char sequence.
    println(tester.length)

    // String.drop(n: Int): String
    // It returns a string after removing the first n character.
    println(tester.drop(3))

    // String.dropLast(n: Int): String
    // It returns a string after removing the last n character.
    println(tester.dropLast(2))

    // CharSequence.elementAt(index: Int): Char
    // It returns a character at the given index or throws an
    // IndexOutOfBoundsException if the index does not exist in character
    // sequence.
    println(tester.elementAt(2))

    // CharSequence.indexOf(char: Char ): Int
    // It returns the index of first occurrence of the given character,
    // starting from the given index value otherwise return -1.
    println(tester.indexOf("man")) //  2 -> H e man t
    println(tester.indexOf("nam")) // -1
    println(tester.indexOf("MaN", ignoreCase = true)) // 2

    // CharSequence.trimMargin() : String
    // Leading whitespace can be removed with trimMargin() function. By
    // default, trimMargin() function uses | as margin prefix.
    val text =
        """Kotlin is official language
    ^            announce by Google for
    ^        android application development
    """
    println(text)
    println(text.trimMargin("^"))

    // STRING TEMPLATE
    // ---------------
    // String templates starts with a dollar sign $ which consists either a
    // variable name or an arbitrary expression in curly braces.
    val testVal = "Olly"
    // String template as variable name
    println("Hello! my name is $testVal")
    // String template as arbitrary expression in curly braces
    println("Hello! my full name is ${testVal.plus(" and Oggy")}")

    // TYPES:
    // ------
    // Byte, Short, String, Int, Long, Float, String, Char, Bool exit

    // INITIALIZATION/DECLARATION
    // --------------------------
    // const is calculated at compile time
    // anything that cannot be compiled at compile time cannot be const
    // convention for const val is UPPERCASE
    // const val COMPILE: Int = 555

    // OPERATORS
    // ---------
    // +, -, /, *, %, >, <, <=, >=, ==, !=, +=, -=, *=, /=, %=, &&, ||, !.
    // above mentioned operators exist.
    // `and`, `or` keywords also exists but they do not short circuit

    // TYPE CONVERSION
    // Every conversion is explicit where smaller data type is converted into larger data type and
    // vice-versa with helper function.
    // var value1 = 10.5
    // var value2: Int = value1.toInt()
    // helper functions used for numeric conversion include
    // toByte(), toShort(), toInt(), toLong(), toFloat(), toDouble(), toChar()

    val x1 = 5
    val y1 = 40

    if (x1 > y1) {
        print("x greater")
    } else {
        println("y greater")
    }

    val ans = if (x1 > y1) x1 else y1
    println(ans)

    // when keyword
    val media = 6
    when {
        media >= 12 -> {
            println("yes")
        }
        media >= 7 && media < 12 -> {
            println("may")
        }
        else -> {
            println("no")
        }
    }

    // ranges & When keyword
    var whenner = 4
    when (whenner) {
        1 -> println("value is : 1")
        2 -> println("value is : 2")
        else -> println("value is not in range of 1..5")
        // else is same as default
    }

    // comma for same logic but multiple choices
    var z4 = 6
    var output =
        when (z4) {
            0, 2, 4, 6, 8, 10 -> "z is even number in range of 0 to 10"
            1, 3, 5, 7, 9 -> "z is odd number in range of 0 to 10"
            else -> "number is not in range of 0 to 10"
        }

    // ranges work too
    // '..' is actually T.rangeTo(that: T)
    when (media) {
        in 12..20 -> {
            // if media between 12-20
            println("yes")
        }
        in 7..12 -> {
            println("may")
        }
        else -> {
            println("no")
        }
    }

    val res = when (media) {
        in 12..20 -> {
            "yes"
        }
        in 7..12 -> {
            "may"
        }
        else -> {
            "no"
        }
    }
    println(res)

    /*
    fun serveTeaTo(customer: Customer) {
        val teaSack = takeRandomTeaSack()
        when (teaSack) {
            is OolongSack -> error("We don't serve Chinese tea like $teaSack!")
            in trialTeaSacks, teaSackBoughtLastNight ->
            error("Are you insane?! We cannot serve uncertified tea!")
        }
        teaPackage.brew().serveTo(customer)
    }
    */

    // for loop
    for (i in 1..10) {
        // println(i)
    }

    var kehi = 0
    for (i in 1..10) {
        kehi += i
        println(i)
        println(kehi)
    }

    // while loop
    var iq = 1
    while (iq <= 10) {
        println("foo")
        iq += 1
    }

    // do while
    var y2 = 11
    do {
        println("bar")
        y2 += 1
    } while (y2 <= 10)

    // ranges and loops
    // Loop from 100 down to 1
    for (i in 100 downTo 1) {
        println(i)
    }

    // Loop from 1 to 100, skipping every other number
    // which means each odd number from 1 to 100
    // downTo and step are extension functions, not keywords.
    for (i in 1..100 step 2) {
        println(i)
    }

    // Loop from 100 down to 1, skipping every other number
    // which means each even number from 100 to 2
    for (i in 100 downTo 1 step 2) {
        println(i)
    }

    // Loop from 1 up to (but not including) 10
    // means each number from 1 to 9
    for (i in 1 until 10) {
        println(i)
    }

    // Jump statements
    // 1. break 2. continue 3. return
    for (i in 1..10) {
        if (i == 6) break
        print("$i ")
    }

    for (i in 1..10) {
        if (i == 6) continue
        print("$i ")
    }

    for (i in 1..10) {
        if (i == 6) return
        print("$i ")
    }

    // ARRAY
    // Kotlin Array can be created using arrayOf(),
    // intArrayOf(), charArrayOf(), booleanArrayOf(),
    // longArrayOf(), shortArrayOf(), byteArrayOf() functions.

    var myArray1 = arrayOf(1, 10, 4, 6, 15)
    var myArray2 = arrayOf<Int>(1, 10, 4, 6, 15) // generic array being typed as Int (boxed)
    var myArray3 = intArrayOf(1, 10, 4, 6, 15) // array of primitive ints (unboxed)
    var myArray4 = arrayOf<String>("Henk", "Olly", "Doraemon", "Hattori", "Ferb")
    var myArray5 = arrayOf<Any>(1, 10, 4, "Henk", "Alfred")

    // access elements of array
    println(myArray1[1])
    println(myArray1.get(1))

    // modify elements of array
    myArray1[1] = 35
    println(myArray1[1])
    myArray2.set(1, 95)
    println(myArray1.get(1))

    // traverse elements of array
    for (e in myArray3) {
        print("$e ")
    }

    // traverse by using index
    for (i in 0..(myArray4.size - 1)) {
        print("${myArray1[i]} ")
    }

    val langs: Array<String> = arrayOf("Kotlin", "Java", "C")
    val langs1 = arrayOf("Kotlin", "Java", "C")

    for (lang in langs) {
        println(lang)
    }

    val batta = Array(3) { "kura" }
    batta[1] = "fohor"
    batta[2] = "khali"

    for (saman in batta) {
        println(saman)
    }

    val numbers: IntArray = intArrayOf(18, 25, 22)
    for (num in numbers) {
        println(num)
    }

    /* FUNCTIONS
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



    HIGHER ORDER FUNCTION:
    ----------------------

    Function which accepts function as parameter or returns a function or both.

    // Lambda is a function which has no name. (anonymous function)
    // Lambda is defined with a curly braces {} which takes variable as a parameter (if any) and body of function.
    // The body of function is written after variable (if any) followed by -> operator.

    // Syntax of Lambda
    // { variable -> body_of_function }

    // lambda function for Print with pattern design
    val myLambdaPrinter = { num: Int -> println("<--$num-->") }

    printSum(8, 3, myLambdaPrinter)

    // Now we want a function we take two number and print those sum by lambdaPrinter
    // so we make function here which take two number and also lambda Function as parameter
    fun printSum(a: Int, b: Int, lambdaPrinter: (Int) -> Unit) {
        lambdaPrinter(a + b)
    }
    // printSum is high level function,which take lambda as parameter

    // lambda function can be pass as parameter
    val myLambdaPrinter = { num: Int -> println("<--$num-->") }

    // higher order (higher level) function which takes parameter of function
    fun printSum(a: Int, b: Int, lambdaPrinter: (Int) -> Unit) {
        lambdaPrinter(a + b)
    }
    printSum(4, 7, myLambdaPrinter)
    */



    // NULL SAFETY
    // -----------
    // declares name as nullable string
    var name: String? = "someone"
    name = null // assigned null

    val notNullText: String = "Definitely not null"
    val nullableText1: String? = "Might be null"
    val nullableText2: String? = null

    // length of name if not null. ? prevents null pointer exception
    println(name?.length)

    // variable can hold nulls
    var countries: List<String?> =
        listOf("Sunland", "Waterland", null, "Fireland", null, "Airland", null, "Spaceland")

    // ? is safe call operator, prints only non-nulls
    // someThing-.otherThing does not throw an NPE if someThing is null.
    // Safe calls are useful in chains. For example, an employee may be assigned to a department (or
    // not). That department may in turn have another employee as a department head, who may or may
    // not have a name, which we want to print:
    fun printDepartmentHead(employee: Employee) {
        println(employee.department?.head?.name)
    }

    // To print only for non-null values, you can use the safe call operator together with let:
    // employee.department?.head?.name?.let { println(it) }
    for (country in countries) {
        country?.let { println(it) }
    }

    fun funny(text: String?) {
        if (text != null) {
            println(text)
        } else {
            println("Nothing to print :(")
        }
    }

    fun funnier(text: String?) {
        val toPrint = text ?: "Nothing to print :("
        println(toPrint)
    }

    // var stre1: String? = "Hello"
    // println(stre1.length) // compile error

    // if (str1 != null) { // -> smart cast
    //     println(str1.length) // -> It works now!
    // }

    // While using is or !is for checking the variable,
    // the compiler tracks this information and internally cast the variable to target type.
    // This is done inside the scope if is or !is returns true.

    var stra2: String? = "Oggy"

    if (stra2 is String) {
        // No Explicit Casting needed.
        println(stra2.length)
    }

    if (stra2 !is String) {
        println("str2 is not string")
    } else {
        // No Explicit Casting needed.
        println(stra2.length)
    }



    // UNSAFE AND SAFE CAST OPERATOR:
    // ------------------------------
    // Sometime it is not possible to cast variable, and it throws an exception, this is called as
    // unsafe cast.

    // Examples

    // A nullable string (String?) cannot be cast to non-nullable string (String), this throw an
    // exception.
    val str: String? = null
    val str1: String = str as String
    // Error : kotlin.TypeCastException: null cannot be cast to non-null type kotlin.String

    // While try to cast integer value of Any type into string type lead to generate a
    // ClassCastException.
    val num: Int = 123
    val str2: String = num as String
    // Throws java.lang.ClassCastException: java.lang.Integer cannot be cast to java.lang.String

    // Kotlin provides a safe cast operator as? for safely cast to a type.
    // It returns a null if casting is not possible rather than throwing an ClassCastException
    // exception.

    // Examples
    val num1: Int = 123
    val strs3 = num1 as? String
    println(strs3) // null



    // Elvis operator ?:
    // -----------------
    // If the expression to the left of ?: is not null, the Elvis operator
    // returns it; otherwise, it returns the expression to the right.

    // the expression on the right-hand side is evaluated only if
    // the left-hand side is null.
    fun loadInfoById(id: String): String? {
        val item = findItem(id) ?: return null
        return item.loadInfo() ?: throw Exception("...")
    }



    // UNSAFE CALLS
    // ------------
    // The not-null assertion operator (!!) converts any value to a non-null type and throws an NPE
    // exception if the value is null.
    /*
    fun printDepartmentHead(employee: Employee) {
        println(employee.department!!.head!!.name!!)
    }
    */
    // unsafe calls are to be avoided.`

    // TODO
    // Always throws a NotImplementedError at run-time if called, stating that operation is not
    // implemented.
    // Throws an error at run-time if calls this function, but compiles
    // fun findItemOrNull(id: String): Item? = TODO("Find item $id")
    // Does not compile at all
    // fun findItemOrNull(id: String): Item? = {}

    // String templates and the string builder
    val ii = 10
    val s = "Kotlin"
    println("ii = $ii")
    println("Length of $s is ${s.length}")
    val sb = StringBuilder()
    sb.append("Hello")
    sb.append(", world!")
    println(sb.toString())

    // lambda expression
    val summer: (Int, Int) -> Int = { xi: Int, yi: Int -> xi + yi }
    val muller = { xa: Int, ya: Int -> xa * ya }

    // According to Kotlin convention, if the last parameter of a function is a function, then a
    // lambda expression passed as the corresponding argument can be placed outside the parentheses:
    // val badProduct = items.fold(1, { acc, e -> acc * e })
    // val goodProduct = items.fold(1) { acc, e -> acc * e }

    // If the lambda is the only argument, the parentheses can be omitted entirely (the
    // documentation calls this feature "trailing lambda as a parameter"):
    // run({ println("Not Cool") })
    // run { println("Very Cool") }

    // EXCEPTION HANDLING
    // There are four different keywords used in exception handling. These are:
    //
    // *try   ->  try block contains set of statements which might generate an exception. It must be
    // followed by either catch or finally or both.
    // *catch   ->  catch block is used to catch the exception thrown from try block.

    // Syntax try catch
    //     try {
    //           code that may throw exception
    //       } catch(e: SomeException) {
    //           code that handles exception  // execute when error occur
    //       } finally {
    //           optional finally block       // execute always
    //       }
    try {
        println("starting of try block")
        // val data = 20 / 0 // may throw exception
        println("end of try block")
    } catch (e: Exception) {
        println("this is catch block with error : ".plus(e))
    } finally {
        println("this is finally block")
    }

    // try block as an Expression
    var x5 =
        try {
            // 20 / 0
        } catch (e: Exception) {
            "Infinite"
        }
    println(x5)
    // multiple catch block
    // ** Note : At a time only one exception is occured and at a time only one catch block is
    // executed.

    try {
        val a = IntArray(5)
        // a[5] = 10 / 0
    } catch (e: ArithmeticException) {
        println("arithmetic exception catch")
    } catch (e: ArrayIndexOutOfBoundsException) {
        println("array index outofbounds exception")
    } catch (e: Exception) {
        println("parent exception class")
    }
    println("code after try catch...")

    // *finally   ->  finally block always execute whether exception is handled or not. So it is
    // used to execute important code statement.
    // *throw   ->  throw keyword is used to throw an exception explicitly.
    //  throw keyword is used to throw an explicit exception.
    //  It is used to throw a custom exception.

    //   Syntax of throw keyword
    //   throw SomeException()

    validate(21)
    validate(14)
}

class Employee {
    val department: Any
}

fun findItem(id: String): Nothing? {
    TODO("")
}

// class with constructor with numerics and tag properties
class Learning(private var numerics: Float, val tag: String) {
    // numeric is val
    fun some(numeric: Float) {
        var res = numeric * 2
        res *= res
    }
}

fun validate(age: Int) {
    if (age < 18) throw ArithmeticException("under 18 year person not allowed!")
    else println("$age is eligible for drive")
}

class Collection() {
    fun CollectionFunc() {
        // Kotlin collections are broadly categories into two different forms. These are:

        // * Immutable Collection (or Collection) -> Immutable collection also called Collection
        // supports read only functionalities
        // * Mutable Collection -> Mutable collections supports both read and write functionalities

        // list are ordered
        // sets are collections of unique elements
        // maps are key-value pairs

        // Immutable
        // ----------------------------------------------

        // List ->  listOf() , listOf<T>()
        var listCollec = listOf("Abc", "Def", "Ghi", "Jkl")
        for (i in listCollec) {
            println(i)
        }
        // functions include contains, containsAll, get(index), indexOf(element),
        // lastIndexOf(element): last occurance returned, isEmpty, subList(start, end), etc.file

        // Map -> mapOf() or mapOf<k,v>().
        // Key-Value pair. The key and value may be of different pairs such as <Int, Int>,<Int,
        // String>, <Char, String>, etc.
        var myMap = mapOf<Int, String>(1 to "Oggy", 2 to "Alfred", 3 to "perman")
        for (e in myMap) {
            println("key: ${e.key} & value: ${e.value}")
        }
        for (e in myMap.keys) {
            println("key is : $e and value is : ${myMap[e]}")
        }
        // Functions include getValue(), contains, containsKey, containsValue, getOrDefault, minus:
        // prints all except parameter, etc
        var iterator = myMap.iterator()
        while (iterator.hasNext()) {
            val pair = iterator.next()
            println("key : ${pair.key} and value : ${pair.value}")
        }

        // Set -> setOf()

        // Mutable
        // ----------------------------------------------

        // List	->  ArrayList<T>() , arrayListOf() , mutableListOf()
        // Similar to listOf, with add(), clear, remove, removeAll, removeAt, set, subList.
        // Map ->   HashMap , hashMapOf() , mutableMapOf()
        // Muteable map with put(), get(), containsKey(), containsValue(), clear(), remove(). Can be
        // constucted with capacity and load factor.
        // Set ->   hashSetOf() , mutableSetOf()
    }
}

class OOP() {
    fun OOPConcepts() {
        // ACCESS MODIFIERS (Encapsulation)
        // publiс – Accessible to anyone
        // private – Accessible only inside the class
        // protected – Accessible inside the class and its inheritors
        // internal – Accessible in the module

        // CLASS:
        // ------
        // class with properties containing default values
        class Student {
            var name = "Lucia"
            var semester = "Fall"
            var gpa = 3.95
        }

        // shorthand syntax without class body
        class Student

        // instance
        class Student {
            var name = "Lucia"
            var semester = "Fall"
            var gpa = 3.95
        }

        fun functo() {
            var student = Student()
            // Instance
            println(student.name)
            // Prints: Lucia
            println(student.semester)
            // Prints: Fall
            println(student.gpa)
            // Prints: 3.95
        }



        // CONSTRUCTOR
        // -----------
        class Student(val name: String, val gps: Double)

        fun main(){
            var student = Student("So", 3.1)
            println(student.name) // So
            println(student.gpa) // 3.1
        }



        // INIT BLOCK
        // init block executes when the class is instantiated.

        class Student(val name: String, val gpa: Double) {
            init {
                println("Student $name has a GPA of $gpa")
            }
        }

        fun main() {
            var student = Student("So", 3.1) // Student So has a GPA of 3.1
        }



        // OPEN CLASS
        // An open class is a class that is allowed to be subclassed.
        // By default, classes in Kotlin are final, meaning they cannot be inherited from.
        // If you want to create a class that other classes can inherit from,
        // you need to mark it as open.

        open class Animal {
            open fun sound() {
                println("Animal makes a sound")
            }
        }

        class Dog : Animal() {
            override fun sound() {
                println("Dog barks")
            }
        }

        // Explanation:
        // Point class' primary constructor takes two parameters x and y.
        // Two secondary constructors exist, depending upon the type of object
        // passed Point or Circle, the respective constructor is selected.
        // This is called constructor overloading.

        // The first secondary constructor delegates to the primary constructor.
        // // The second secondary constructor delegates to the primary constructor
        // indirectly through the first secondary constructor, assuming the Circle
        // class has a property centre of type Point.

        class Circle(val centre: Point)

        // Constructor delegation ensures that the correct constructor
        // is called based on the type of the argument provided.

        open class Point(val x: Int, val y: Int) {
            constructor(other: Point) : this(other.x, other.y) { /**/ }
            constructor(circle: Circle) : this(circle.centre) { /**/ }
        }

        // Abstraction vs Encapsulation
        // Abstraction is about what others see and how they interact with an object.
        // Encapsulation is about how an object operates internally and how it responds to messages.

        // ABSTRACTION (INTERFACE & ABSTRACT CLASS)
        // Interfaces cannot have a state. They can only have abstract properties and functions.
        interface RegularCat {
            fun pet()
            fun feed(food: Food)
        }

        interface SickCat {
            fun checkStomach()
            fun giveMedicine(pill: Pill)
        }

        // Abstract classes cannot have an instance, but can have a state.
        // They can have abstract and non-abstract properties and functions
        abstract class RegularCat {
            abstract val name: String
            abstract fun pet()
            abstract fun feed(food: Food)
        }

        abstract class SickCat {
            abstract val location: String
            abstract fun checkStomach()
            fun giveMedicine(pill: Pill) {}
        }

        // INHERITANCE
        // Inheritance is the process of creating a new class from an existing class.
        // The new class is called a subclass, and the existing class is called a superclass.
        // you can inherit only from one class, and from as many interfaces as you like.

        class SickDomesticCat : RegularCat(), CatAtHospital {
            override var isHungry: Boolean = false
                get() = field
                set(value) { /**/ }
            override fun pet() { /**/ }
            override fun feed(food: Food) { /**/ }
            override fun checkStomach() { /**/ }
            override fun giveMedicine(pill: Pill)
            {//.}
            }

            // WHY DO YOU PREVENT A CAT FROM POOPING?
            abstract class Cat {

                // The cat goes through various daily activities.
                fun anotherDay() {
                    // Finds food and digests it.
                    val food = findFood()
                    digest(food)

                    // Finds a place to poop and poops.
                    val whereToPoop = findWhereToPoop()
                    poop(whereToPoop)
                }

                // The cat digests the food.
                private fun digest(food: Food) {
                    println("Digesting food: $food")
                    // After digestion, the cat poops.
                    val whereToPoop = findWhereToPoop()
                    poop(whereToPoop)
                }

                // The cat poops at the given place.
                private fun poop(where: Place): Poop {
                    println("Pooping at: $where")
                    // Create and return a Poop object (implementation is abstracted).
                    return Poop()
                }

                // Abstract methods to be implemented by subclasses.
                abstract fun feed(food: Food)
                abstract fun findWhereToPoop(): Place
                abstract fun findFood(): Food
            }

            // Class representing a Domestic Cat, extending the abstract Cat class.
            class DomesticCat(private val tray: Tray, private val bowl: Bowl) : Cat() {

                // The cat is fed with food, placing it in the bowl.
                override fun feed(food: Food) {
                    println("Feeding the cat with: $food")
                    bowl.addFood(food)
                }

                // The cat finds a place to poop, which is the tray.
                override fun findWhereToPoop(): Place {
                    return tray
                }

                // The cat finds food, either from the bowl or somewhere else.
                override fun findFood(): Food {
                    return bowl.getFood()
                        ?: run {
                            println("Finding food somewhere else")
                            Food() // Implementation of finding food elsewhere.
                        }
                }
            }

            // Dummy classes for illustration purposes.
            class Food {
                override fun toString(): String {
                    return "Food"
                }
            }

            class Place {
                override fun toString(): String {
                    return "Place"
                }
            }

            class Poop

            class Tray : Place() {
                override fun toString(): String {
                    return "Tray"
                }
            }

            class Bowl {
                fun addFood(food: Food) {
                    println("Food added to the bowl: $food")
                }

                fun getFood(): Food? {
                    // Implementation to get food from the bowl (could be null).
                    return null
                }
            }

            fun catPoop() {
                val tray = Tray()
                val bowl = Bowl()
                val domesticCat = DomesticCat(tray, bowl)

                domesticCat.anotherDay() // Simulate a day in the life of the cat.
            }

            // GETTER, SETTER, PROPERTY, FIELD
            /*___________________________________________________________
            // Properties are declared using var or val keywords.
            // Getter and setter can be defined for a property.
            // Properties can optionally have an initializer, getter, and setter.
            // Use the field keyword to access the values inside the getter or setter,
            // otherwise you might encounter infinite recursion.
            // Properties may have no (backing) filed at all.*/

            class PositiveAttitude(startingAttitude: Int) {
                var attitude = max(0, startingAttitude)
                    set(value) {
                        if (value != 0) {
                            field = value
                        } else {
                            println("Only positive attitude!")
                            field = 0
                        }
                    }

                var hiddenAttitude: Int = startingAttitude
                    private set
                    get() {
                        if (isSecretelyNegative) {
                            println("Don't ask this!")
                            field += 10
                        }
                        return field
                    }

                val isSecretelyNegative: Boolean
                    get() = hiddenAttitude < 0
            }


            // example code
            class LinkedList {
                private var head: Node? = null

                fun add(element: String) {
                    val newNode = Node(element)

                    val it = tail(head)
                    if (it == null) {
                        head = newNode
                    } else {
                        it.next = newNode
                    }
                }

                private fun tail(head: Node?): Node? {
                    var it: Node?

                    it = head
                    while (it?.next != null) {
                        it = it.next
                    }

                    return it
                }

                fun remove(element: String): Boolean {
                    var result = false
                    var previousIt: Node? = null
                    var it: Node? = head
                    while (!result && it != null) {
                        if (0 == element.compareTo(it.data)) {
                            result = true
                            unlink(previousIt, it)
                            break
                        }
                        previousIt = it
                        it = it.next
                    }

                    return result
                }

                private fun unlink(previousIt: Node?, currentIt: Node) {
                    if (currentIt == head) {
                        head = currentIt.next
                    } else {
                        previousIt?.next = currentIt.next
                    }
                }

                fun size(): Int {
                    var size = 0

                    var it = head
                    while (it != null) {
                        ++size
                        it = it.next
                    }

                    return size
                }

                fun get(idx: Int): String {
                    var index = idx
                    var it = head
                    while (index > 0 && it != null) {
                        it = it.next
                        index--
                    }

                    if (it == null) {
                        throw IndexOutOfBoundsException("Index is out of range")
                    }

                    return it.data
                }

                private data class Node(val data: String) {
                    var next: Node? = null
                }
            }

            // extension function
            fun String.exclaim(): String {
                return this + "!"
            }
            fun operate(a: Int, b: Int, op: (Int, Int) -> Int): Int {
                return op(a, b)
            }
            fun opUser() {
                val res = operate(2, 3)
                println(result)
            }
        }
    }

/*
*   Default Functions
*/
fun main(args: Array<String>) {

    var result = findVolume(2, 3)
    print(result)
}

fun findVolume(length: Int, breadth: Int, height: Int = 10): Int {
    return length * breadth * height
}


/*
 * FUNCTIONS as Expressions
 */
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

/*
  INFIX FUNCTIONS
*/
fun main(args: Array<String>) {
    val x: Int = 6
    val y: Int = 10
    val greaterVal = x findGreaterValue y // x.findGreaterValue(y)
    println(greaterVal)
}

infix fun Int.findGreaterValue(other: Int): Int { // INFIX and Extension Func
    if (this > other) return this
    else return other
}
/*
* 1. All INFIX Functions are Extension functions But all Extension functions are not INFIX
* 2. INFIX Functions just have ONE PARAMETER
*/

import java.math.BigInteger

/*
*   Tailrec Function : Recursive Functions
*       -> Prevents Stackoverflow Exception
*
*   Fibonacci Series
*       0  1  1  2  3  5  8  13  21 ......
* */
fun main(args: Array<String>) {

    println(getFibonacciNumber(10000, BigInteger("1"), BigInteger("0")))
}

tailrec fun getFibonacciNumber(n: Int, a: BigInteger, b: BigInteger): BigInteger {

    if (n == 0)
        return  b
    else
        return getFibonacciNumber(n - 1, a + b, a)
}


/*
*   Class, Primary Constructor, Secondary Constructor and Init Block
* */
fun main(args: Array<String>) {

    var student = Student("Steve", 10)

    println(student.id)
}

class Student(var name: String) {

    var id: Int = -1

    init {
        println("Student has got a name as $name and id is $id")
    }

    constructor(n: String, id: Int): this(n) {
        // The body of the secondary constructor is called after init block
        this.id = id
    }
}

enum class Color(val colorShade: String) {
    RED("light red"),
    GREEN("light green"),
}

sealed class Shape {
    // subclass can be a data class
    data class Circle(var radius: Float): Shape()
    // subclass can be a regular class
    class Square(var side: Int): Shape()
    // subclass can be an object (singleton)
    object NotAShape : Shape()
    // sealed class Line : Shape()     // subclass can be another sealed class
    // sealed interface Draw           // subclass can be an interface
}

// You can define any type of subclass outside the sealed class too
class Rectangle(var length: Int, var breadth: Int): Shape()


fun main() {

    var circle = Shape.Circle(3.0f)
    var square = Shape.Square(8)
    var rectangle = Rectangle(20,10)    // Slightly different than above two 

    val noShape = Shape.NotAShape 

    checkShape(noShape)
}

fun checkShape(shape: Shape) {

    when (shape) {
        is Shape.Circle -> println("Circle area is ${3.14 * shape.radius * shape.radius}")
        is Shape.Square -> println("Square area is ${shape.side * shape.side}")
        is Rectangle -> println("Rectagle area is ${shape.length * shape.breadth}")
        Shape.NotAShape -> println("No shape found")  // 'is' is not required for object (singleton)
        //  else -> "else case is not required as all case is covered above"
    }
}


/*
*   1. Companion Object
* */
fun main(args: Array<String>) {

    MyClass.count           // You can print it and check result

    MyClass.typeOfCustomers()
}

class MyClass {

    companion object {

        var count: Int = -1             // Behaves like STATIC variable

        @JvmStatic
        fun typeOfCustomers(): String { // Behaves like STATIC method
            return "Indian"
        }
    }
}



/*
    EXAMPLE ONE

*   1. Lambda Expression
*   2. Higher-Order Function
* */
fun main(args: Array<String>) {

    val program = Program()

    program.addTwoNumbers(2, 7)     // Simple way... for better understanding

    program.addTwoNumbers(2, 7, object : MyInterface {   // Using Interface / OOPs way

        override fun execute(sum: Int) {
            println(sum)    // Body
        }
    })

    val test: String = "Hello"

    val myLambda: (Int) -> Unit = { s: Int -> println(s)}   // Lambda Expression [ Function ]
    program.addTwoNumbers(2, 7, myLambda)
}

class Program {

    fun addTwoNumbers(a: Int, b: Int, action: (Int) -> Unit) {  // High Level Function with Lambda as Parameter

        val sum = a + b
        action(sum)     // println(sum)
//        println(sum)  // Body
    }

    fun addTwoNumbers(a: Int, b: Int, action: MyInterface) {    // Using Interface / Object Oriented Way
        val sum = a + b
        action.execute(sum)
    }

    fun addTwoNumbers(a: Int, b: Int) {                         // Simple way.. Just for Better Understanding

        val sum =  a + b
        println(sum)
    }
}

interface MyInterface {
    fun execute(sum: Int)
}


/*
    EXAMPLE TWO

*   1. Lambda Expression
*   2. Higher-Order Function
* */
fun main(args: Array<String>) {

    val program = MyProgram()

//    val myLambda: (Int, Int) -> Int = { x, y -> x + y}  // Lambda Expression [ Function ]
// OR,
//    program.addTwoNumbers(2, 7, { x, y -> x + y })
// OR,
    program.addTwoNumbers(2, 7) {x, y -> x + y}
}

class MyProgram {

    fun addTwoNumbers(a: Int, b: Int, action: (Int, Int) -> Int) {  // High Level Function with Lambda as Parameter

        val result = action(a, b)
        println(result)
    }
}



/*
*   1. Closures
* */
fun main(args: Array<String>) {

    val program = TheProgram()

    var result = 0

    program.addTwoNumbers(2, 7) {x, y -> result = x + y}

    println(result)
}

class TheProgram {

    fun addTwoNumbers(a: Int, b: Int, action: (Int, Int) -> Unit) {  // High Level Function with Lambda as Parameter

        action(a, b)
    }
}


/*
*   1. 'it' keyword
* */
fun main(args: Array<String>) {

    val program = Programs()
    program.reverseAndDisplay("hello", { it.reversed() })
}

class Programs {

    fun reverseAndDisplay(str: String, myFunc: (String) -> String) {  // High Level Function with Lambda as Parameter

        val result = myFunc(str)    // it.reversed() ==> str.reversed() ==> "hello".reversed() = "olleh"
        println(result)
    }
}


/*
*   1. 'with' function
*   2. 'apply' function
* */
fun main(args: Array<String>) {

    var person = Perrson()

    with(person) {
        name = "Steve"
        age = 23
    }

    person.apply {
        name = "Steve"
        age = 23
    }.startRun()

    println(person.name)
    println(person.age)
}

class Perrson {

    var name: String = ""
    var age: Int = -1

    fun startRun() {
        println("Now I am ready to run")
    }
}

/*
*   1. Arrays
* */
fun main(args: Array<String>) {

    // Elements :   32  0   0   54  0
    // Index    :   0   1   2   3   4

    var myArray = Array<Int>(5) { 0 }   // Mutable. Fixed Size.
    myArray[0] = 32
    myArray[3] = 54
    myArray[1] = 11

    for (element in myArray) {              // Using individual elements (Objects)
        println(element)
    }

    println()

    for (index in 0..myArray.size - 1) {
        println(myArray[index])
    }
}


/*
*   1. List and ArrayList 
* */
fun main(args: Array<String>) {

    // Elements :
    // Index    :   0   1   2   3   4

//    var list = mutableListOf<String>()  // Mutable, No Fixed Size, Can Add or Remove Elements
//    var list = arrayListOf<String>()    // Mutable, No Fixed Size, Can Add or Remove Elements
    var list = ArrayList<String>()      // Mutable, No Fixed Size, Can Add or Remove Elements
    list.add("Yogi")        // 0
    list.add("Manmohan")    // 1
    list.add("Vajpayee")    // 2

//    list.remove("Manmohan")
//    list.add("Vajpayee")

    list[1] = "Modi"

    for (element in list) {             // Using individual elements (Objects)
        println(element)
    }
}



/*
*   1. Map and HashMap
* */
fun main(args: Array<String>) {

    // Map Tutorial: Key-Value pair
//    var myMap = HashMap<Int, String>()      // Mutable, READ and WRITE both, No Fixed Size
//    var myMap = mutableMapOf<Int, String>() // Mutable, READ and WRITE both, No Fixed Size
    var myMap = hashMapOf<Int, String>()      // Mutable, READ and WRITE both, No Fixed Size

    myMap.put(4, "Yogi")
    myMap.put(43, "Manmohan")
    myMap.put(7, "Vajpayee")

    myMap.put(43, "Modi")

    for (key in myMap.keys) {
        println("Element at $key = ${myMap[key]}")  // myMap.get(key)
    }
}




/*
*   1. Set and HashSet
* */
fun main(args: Array<String>) {

    // "Set" contains unique elements
    // "HashSet" also contains unique elements but sequence is not guaranteed in output

    var mySet = mutableSetOf<Int>( 2, 54, 3, 1, 0, 9, 9, 9, 8)  // Mutable Set, READ and WRITE both
//    var mySet = hashSetOf<Int>( 2, 54, 3, 1, 0, 9, 9, 9, 8)     // Mutable Set, READ and WRITE both

    mySet.remove(54)
    mySet.add(100)

    for (element in mySet) {
        println(element)
    }
}



/** FILTER
 * Returns a list containing only elements matching the given [predicate]
 * */

/** MAP
 * Returns a list containing the results of applying the given [transform] function
 * to each element in the original collection
 * */

fun main(args: Array<String>) {

    val myNumbers: List<Int> = listOf(2, 3, 4, 6, 23, 90)

    val mySmallNums = myNumbers.filter { it < 10 }    // OR { num -> num < 10 }
    for (num in mySmallNums) {
        println(num)
    }

    val mySquaredNums = myNumbers.map { it * it }     // OR { num -> num * num }
    for (num in mySquaredNums) {
        println(num)
    }

    var people = listOf<Pperson>(Pperson(10, "Steve"), Pperson(23, "Annie"), Pperson(17, "Sam"))
    var names = people.filter { person ->person.name.startsWith("S") }.map { it.name }

    for (name in names) {
        println(name)
    }
}

class Pperson(var age: Int, var name: String) {
    // Some other code..
}



/**
 *  PREDICATES
 * */
fun main(args: Array<String>) {

    val myNumbers = listOf(2, 3, 4, 6, 23, 90)

    val myPredicate = { num: Int -> num > 10 }

    val check1 = myNumbers.all( myPredicate )       // Are all elements greater than 10?
    println(check1)

    val check2 = myNumbers.any(myPredicate)         // Does any of these elements satisfy the predicate?
    println(check2)

    val totalCount: Int = myNumbers.count(myPredicate) // Number of elements that satify the predicate.
    println(totalCount)

    val num = myNumbers.find(myPredicate)           // Returns the first number that matches the predicate
    println(num)
}



fun main(args: Array<String>) {

    // WAP to find out length of name
    val name: String? = "Steve"     // change it to null and see the effect in output

    // 1. Safe Call ( ?. )
    // Returns the length if 'name' is not null else returns NULL
    // Use it if you don't mind getting NULL value
    println("The length of name is ${name?.length}")


    // 2. Safe Call with let ( ?.let )
    // It executes the block ONLY IF name is NOT NULL
    name?.let {
        println("The length of name is ${name.length}")
    }


    // 3. Elvis-operator ( ?: )
    // When we have nullable reference 'name', we can say "is name is not null", use it,
    // otherwise use some non-null value"
    val len = if (name != null)
        name.length
    else
        -1

    val length = name?.length ?: -1
    println("The length of name is ${length}")

    // 4. Non-null assertion operator ( !! )
    // Use it when you are sure the value is NOT NULL
    // Throws NullPointerException if the value is found to be NULL

    println("The length of name is ${name!!.length}")
}


fun main(args: Array<String>) {

    val country = Country()

//    country.name = "India"
//    println(country.name)

    country.setup()
}

class Country {

    lateinit var name: String

    fun setup() {
        name = "USA"
        println("The name of country is $name")
    }
}

// lateinit used only with mutable data type [ var ]
// lateinit used only with non-nullable data type
// lateinit values must be initialised before you use it

// If you try to access lateinit variable without initializing it then it throws UninitializedPropertyAccessException



val pi: Float by lazy {
    3.14f
}

fun main(args: Array<String>) {

    println("Some initial code.....")

    // pi is not initialised yet

    val area1 = pi * 4 * 4      // pi gets initialised and assigned the value of 3.14f for the first time

    val area2 = pi * 9 * 9      // The value pi is loaded from cache memory

    println("Some more code....")
}


// ‘lazy initialization’ was designed to prevent unnecessary initialization of objects.
// You variables will not be initialised unless you use it in your code
// It is initialized only once. Next time when you use it, you get the value from cache memory.

// It is thread safe
// It is initialized in the thread where it is used for the first time.
// Other threads use the same value stored in the cache

// The variable can be var or val.
// The variable can be nullable or non-nullable


class Person {
    var name: String = "Sriyank Siddhartha"
    var age: Int = 26
}

fun main() {

    /** Scope Function: 'with'
        Property 1: Refer to context object by using 'this'
        Property 2: The return value is the 'lambda result'  */

    val person = Person()

    val bio: String = with(person) {
        println(name)
        println(age)
        age + 5
        "He is a freak who loves to teach in his own way" // will be returned and stored in 'bio' String variable
    }

//    println("Age after five years is $ageAfterFiveYears")
    println(bio)
}

class Person {
    var name: String = ""
    var age: Int = 0
}

fun main() {

    /** Scope Function: 'apply'
    Property 1: Refer to context object by using 'this'
    Property 2: The return value is the 'context object'  */

    val person = Person().apply {
        name = "Sriyank Siddhartha"
        age = 26
    }

    with(person) {
        println(name)       // prints   Sriyank Siddhartha
        println(age)        // prints   26 
    }

    // Perform some other operations on 'person' object
    person.also {
        it.name = "Shreks from Smartherd"
        println("New name: ${it.name}")     // prints   New name: Shreks from Smartherd
    }
}


fun main() {

    /** Scope Function: 'also'              'ALSO PERFORM THE FOLLOWING EXTRA OPERATION'
    Property 1: Refer to context object by using 'it'
    Property 2: The return value is the 'context object'  */

    // Initialise numbersList 
    val numbersList: MutableList<Int> = mutableListOf(1, 2, 3)

    // Some other code... may be a function call or program to swap numbers (doesn't matter what code)

    // Operations on the 'numbersList'
    val duplicateNumbers = numbersList.also {
        println("The list elements are: $it")
        it.add(4)
        println("The list elements after adding an element: $it")
        it.remove(2)
        println("The list elements after removing an element: $it")
    }

    // duplicateNumbers will be same as numbersList
    println("Original numbers: $numbersList")
    println("Duplicate numbers: $duplicateNumbers")
}



fun main() {

    /** Scope Function: 'let'
    Property 1: Refer to context object by using 'it'
    Property 2: The return value is the 'lambda result'  */

    // Use 'let' function to avoid NullPointerException

    val name: String? = "Hello"

    // Execute the lambda expression only if the 'name' variable is NOT NULL
    val stringLength = name?.let {
        println(it.reversed())
        println(it.capitalize())
        it.length       // Will be returned and stored within stringLength variable 
    }

    println(stringLength)
}


class Person {
    var name: String = "Sriyank Siddhartha"
    var age: Int = 26
}

fun main() {

    /** Scope Function: 'run'
    Property 1: Refer to context object by using 'this'
    Property 2: The return value is the 'lambda result'  */

    // 'run' is combination of 'with' and 'let'
    // If you want to operate on a Nullable object and avoid NullPointerException then use 'run'

    val person: Person? = Person()

    val bio = person?.run {
        println(name)
        println(age)
        age + 5
        "He is a freak who loves to teach in his own way"   // will be returned and stored in 'bio' variable
    }

    println(bio)
}



import kotlin.concurrent.thread

fun main() {        // Executes in main thread

    println("Main program starts: ${Thread.currentThread().name}")

    thread {    // creates a background thread (worker thread)
        println("Fake work starts: ${Thread.currentThread().name}")
        Thread.sleep(1000)      // Pretend doing some work... may be file upload
        println("Fake work finished: ${Thread.currentThread().name}")
    }

    println("Main program ends: ${Thread.currentThread().name}")
}



import kotlinx.coroutines.*


fun main() {        // Executes in main thread

    println("Main program starts: ${Thread.currentThread().name}")

    GlobalScope.launch {    // creates a background coroutine that runs on a background thread
        println("Fake work starts: ${Thread.currentThread().name}")
        Thread.sleep(1000)      // Pretend doing some work... may be file upload
        println("Fake work finished: ${Thread.currentThread().name}")
    }

    // Blocks the current main thread & wait for coroutine to finish (practically not a right way to wait)
    Thread.sleep(2000)
    println("Main program ends: ${Thread.currentThread().name}")
}


import kotlinx.coroutines.*


fun main() = runBlocking {        // Executes in main thread

        println("Main program starts: ${Thread.currentThread().name}")  // main thread

        GlobalScope.launch {    // Thread: T1
            println("Fake work starts: ${Thread.currentThread().name}")     // Thread: T1
            delay(1000)   // Coroutine is suspended but Thread: T1 is free (not blocked)
            println("Fake work finished: ${Thread.currentThread().name}") // Either T1 or some other thread.
        }

        delay(2000)  // main thread: wait for coroutine to finish (practically not a right way to wait)

        println("Main program ends: ${Thread.currentThread().name}")    // main thread
}


import kotlinx.coroutines.*


fun main() = runBlocking {        // Executes in main thread

        println("Main program starts: ${Thread.currentThread().name}")  // main thread

        GlobalScope.launch {    // Thread: T1
            println("Fake work starts: ${Thread.currentThread().name}")     // Thread: T1
            mySuspendFunc(1000)   // Coroutine is suspended but Thread: T1 is free (not blocked)
            println("Fake work finished: ${Thread.currentThread().name}") // Either T1 or some other thread.
        }

        mySuspendFunc(2000)  // main thread: wait for coroutine to finish (practically not a right way to wait)

        println("Main program ends: ${Thread.currentThread().name}")    // main thread
}

suspend fun mySuspendFunc(time: Long) {
    // code..
    delay(time)
}


import kotlinx.coroutines.*


fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val job: Job = launch {   // Thread: main
        println("Fake work starts: ${Thread.currentThread().name}")     // Thread: main
        delay(1000)   // Coroutine is suspended but Thread: main is free (not blocked)
        println("Fake work finished: ${Thread.currentThread().name}") // Thread: main
    }

    job.join()      // main thread: wait for coroutine to finish 

    println("Main program ends: ${Thread.currentThread().name}")    // main thread
}


import kotlinx.coroutines.*


fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val deferredJob: Deferred<Unit> = async   // Thread: main
        println("Fake work starts: ${Thread.currentThread().name}")     // Thread: main
        delay(1000)   // Coroutine is suspended but Thread: main is free (not blocked)
        println("Fake work finished: ${Thread.currentThread().name}") // Thread: main
        15
    }

    val num: Int = deferredJob.await()  // main thread: wait for coroutine to finish and return data 

    println("Main program ends: ${Thread.currentThread().name}")    // main thread
}



import kotlinx.coroutines.*


fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val deferredJob: Deferred<Unit> = async   // Thread: main
        println("Fake work starts: ${Thread.currentThread().name}")     // Thread: main
        delay(1000)   // Coroutine is suspended but Thread: main is free (not blocked)
        println("Fake work finished: ${Thread.currentThread().name}") // Thread: main
        15
    }

    val num: Int = deferredJob.await()  // main thread: wait for coroutine to finish and return data 

    println("Main program ends: ${Thread.currentThread().name}")    // main thread
}

suspend fun myOwnSuspendingFunc() {
    delay(1000)     // do something
}



import kotlinx.coroutines.runBlocking
import org.junit.Assert
import org.junit.Test

class SimpleTest {

    @Test
    fun myFirstTest() = runBlocking {
        myOwnSuspendingFunc()
        Assert.assertEquals(10, 5 + 5)
    }
}




import kotlinx.coroutines.*

fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val job: Job = launch {     // Thread main: Creates a non-blocking coroutine
        for (i in 0..500) {
            print("$i.")
            yield()     // or use delay() or any other suspending function as per your need.
        }
    }

    delay(10)  // Let's print a few values before we cancel
    job.cancelAndJoin()

    println("\nMain program ends: ${Thread.currentThread().name}")    // main thread
}




import kotlinx.coroutines.*

fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val job: Job = launch(Dispatchers.Default) {     // Thread T1: Creates a non-blocking coroutine
        for (i in 0..500) {
            if (!isActive) {
                return@launch // break
            }
            print("$i.")
            Thread.sleep(1)
        }
    }

    delay(10)  // Let's print a few values before we cancel
    job.cancelAndJoin()

    println("\nMain program ends: ${Thread.currentThread().name}")    // main thread
}



import kotlinx.coroutines.*

fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val job: Job = launch(Dispatchers.Default) {     // Thread T1: Creates a non-blocking coroutine
        try {
            for (i in 0..500) {
                print("$i.")
                delay(5)    // or use yield() or any other suspending function as per your need.
            }
        } catch (ex: CancellationException) {
            print("\nException caught safely: ${ex.message}")
        } finally {
            print("\nClose resources in finally")
        }
    }

    delay(10)  // Let's print a few values before we cancel
    job.cancel(CancellationException("My own crash message"))
    job.join()

    println("\nMain program ends: ${Thread.currentThread().name}")    // main thread
}


import kotlinx.coroutines.*

fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val job: Job = launch(Dispatchers.Default) {     // Thread T1: Creates a non-blocking coroutine
        try {
            for (i in 0..500) {
                print("$i.")
                delay(5)    // or use yield() or any other suspending function as per your need.
            }
        } catch (ex: CancellationException) {
            print("\nException caught safely: ${ex.message}")
        } finally {
            withContext(NonCancellable) {
                delay(1000)     // Generally we don't use suspending function in finally
                print("\nClose resources in finally")
            }
        }
    }

    delay(10)  // Let's print a few values before we cancel
    job.cancel(CancellationException("My own crash message"))
    job.join()

    println("\nMain program ends: ${Thread.currentThread().name}")    // main thread
}



import kotlinx.coroutines.*

fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    withTimeout(1300) {
        try {
            for (i in 0..1000) {
                print("$i.")
                delay(500)
            }
        } catch (ex: TimeoutCancellationException) {
            // .. code..
        } finally {
            // .. code..
        }
    }

    println("\nMain program ends: ${Thread.currentThread().name}")    // main thread
}


import kotlinx.coroutines.*

fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val result: String? = withTimeoutOrNull(2000) {
        for (i in 0..500) {
            print("$i.")
            delay(500)
        }

        "I am done"
    }

    print("Result: $result")

    println("\nMain program ends: ${Thread.currentThread().name}")    // main thread
}


import kotlinx.coroutines.*
import kotlin.system.measureTimeMillis


fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val time = measureTimeMillis {
        val msgOne = getMessageOne()
        val msgTwo = getMessageTwo()
        println("The entire message is: ${msgOne + msgTwo}")
    }

    println("Completed in $time ms")
    println("Main program ends: ${Thread.currentThread().name}")    // main thread
}

suspend fun getMessageOne(): String {
    delay(1000L)    // pretend to do some work
    return "Hello "
}

suspend fun getMessageTwo(): String {
    delay(1000L)    // pretend to do some work
    return "World!"
}


import kotlinx.coroutines.*
import kotlin.system.measureTimeMillis


fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val time = measureTimeMillis {
        val msgOne: Deferred<String> = async {
            // ..more code..
            getMessageOne()
        }
        val msgTwo: Deferred<String> = async {
            // ..more code..
            getMessageTwo()
        }
        println("The entire message is: ${msgOne.await() + msgTwo.await()}")
    }

    println("Completed in $time ms")
    println("Main program ends: ${Thread.currentThread().name}")    // main thread
}

suspend fun getMessageOne(): String {
    delay(1000L)    // pretend to do some work
    return "Hello "
}

suspend fun getMessageTwo(): String {
    delay(1000L)    // pretend to do some work
    return "World!"
}


import kotlinx.coroutines.*

fun main() = runBlocking {    // Creates a blocking coroutine that executes in current thread (main)

    println("Main program starts: ${Thread.currentThread().name}")  // main thread

    val msgOne: Deferred<String> = async(start = CoroutineStart.LAZY) { getMessageOne() }
    val msgTwo: Deferred<String> = async(start = CoroutineStart.LAZY) { getMessageTwo() }
    println("The entire message is: ${msgOne.await() + msgTwo.await()}")

    println("Main program ends: ${Thread.currentThread().name}")    // main thread
}

suspend fun getMessageOne(): String {
    delay(1000L)    // pretend to do some work
    println("After working in getMessageOne()")
    return "Hello "
}

suspend fun getMessageTwo(): String {
    delay(1000L)    // pretend to do some work
    println("After working in getMessageTwo()")
    return "World!"
}

import kotlinx.coroutines.*

fun main() = runBlocking {

    println("runBlocking: $this")

    launch {
        println("launch: $this")

        launch(coroutineContext) {
            println("child launch: $this")
        }
    }

    async {
        println("async: $this")
    }

    println("... some other code...")
}

import kotlinx.coroutines.*

fun main() = runBlocking {

    // this: CoroutineScope instance
    // coroutineContext: CoroutineContext instance

    /* Without Parameter: CONFINED      [CONFINED DISPATCHER]
        - Inherits CoroutineContext from immediate parent coroutine.
        - Even after delay() or suspending function, it continues to run in the same thread.  */
    launch {
        println("C1: ${Thread.currentThread().name}")       // Thread: main
        delay(1000)
        println("C1 after delay: ${Thread.currentThread().name}")   // Thread: main
    }

    /* With parameter: Dispatchers.Default [similar to GlobalScope.launch { } ]
        - Gets its own context at Global level. Executes in a separate background thread.
        - After delay() or suspending function execution,
            it continues to run either in the same thread or some other thread.  */
    launch(Dispatchers.Default) {
        println("C2: ${Thread.currentThread().name}")   // Thread: T1
        delay(1000)
        println("C2 after delay: ${Thread.currentThread().name}")   // Thread: Either T1 or some other thread
    }

    /*  With parameter: Dispatchers.Unconfined      [UNCONFINED DISPATCHER]
        - Inherits CoroutineContext from the immediate parent coroutine.
        - After delay() or suspending function execution, it continues to run in some other thread.  */
    launch(Dispatchers.Unconfined) {
        println("C3: ${Thread.currentThread().name}")   // Thread: main
        delay(1000)
        println("C3 after delay: ${Thread.currentThread().name}")   // Thread: some other thread T1
    }

    launch(coroutineContext) {
        println("C4: ${Thread.currentThread().name}")       // Thread: main
        delay(1000)
        println("C4 after delay: ${Thread.currentThread().name}")   // Thread: main 
    }

    println("...Main Program...")
}


/*
 *   Interoperability Example
 */
public class MyJavaFile {

	public static void main(String[] args) {

		int sum = MyKotlinInteroperabilityKt.addNumbers(3, 4);
		System.out.println("Printing sum from Java file :" + sum);
	}

	public static int getArea(int l, int b) {
		return l * b;
	}
}

/*
 *   Interoperability Example
 */
fun main(args: Array<String>) {

    var area = MyJavaFile.getArea(10, 5)
    println("Printing area from Kotlin file: $area")
}

fun addNumbers(a: Int, b: Int): Int {
    return a + b
}
