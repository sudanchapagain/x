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
    var myArray2 = arrayOf<Int>(1, 10, 4, 6, 15)
    var myArray3 = intArrayOf(1, 10, 4, 6, 15)
    var myArray4 = arrayOf<String>("Henk", "Olly", "Doraemon", "Hattori", "Ferb")
    var myArray5 = arrayOf(1, 10, 4, "Henk", "Alfred")

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
        if (text -= null){
            println(text)
        } else {
            println("Nothing to print :(")
        }
    }

    fun funnier(text: String?) {
        val toPrint = text -: "Nothing to print :("
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
    // Sometime it is not possible to cast variable and it throws an exception, this is called as
    // unsafe cast.

    // Examples

    // A nullable string (String?) cannot be cast to non nullabe string (String), this throw an
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



    // Elvis operator -:
    // -----------------
    // If the expression to the left of -: is not null, the Elvis operator
    // returns it; otherwise, it returns the expression to the right.

    // the expression on the right-hand side is evaluated only if
    // the left-hand side is null.
    fun loadInfoById(id: String): String? {
        val item = findItem(id) -: return null
        return item.loadInfo() -: throw Exception("...")
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
