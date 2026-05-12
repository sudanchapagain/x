// variables, data types, string, string properties, string equality, string templates, nullable types, type inference

fun main(args: Array<String>) {
    // val is immuteable, var is muteable
    val x: Int = 2
    var y = 4 // type is inferred
    val z = 3
    y = 5
    println(x + y + z)

    var mutable: Int = 1
    mutable = 2
    val immutable: Double = 2.0 // as in re-assignment not allowed. the value might mutate.
    // immutable = 3.0 // Error: you can't reassign a val!
    var greeting = "Hello, world!" // type inferred as String

    const val name = "Sudan" // this is immutable constant. It is not allowed
                             // inside function definitions

    // Allowed - primitive types and strings
    const val MAX_COUNT = 100
    const val SERVICE_URL = "https://api.example.com"
    const val IS_DEBUGGING = true

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // Not allowed - complex types
    // const val DATE_NOW = Date() // Error: Only primitives and strings allowed

    // - Byte: 8 bits (-128 to 127), 1 byte
    // - Short: 16 bits () (-32,768 to 32,768), 2 byte
    // - Integer: 32 bits (+- 2.1 billion), 4 bytes
    // - Long: 64 bits, 8 bytes
    // - Float: 32 bits, 4 bytes (~6-7 decimal digits precision)
    // - Double: 64 bits (8 bytes), ~15-16 decimal digits
    // - Char: 16 bits (2 bytes). UTF-16 code unit
    // - Boolean: on JVM 1 byte. not implementation dependent.
    // - Strings: not fixed. contains UTF-16
    // - Triple quote strings: `"""` raw string literal. multi line text without escaping.

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

    println("Enter your name")
    val nameVar = readLine()
    println("Enter your age")
    val userInput: String? = readLine()
    val ageVar: Int = userInput?.toIntOrNull() ?: 0
    println("Your name is $nameVar and your age is $ageVar")
    // String templates starts with a dollar sign $ which consists either a
    // variable name or an arbitrary expression in curly braces.
    val testVal = "Olly"
    // String template as variable name
    println("Hello! my name is $testVal")
    // String template as arbitrary expression in curly braces
    println("Hello! my full name is ${testVal.plus(" and Oggy")}")

    val ii = 10
    val s = "Kotlin"
    println("ii = $ii")
    println("Length of $s is ${s.length}")
    val sb = StringBuilder()
    sb.append("Hello")
    sb.append(", world!")
    println(sb.toString())

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

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // Properties of String

    // length: Int -> It returns the length of string sequence.
    // indices: IntRange -> It returns the ranges of valid character indices
    //          from current char sequence.
    // lastIndex: Int -> It returns the index of last character from char sequence.

    var testingStr = "Kotlin"
    println(testingStr.length)
    println(testingStr.lastIndex)
    println(testingStr.indices)

    val text = """
        hello
        world
    """.trimIndent()

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

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

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

    // NULL SAFETY
    // -----------
    // declares name as nullable string
    var name: String? = "someone"
    name = null // assigned null

    var catchPhrase: String? = null // nullable type. i.e. Nullable String
    catchPhrase = "Hey, What's up, everybody?"

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

    // length1 contains name's length if name isn't null; null otherwise
    val length: Int? = name?.length // safe cast operator. `?.`

    // elvis operator ?:
    // length1 contains name's length if name isn't null; 0 otherwise
    val length2: Int = name?.length ?: 0
    // the elvis operator can also execute statements in the case of null values
    val length3 = name?.length ?: return

    // non-null assertion operator !!
    name = "Francis"
    val length4: Int = name!!.length // works if name isn't null; crashes otherwise

    // smart casts and checking for null
    var nonNullableAuthor: String
    var nullableAuthor: String?
    if (name != null) { // checking for null
        nonNullableAuthor = name // smart cast to String
    } else {
        nullableAuthor = name // Smart cast to String?
    }

    // var stre1: String? = "Hello"
    // println(stre1.length) // compile error

    // if (str1 != null) { // -> smart cast
    //     println(str1.length) // -> It works now!
    // }

    // While using is or !is for checking the variable,
    // the compiler tracks this information and internally cast the variable to target type.
    // This is done inside the scope if is or !is returns true.

    println(
        "123".toInt().toDouble()
    )

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

    val value: Any = "Kotlin"
    if (value is String) {
        println("It is string")
    }

    val obj: Any = "Kotlin"
    val casted = obj as String

    // you can use `as?` for safe type casts.

    val obj1: Any? = null
    val casted2 = obj1 as? String

    if (casted2 != null) {
        println(casted2.length)
    }

    // or
    println(
        (obj1 as? String)?.length
    )

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

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

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
    val text = """Kotlin is official language announce by Google for android application development"""
    println(text)
    println(text.trimMargin("^"))

    // `==` checks structural equality (calls `equals()`).
    // `===` checks referential equality (same object in memory).
    val a = "Hi"
    val b = "Hi"

    println(a == b) // true
    println(a === b) // may be true due to string interning

    // string interning means strings are constructed as global in JVM and
    // identifiers hold reference to it. so multiple identifiers will hold
    // same memory
}
