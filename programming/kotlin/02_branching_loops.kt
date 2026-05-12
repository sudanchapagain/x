fun main() {
    val x1 = 5
    val y1 = 40

    if (x1 > y1) {
        print("x greater")
    } else {
        println("y greater")
    }

    val ans = if (x1 > y1) x1 else y1
    println(ans)

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

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

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // using if to choose different paths
    var condition = true
    if (condition) {
        // if condition is true, this gets executed
    } else {
        // if condition is false, this gets executed
    }

    // braces are not strictly required
    val a = 20
    val b = 10
    if (a > b) println("a")
    else println("b")

    // using if to set a value
    val x = 100
    val y = 1
    val more = if (x > y) x else y // more == 100
    val less = if (x < y) {
        println("x is smaller.")
        x // the last expression is the block's value
    } else {
        println("y is smaller.")
        y
    }

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // using when to choose different paths
    val year = 2010
    when (year) {
        2010 -> print("Froyo")
        2011 -> print("Ice Cream Sandwich")
        2008, 2009 -> print("The early days")
        in 2012..2015 -> {
            println("Jellybean through Marshmallow,")
            println("When things got interesting.")
        }
        else -> println("Some other era")
    }

    // using when to set a value
    val androidEra = when (year) {
        2010 -> "Froyo"
        2011 -> "Ice Cream Sandwich"
        2008, 2009 -> "The early days"
        in 2012..2015 -> {
            print("Jellybean through marshmallow")
            // the last expression is the block's value
            "When things got interesting"
        }
        else -> "Some other era"
    }

    // using when with conditionals to set a value
    val catsOwned = 2
    val dogsOwned = 1
    val judgement = when {
        catsOwned == 0 -> "No cats"
        catsOwned < 0 -> {
            print("Call the cat police!")
            // the last expression is the block's value
            "owes someone some cats"
        }
        catsOwned == 1 && dogsOwned == 1 -> "seeking balance"
        catsOwned > 0 && catsOwned < 3 -> "yay cats!"
        else -> "cat nirvana"
    }

    return when (obj) {
        is Int -> "Integer: ${obj + 1}"
        is String -> "String length: ${obj.length}" // once type is known, properties can be accessed.
        is Boolean -> "Boolean: $obj"
        !is Double -> "something something" // negation can be used too.
        else -> "Unknown type" // this will be reached only if passed value is Double.
    }

    // also we dont need to return any thing. we can just execute a statement upon the condition being true
    when {
        x is String -> println("yes string")
        else -> println("not string. :(")
    }

    // they can be combined
    when (obj) {
        is Int, is Long -> println("number")
    }

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // to maintain exhaustiveness we can make use of sealed classed.
    sealed class Result
    class Success(val data: String): Result()
    class Error(val message: String): Result()

    fun handle(res: Result): String = when (res) {
        is Success -> result.data
        is Error -> result.message
        // else is not required as all subclasses have been implemneted.
        // when not implemented the compiler tell us.
    }

    // a issue that comes up is due to JVM's type erasure. i.e. JVM is less type
    // safe / makes guarantees than the kotlin language. a good example is List
    // in kotlin List can be of strings, ints, etc alone. But since, JVM treats
    // lists as generic data type the following code cannot be written

    when (obj) {
        is List<String> -> println("string in list")
    }

    // the compiler rejects this, as JVM cannot check String inside the list at
    // runtime.
    when (obj) {
        is List<*> -> println("some kind of list ig")
    }
    // List<*> is a star projection. It means 'a list of unknown type', and this
    // is the safest generic check you can perform at runtime.
    // so we lose out on the element's type but smart casts do work.
    when (obj) {
        // btw the element's type become Any?
        is List<*> -> {
            println(obj.size) // ok
            println(obj[0]) // works
        }
    }

    // to discriminate based on generic argument, we have to inspect type
    when (obj) {
        is List<*> -> {
            if (obj.all { it is String }) {
                println("list of string")
            } else if (obj.all { it is Int }) {
                println("list of ints")
            }
        }
    }

    // however, this is runtime dependent and not enforced by the type system.
    // for large collections, this becomes expensive.

    // Reified type parameters change the situation, but only inside inline functions.
    // With reified, the compiler keeps the type information and allows proper is checks:
    inline fun <reified T> checkType(value: Any): Boolean {
        return value is T
    }
    // now you can write
    if (checkType<List<String>>(obj)) println("this works")

    // can be combined with when
    inline fun <reified T> match(value: Any) {
        when (value) {
            is T -> println("matched generic type")
            else -> println("not matched")
        }
    }

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

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

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

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

    // iterate over list or set
    for (item in listOrset) {
        println(item)
    }
    // iterate over map
    for ((key, value) in myMap) {
        println("$key -> $value")
    }
    // iterating over ranges
    for (i in 0..10) {} // 0 to 10
    for (i in 0 until 10) {} // 0 to 9
    for (i in 1..10 step 2) {} // 1, 3, 5, 7, 9
    for (i in 10 downTo 1) {} // 10 to 1
    // while and do while
    var x = 0
    while (x < 10) {
        x++
        println(x)
    }

    do {
        x--
        println(x)
    } while (x > 0)

    val range = 1..5
    for (i in range) {
        println("hello")
    }

    if (3 in range) {
        println("3 exists")
    }

    // array & ranges
    for (x in arr) {
        sum += x
    }

    // this compiles to simple indexed loop. i.e.
    for (i in 0 until arr.size) {
        sum += arr[i]
    }

    // however, for Array<Int>,
    // each reference is dereferenced to its value,
    // Integer is unboxed to int
    // finally the value is obtained.
}
