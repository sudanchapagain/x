fun main() {
    // ARRAY

    // Kotlin Array can be created using arrayOf(),
    // intArrayOf(), charArrayOf(), booleanArrayOf(),
    // longArrayOf(), shortArrayOf(), byteArrayOf() functions.

    var myArray1 = arrayOf(1, 10, 4, 6, 15)
    var myArray3 = intArrayOf(1, 10, 4, 6, 15) // array of primitive ints (unboxed). much better cache locality, no boxing/unboxing
    var myArray4 = arrayOf<String>("Henk", "Olly", "Doraemon", "Hattori", "Ferb")
    var myArray5 = arrayOf<Any>(1, 10, 4, "Henk", "Alfred")

    // going out of bounds throws `ArrayIndexOutOfBoundsException`.
    // there is no wrapping or clamping of index.

    // Typed array: generic array and primitive array are different in kotlin
    var myArray2: Array<Int> = arrayOf<Int>(1, 10, 4, 6, 15) // generic array being typed as Int (boxed)
    // this is array of references to Integer object (On JVM) i.e. boxed types

    // primitive specific array that have no boxing are
    // - IntArray
    // - DoubleArray
    // - FloatArray
    // - LongArray
    // - ShortArray
    // - CharArray
    // - BooleanArray

    val nums = IntArray(3) { i -> i * 2 } // [0, 2, 4]
    // or
    val numss = intArrayOf(1, 2, 3)

    // this also means you cannot assign IntArray to Array<Int>,
    // IntArray is typically 2 to 5x faster in numeric loops
    // it uses significantly less memory
    // and finally it avoids GC pressure from many small objects

    // access elements of array
    println(myArray1[1])
    println(myArray1.get(1))

    // modify elements of array
    myArray1[1] = 35
    println(myArray1[1])
    myArray2.set(1, 95) // change second to be 95
    println(myArray1.get(1))

    // traverse elements of array
    for (e in myArray3) {
        print("$e ")
    }

    // traverse by using index
    for (i in 0..(myArray4.size - 1)) {
        print("${myArray1[i]} ")
    }

    // to store raw binary data you can use Byte arrays

    val bytes = byteArrayOf(1, 2, 3, 4)
    // common use filo i/o, network data, encoding and decoding
    val str = "hello world this is a string"
    val byt = str.toByteArray()

    // 2D arrays or any other dimensional array is just array of arrays as kotlin does not have true multidimensional array.

    val matrix = Array(2) { IntArray(3) }
    // [ [0,0,0],
    //   [0,0,0] ]

    // access
    matrix[0][1] = 5
    println(matrix[0][1])

    // we can make jagged ones
    val jag = arrayOf(
        intArrayOf(1, 2),
        intArrayOf(3, 4, 5)
    )

    // Strings are not arrays in Kotlin, but behavior is a bit same.

    val s = "hello"
    println(s[0]) // h
    // however it is immutable, and characters cannot be changed in place
    // we can tranform it to char array to modify
    val chars = s.toCharArray()
    chars[0] = 'H' // allowed


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
    val chars: CharArray = charArrayOf('a', 'b', 'c')
    for (ch in chars) {
        println(ch)
    }
    val bools: BooleanArray = booleanArrayOf(true, false, true)
    for (b in bools) {
        println(b)
    }
}
