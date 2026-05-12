fun main() {
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

    fun validate(age: Int) {
        if (age < 18) throw ArithmeticException("under 18 year person not allowed!")
        else println("$age is eligible for drive")
    }

    try {
        val res = 10/0
    } catch (e: ArithmeticException) {
        println("Divide by zero $e")
    }

    // Other common exceptions include:

    // - `ArithmeticException`
    // - `ArrayIndexOutOfBoundsException`
    // - `CharacterCodingException`
    // - `ClassCastException`
    // - `FileFailedToInitializeException`
    // - `IllegalArgumentException`
    // - `IllegalArgumentExceptionWithMessage`
    // - `IllegalStateException`
    // - `IllegalStateExceptionWithMessage`
    // - `IndexOutOfBoundsException`
    // - `InvalidReceiverTypeException`
    // - `KotlinNothingValueException`
    // - `NotImplementedError`
    // - `NoWhenBranchMatchedException`
    // - `NullPointerException`
    // - `NumberFormatException`
    // - `OutOfMemoryError`
    // - `RuntimeException`
    // - `TypeCastException`

}
