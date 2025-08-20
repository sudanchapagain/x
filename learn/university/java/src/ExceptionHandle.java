package src;

// Exception Handling
public class ExceptionHandle {
    public void Exceptioner() {
        /*
           ArithmeticException -> Arithmetic error, such as divide-by-zero.
           ArrayIndexOutOfBoundsException -> Array index is out-of-bounds.
           ArrayStoreException -> Assignment to an array element of an incompatible type.
           ClassCastException -> Invalid cast.
           EnumConstantNotPresentException -> An attempt is made to use an undefined enumeration value.
           IllegalArgumentException -> Illegal argument used to invoke a method.
           IllegalCallerException -> A method cannot be legally executed by the calling code.
           IllegalMonitorStateException -> Illegal monitor operation, such as waiting on an unlocked thread.
           IllegalStateException -> Environment or application is in incorrect state.
           IllegalThreadStateException -> Requested operation not compatible with current thread state.
           IndexOutOfBoundsException -> Some type of index is out-of-bounds.
           LayerInstantiationException -> A module layer cannot be created.
           NegativeArraySizeException -> Array created with a negative size.
           NullPointerException -> Invalid use of a null reference.
           NumberFormatException -> Invalid conversion of a string to a numeric format.
           SecurityException -> Attempt to violate security.
           StringIndexOutOfBoundsException -> Attempt to index outside the bounds of a string.
           TypeNotPresentException -> Type not found.
           UnsupportedOperationException -> An unsupported operation was encountered.
           ClassNotFoundException -> Class not found.
           CloneNotSupportedException -> Attempt to clone an object that does not implement the Cloneable interface.
           IllegalAccessException -> Access to a class is denied.
           InstantiationException -> Attempt to create an object of an abstract class or interface.
           InterruptedException -> One thread has been interrupted by another thread.
           NoSuchFieldException -> A requested field does not exist.
           NoSuchMethodException -> A requested method does not exist.
           ReflectiveOperationException -> Superclass of reflection-related exceptions.
        */
        try {
            int result = divide(10, 0);
            System.out.println("Division Result: " + result);
        } catch (ArithmeticException e) {
            System.out.println("Caught an exception: " + e.getMessage());
        } finally {
            System.out.println("This is the finally block.");
        }
    }

    public int divide(int a, int b) {
        return (a / b);
    }
}

// This program creates a custom exception type.
class MyException extends Exception {
    private int detail;

    MyException(int a) {
        detail = a;
    }

    public String toString() {
        return "MyException[" + detail + "]";
    }
}

class ExceptionDemo {
    static void compute(int a) throws MyException {
        System.out.println("Called compute(" + a + ")");
        if (a > 10) {
            throw new MyException(a);
        }
        System.out.println("Normal exit");
    }

    public static void main(String[] args) {
        try {
            compute(1);
            compute(20);
        } catch (MyException e) {
            System.out.println("Caught " + e);
        }
    }
}

// Demonstrate exception chaining.
class ChainExcDemo {
    static void demoproc() {
        // create an exception
        NullPointerException e = new NullPointerException("top layer");
        // add a cause
        e.initCause(new ArithmeticException("cause"));
        throw e;
    }

    public static void main(String[] args) {
        try {
            demoproc();
        } catch(NullPointerException e) {
            // display top level exception
            System.out.println("Caught: " + e);
            // display cause exception
            System.out.println("Original cause: " +
            e.getCause());
        }
    }
}

// The following program shows the multi-catch feature in action:
// Demonstrate the multi-catch feature.
class MultiCatch {
    public static void main(String[] args) {
        int a=10, b=0;
        int[] vals = { 1, 2, 3 };
        try {
            int result = a / b; // generate an ArithmeticException
            vals[10] = 19; // generate an ArrayIndexOutOfBoundsException
            // This catch clause catches both exceptions.
        } catch(ArithmeticException | ArrayIndexOutOfBoundsException e) {
            System.out.println("Exception caught: " + e);
        }
        System.out.println("After multi-catch.");
    }
}
