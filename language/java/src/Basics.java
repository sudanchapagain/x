package src;

import java.util.*;

public class Basics {
    public static void basics(String[] args) {
        // <https://docs.oracle.com/en/java/javase/17/>
        // int is 32bit.
        // byte is 8bit duh.
        // short 16bit
        // long is 64bit
        // float is 32bit
        // double is 64bit
        // char is 16bit
        // boolean 1byte
        // all these are primitives and allocated on the stack.
        // inlined into objects if fields
        boolean bool1 = true; // size is JVM-dependent
        char char1 = 'a'; // 2 bytes (16 bits, UTF-16)

        int int1 = 10; // 4 bytes (32 bits)
        float float1 = 10.5f; // 4 bytes (32 bits, IEEE 754)

        double double1 = 10.5; // 8 bytes (64 bits, IEEE 754)
        long long1 = 1000000000000000000L; // 8 bytes (64 bits)

        System.out.println("byte: " + Byte.BYTES + " bytes (" + Byte.SIZE + " bits)");
        System.out.println("short: " + Short.BYTES + " bytes (" + Short.SIZE + " bits)");
        System.out.println("int: " + Integer.BYTES + " bytes (" + Integer.SIZE + " bits)");
        System.out.println("long: " + Long.BYTES + " bytes (" + Long.SIZE + " bits)");
        System.out.println("float: " + Float.BYTES + " bytes (" + Float.SIZE + " bits)");
        System.out.println("double: " + Double.BYTES + " bytes (" + Double.SIZE + " bits)");
        System.out.println("char: " + Character.BYTES + " bytes (" + Character.SIZE + " bits)");

        // in hex format
        int hex1 = 0x1A; // decimal 26
        int hex2 = 0xFF; // decimal 255
        long hex3 = 0x7fffL; // decimal 32767, long literal
        char hexChar = 0x41; // 'A' (Unicode 65)

        // print decimals in hex format
        int num = 255;
        System.out.println("Decimal: " + num);
        System.out.println("Hex: 0x" + Integer.toHexString(num));

        /* simiarly for binary and octal */


        // boxed types are object counterparts of primitives.
        // int to Integer
        // boolean to Boolean, etc.
        // these are heap allocated. boxing and unboxing create temporary
        // objects and adds GC pressure
        // int x = 10;
        // Integer y = x; //boxing
        // int z = y; //unboxing

        // every object in java has
        // - header: metadata with mark word, class pointer. these are
        //           typically ~12 bytes.
        // - fields: memory for variables which are aligned by type.
        // - padding: to ensure alignment (8 byte alignment on 64 bit JVM)
        // 
        // An object with two int fields
        // = 12 (header)
        //  + 4
        //  + 4
        //  + 0 (padding)
        // = 20 -> rounded to 24 bytes as hotspot jvm (used by oracle,
        //         openjdk, etc) round all object sizes to a multiple of
        //         8-bytes on 64-bit architectures.
        // 
        // DEMONSTRATE WITH :
        // 
        // class A {
        //    int x;
        //    int y;
        // }
        // System.out.println(ClassLayout.parseClass(A.class).toPrintable());
        //
        // object references (like pointers) are typically 4 bytes or 8 bytes
        // depending upon the config.
        //
        // arrays in java are objects so they are heap allocated.
        // they have header, length field, and actual data
        // element layout is contiguous in memory for primitives
        // int[] a = new int[5];
        //
        // jagged arrays like int[][] a = new int[3][2];
        // each sub array is its own object. a[0], a[1] are references to
        // int[] objects. memory is not truly 2d
        //
        // strings are final and immutable.
        // it allows for safe sharing and caching.
        // java maintains a pool of unique string literals.
        // String a = "hello";
        // if b also equals to "hello" then both will refer to the same object
        // you can use a.intern() to manually add to the pool.
        //
        // heap is shared across threads in java where as stack is per thread
        // to write efficient java code:
        // - opt for primitives whenever possible
        // - minimize object allocation i.e. reuse objects.
        // - avoid autoboxing in loops or hot paths.
        // - use stringbuilder instead of + in loops.
        // - use array of ArrayList over LinkedList unless necessary.
        // - usr profilers (JFR, VisualVM, async-profiler)
        //
        // Java 17 uses G1GC by default.
        // to tunr GC some of the flags are (remember only tunr when GC is
        // bottleneck)
        // - `-Xms512m -Xmx2g`: heap size
        // - `-XX:+UseG1GC`
        // - `-XX:+PrintGCDetails -XX:+PrintGCDateStamps`
        //
        // <https://en.wikipedia.org/wiki/Escape_analysis>
        // java performs escape analysis. i.e. analyzes whether an object
        //      escapes the method (if so must go on heap). if it does not
        //      escape then it can be stack-allocated or eliminated.
        // it enables scalar replacement (breaking object into fields.) or
        // avoiding gc with stack allocation.
        // e.g.
        // void foo() {
        //     Point p = new Point(1,2); // may be stack allocated.
        // }
        // it is enabled by default but you can explicitly turn it on with
        // `-XX:+DoEscapeAnalysis`
        //
        // <https://shipilev.net/jvm/anatomy-quarks/>
        // java bytecode can be analyzed with `javap -c`
        System.out.println("Hello World");
        int xArgument = Integer.parseInt(args[0]);
        int yArgument = Integer.parseInt(args[1]);
        System.out.println("Result of sum: " + (xArgument + yArgument));
        final String name = "java";

        System.out.println("Bitwise AND: " + (xArgument & yArgument));
        System.out.println("Bitwise OR: " + (xArgument | yArgument));
        System.out.println("Bitwise XOR: " + (xArgument ^ yArgument));
        System.out.println("Bitwise NOT: " + (~xArgument));
        System.out.println("Bitwise Left Shift: " + (xArgument << 2));
        System.out.println("Bitwise Right Shift: " + (xArgument >> 2));
        System.out.println("Bitwise Unsigned Right Shift: " + (xArgument >>> 2));

        // control flow
        // if, if else, switch, for, while, do while, for each, break, continue,
        // return
        if ((xArgument & yArgument) != 0) {
        } else {
        }

        for (int x = 0; x < xArgument; x++) {
        }

        while (xArgument < yArgument) {
            break;
        }

        char option = 'a';
        int x = 0, y = 0;

        switch (option) {
            case '+':
                System.out.println("Addition=" + (x + y));
                break;
            case '-':
                System.out.println("Subtraction=" + (x - y));
                break;
            default:
                System.out.println("Wrong choice!");
                break;
        }

        // array in java
        int[] numbers = {5, 2, 9, 1, 6};
        Arrays.sort(numbers);
        System.out.println("Sorted array: " + Arrays.toString(numbers));
    }
}
