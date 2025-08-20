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

        // Control Flow
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

        // Array in Java
        int[] numbers = {5, 2, 9, 1, 6};
        Arrays.sort(numbers);
        System.out.println("Sorted array: " + Arrays.toString(numbers));
    }
}
