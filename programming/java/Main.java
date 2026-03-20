import src.*;
import src.inheritance.*;
import src.inheritance.abstractClass.*;

/*
( JDK - Java Development Tools (JRE + Development Toools)
  ( JRE - Java Runtime Environemnt (jvm + library class)
    ( JVM - Java Virtual Machine   {JIT - just in time )))

  - JDK: compiler (javac), archiver (jar), docs gen (javadoc), debuggers, monitoring tools
  - JRE: UI toolkits (Swing, AWT, JavaFX), deployment tech (Java Web Start, applets),
      integration libs (JDBC, RMI, CORBA), base libraries (java.*, javax.*)
  - JVM: class loader subsystem, bytecode verifier, bytecode interpreter, jit compiler
*/


public class Main {
    public static void main(String[] args) {
        String str[] = {
                "10",
                "20"
        };
        Basics basic = new Basics();
        Basics.basics(str);
        System.out.println("---------------------------------");

        // classes, interface, abstract class, inheritance, method overriding
        ExampleAbstractClass eac = new ExampleAbstractClass();
        eac.something();
        ExampleInterface ei = new ExampleInterface();
        ei.nothing();
        IntegerCalculator ic = new IntegerCalculator();
        ic.add(20, 25);
        ic.subtract(20, 25);
        ic.multiple(20, 25);
        ic.divide(20, 25);
        System.out.println("---------------------------------");

        // constructors, method overloading (constructor overloading implementation)
        ConstClass constClass = new ConstClass();
        constClass.consto();
        System.out.println("---------------------------------");

        // exception handling
        ExceptionHandle eh = new ExceptionHandle();
        eh.Exceptioner();
        System.out.println("--------------------------------");

        // collections
        Collect col = new Collect();
        col.collect();
        System.out.println("-------------------------------");

        // generics
        GenericClass<String> stringBox = new GenericClass<>("Hello");
        System.out.println("String Box Content: " + stringBox.getContent());

        GenericClass<Integer> integerBox = new GenericClass<>(123);
        System.out.println("Integer Box Content: " + integerBox.getContent());

        Integer[] intArray = {1, 2, 3, 4};
        stringBox.printArray(intArray);
        String[] strArray = {"Apple", "Banana", "Cherry"};
        stringBox.printArray(strArray);
        System.out.println("-------------------------------");

        // Multithreading
        Threado th = new Threado();
        th.myThread();
        th.syncThread();
    }
}

class LearnMore {
    // vararg function
    public static void Names(String... n) {
        for (String i : n) {
            System.out.print(i + " "); 
        }
        System.out.println(); 
    }

    public static void caller(String[] args) {
        Names("geek1", "geek2");
        Names("geek1", "geek2", "geek3");
    }
}
