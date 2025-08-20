import src.*;
import src.inheritance.*;
import src.inheritance.abstractClass.*;

public class Main {
    public static void main(String[] args) {
        // Basics
        String str[] = {"10", "20"};
        Basics basic = new Basics();
        basic.basics(str);
        System.out.println("---------------------------------");

        // Classes, Interface, Abstract class, inheritance, Method overriding
        ExampleAbstractClass eac = new ExampleAbstractClass();
        eac.something();
        ExampleInterface ei = new ExampleInterface();
        ei.nothing();
        System.out.println("---------------------------------");

        // Constructors, Method overloading (Constructor Overloading implementation)
        ConstClass constClass = new ConstClass();
        constClass.consto();
        System.out.println("---------------------------------");

        // Exception Handling
        ExceptionHandle eh = new ExceptionHandle();
        eh.Exceptioner();
        System.out.println("--------------------------------");

        // Collections
        Collect col = new Collect();
        col.collect();
        System.out.println("-------------------------------");

        // Generics
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
