package src.inheritance;

// Interface, Inheritance, Method Overriding
interface exampleInterfaceto {
    public void nothing();
}

public class ExampleInterface implements exampleInterfaceto {
    @Override
    public void nothing() {
        System.out.println("Nothing");
    }
}
