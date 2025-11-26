package src.inheritance;

// interface, inheritance, method overriding
interface exampleInterfaceto {
    public void nothing();
}

public class ExampleInterface implements exampleInterfaceto {
    @Override
    public void nothing() {
        System.out.println("Nothing");
    }
}
