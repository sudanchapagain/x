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

// to implement multiple inheritance use interfaces
interface Add {
    void add(int a, int b);
}

interface Subtract {
    void subtract(int a, int b);
}

interface Multiply {
    void multiple(int a, int b);
}

interface Divide {
    void divide(int a, int b);
}

class IntegerCalculator implements Add, Subtract, Multiply, Divide {
    @Override
    public void add(int a, int b) {
        System.out.println(a + b);
    }

    @Override
    public void subtract(int a, int b) {
        System.out.println(a - b);
    }

    @Override
    public void multiply(int a, int b) {
        System.out.println(a * b);
    }

    @Override
    public void divide(int a, int b) {
        System.out.println(a / b);
    }
}


