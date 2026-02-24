package src;

// constructors, method overloading (constructor overloading implementation)
public class ConstClass {
    public int x;

    public ConstClass(){
        System.out.println("Default constructor");
    }

    public ConstClass(int x){
        System.out.println("Parameterized constructor with x = " + x);
        this.x = x;
    }

    public ConstClass(ConstClass c){
        // creates deep copy
        this.x = c.x;
        System.out.println("copy constructor");
    }

    public void consto(){
        int c = 20;
        ConstClass cc2 = new ConstClass(20);
        ConstClass cc3 = new ConstClass(cc2);
    }
}
