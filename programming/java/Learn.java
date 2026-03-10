import java.util.Scanner;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import javax.swing.*;
import javax.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class Learn {
    public static void main(String[] args) {
        System.out.println("Sudan Chapagain");

        int a = Integer.parseInt(args[0]);
        int b = Integer.parseInt(args[1]);

        int sum = a + b;
        System.out.println("sum = " + sum);

        float cel = Float.parseFloat(args[2]);
        System.out.print("celcius to fahrenheit = "
          + ((9f / 5f) * cel + 32)
        );
    
        Scanner sc = new Scanner(System.in);
        System.out.println("input three numbers");
        int x = sc.nextInt();
        int y = sc.nextInt();
        int z = sc.nextInt();
        int great = (x > y && x > z) ? x : (y > z && y > x)? y : z;
        System.out.println(great);
        sc.close();

        var band = a & b;
        var bor = a | b;
        var bxor = a ^ b;
        var bnot = ~a;
        var brightshift = a >> 2;
        var bleftshift = b << 2;
        var bunsignedrightshift = a >>> 2;
        System.out.println("output of binary operations"
          + band + " "
          + bor + " "
          + bxor + " "
          + bnot + " "
          + brightshift + " "
          + bleftshift + " "
          + bunsignedrightshift
        );

        String str = "hello world!";
        var str01 = str.charAt(0);

        char[] chars = new char[5];
        str.getChars(0, 3, chars, 0); // str.getChars(srcBegin, srcEnd, dst, dstBegin)
        for (char c : chars) {
          System.out.print(c);
        }

        char[] charArr = str.toCharArray();
        String charArrToString = charArr.toString();
        var isCharEqualTest1 = str.equals(charArrToString);
        var isCharEqualTest2 = str.equalsIgnoreCase(charArrToString);
        var isCharEqualTest3 = str.compareTo(charArrToString);

        var getFirstThree = str.substring(0, 2);
        getFirstThree = getFirstThree.concat("Meow");
        getFirstThree = getFirstThree.replace("Meow", "\n");

        if (x % 2 == 0 ) {
            System.out.println("even");
        } if (x % 3 == 0 && x % 9 == 0){
            System.out.println("3 and 9 divisible");
        } else {
            System.out.println("odd");
        }

        switch (x) {
            case 1:
                System.out.print("1");
                break;
            default:
                break;
        }

        while (x < 100) {
            x++;
        }

        for (int i = 0; i < 200; i *= 2) {
          continue;
        }

        for (String argsString : args) {
          System.out.println(argsString);
        }

        Scanner scc = new Scanner(System.in);
        System.out.println("Enter radius");
        float r = scc.nextFloat();
        scc.close();

        Circle c = new Circle(r);
        System.out.println("Area: " + c.findArea());
        System.out.println("Circumference: " + c.findCircumference());

        Value vobj = new Value();
        int p = 15, q = 20;
        System.out.println("call by value");
        System.out.println("before method call: " + p + " " + q);
        vobj.method(x, y);
        System.out.println("after method " + p + " " + q);

        Reference robj = new Reference(15, 20);
        System.out.println("incall by reference");
        System.out.println("before method call: " + robj.a + " " + robj.b);
        robj.method(rboj);
        System.out.println("after method call" + robj.a + " " + robj.b);

        Apple apple = new apple("Nepali Apple", "sweet", "small");
        apple.eat();
        Orange orange = new Orange("red", "sour", "medium");
        orange.eat();

        IntegerCalculator ic = new IntegerCalculator();
        ic.add(20, 25);
        ic.subtract(20, 25);
        ic.multiple(20, 25);
        ic.divide(20, 25);

        try {
            int result = 10/0;
            System.out.println("Result: " + result);
        } catch (ArithmeticException e) {
            System.out.println("Arithmetic exception division by zero");
        }

        try {
            int[] array = {1, 2, 3};
            System.out.println("Array element at index: 3" + array[3]);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Array index out of bounds exception caught");
        }

        ThreadButMine tbm = new ThreadButMine();
        Thread t1 = new Thread(tbm);
        t1.start();

        ThreadButMine2 tbm2_1 = new ThreadButMine2();
        ThreadButMine2 tbm2_2 = new ThreadButMine2();
        ThreadButMine2 tbm2_3 = new ThreadButMine2();

        tbm2_1.setPriority(Thread.MAX_PRIORITY);
        tbm2_2.setPriority(Thread.MIN_PRIORITY);
        tbm2_3.setPriority(Thread.NORM_PRIORITY);

        tbm2_1.start();
        tbm2_2.start();
        tbm2_3.start();

        Callme target = new Callme();
        Caller cobj1 = new Calller(target, "How");
        Caller cobj2 = new Caller(target, "are");
        Caller cobj3 = new Caller(target, "you?");

        cobj1.start();
        cobj2.start();
        cobj3.start();

        try {
            cobj1.join();
            cobj2.join();
            cobj3.join();
        } catch (InterruptedException e) {
            System.out.println("interrupted");
        }

        // IO with file input & output streams
        try {
            var fout = new FileOutputStream("meow.txt");
            String stringForFile = "meow meow biralo";
            byte stringInBytes[] = stringForFile.getBytes();
            fout.write(stringInBytes);
            fout.close();

            var fin = new FileInputStream("meow.txt");
            int i = 0;
            int count = 0;
            while ((i = fin.read()) != -1) {
                char ch = (char)i;
                if (ch == 'a' || ch == 'e' || ch == 'i' || ch == 'o' || ch == 'u') {
                    count++;
                }
            }
            System.out.println("total number of vowels: " + count);
            fin.close();
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }

        // IO with file reader & writers
        try {
            var fout = new FileWriter("input.txt");
            String stringForIO = "MEOW MEOW BIRALO";
            fout.write(stringForIO);
            fout.close();

            var fin = new FileReader("meow.txt");
            int i = 0;
            while ((i = fin.read()) != -1) {
                System.out.print( (char)i );
            }
            fin.close();
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }

        new AreaPerimeter();
    }
}

class Circle {
    private float radius;

    Circle() {
        radius = 0;
    }

    Circle(float r) {
        radius = r;
    }

    float findArea() {
        return (float)(Math.PI * radius * radius);
    }

    float findCircumference() {
        return (float)(2 * Math.PI * radius);
    }

    int findCircumference(int radius) {
      return (int)(2 * MATH.PI * radius);
    }
}

class Value {
    void method(int i, int j) {
        i *= 2;
        j /= 2;
        System.out.println("Inside method: " + i + " " + j);
    }
}

class Reference {
    int a, b;
    Reference(int i, int j) {
        a = i;
        b = j;
    }

    void method(Reference obj) {
        obj.a *= 2;
        obj.b /= 2;
        System.out.println("Inside method: " + obj.a + " " + obj.b);
    }
}

// single inheritance, multi-level, multiple, hiearchial
// 

// no @ to method overload
// to override abstract method @override

abstract class Fruit {
    String name, taste, size;

    public Fruit(String name, String taste, String size) {
        this.name = name;
        this.taste = taste;
        this.size = size;
    }

    abstract void eat();
}

class Apple extends Fruit {
  public Apple(String name, String taste, String size) {
    super(name, taste, size);
  }

  @Override
  void eat() {
    System.out.println("Apple taste: " + taste);
  }
}

class Orange extends Fruit {
    public Orange(String name, String taste, String size) {
        super(name, taste, size);
    }

    @Override
    void eat() {
        System.out.println("Orange taste: " + taste);
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

// via implementing runnable interface
class ThreadButMine implements Runnable {
    public void run() {
        for (int i = 0; i < 5; i++) {
            System.out.println("thread0: " + i);
        }
    }
}

// via extending Thread class
class ThreadButMine2 extends Thread {
    public void run() {
        for (int i = 0; i < 5; i++) {
            System.out.println("thread1: " + i);
        }

        String name = Thread.currentThread().getName();
        int priority = Thread.currentThread().getPriority();
        System.out.println("name of thread: " + name + "with priority: " + priority);
    }
}

// synchronization
class Callme {
    synchronized void call(String msg) {
        System.out.println("[" +  msg);
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            System.out.println("Interrupted");
        }
        System.out.print("]");
    }
}

class Caller implements Runnabele {
    String msg;
    Callme target;
    Thread t;

    public Caller(Callme targ, String s) {
        target = targ;
        msg = s;
        t = new Thread(this);
    }

    public void run() {
        target.call(msg);
    }
}

// swing
class AreaPerimeter extends JFrame implements ActionListener {
    JTextField length, breadth, area, perimeter;
    JButton calculate;

    AreaPerimeter() {
        JLabel lengthLabel = new JLabel("Length");
        lengthLabel.setBounds(50, 175, 150, 20);
        add(lengthLabel);
        length = new JTextField();
        length.setBounds(50, 75, 150, 20);
        add(length);

        JLabel breadthLabel = new JLabel("Breadth");
        breadthLabel.setBounds(50, 100, 150, 20);
        add(breadthLabel);
        breadth = new JTextField();
        breadth.setBounds(50, 125, 150, 20);
        add(breadth);

        JLabel areaLabel = new JLabel("Area");
        areaLabel.setBounds(50, 150, 150, 20);
        add(areaLabel);
        area = new JTextField();
        area.setBounds(50, 175, 150, 20);
        area.setEditable(false);
        add(area);

        JLabel perimeterLabel = new JLabel("perimeter");
        perimeterLabel.setBounds(50, 200, 150, 20);
        add(perimeterLabel);
        perimeter.setBounds(50, 225, 150, 20);
        perimeter.setEditable(false);
        add(perimeter);

        Calculate.addActionListener(this);
        setSize(300, 400);
        setLayout(null);
        setVisible(true);
        setTitle("Area & Perimeter calculator");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

    public void actionPerformed(ActionEvent e) {
        double l = Double.parseDouble(length.getText());
        double b = Double.parseDouble(breadth.getText());
        area.setText(String.valueOf(l * b));
        perimeter.setText(String.valueOf(2 * (l + b)));
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
