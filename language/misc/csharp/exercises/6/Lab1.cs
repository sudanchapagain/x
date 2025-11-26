namespace Lab06;

public class Calculator
{
    public int Add(int a, int b) => a + b;
    public double Add(double a, double b) => a + b;
    public int Add(int a, int b, int c) => a + b + c;
}

public static class Lab1
{
    public static void Run()
    {
        var calc = new Calculator();
        Console.WriteLine($"Add(int, int): {calc.Add(5, 3)}");
        Console.WriteLine($"Add(double, double): {calc.Add(5.5, 3.3)}");
        Console.WriteLine($"Add(int, int, int): {calc.Add(1, 2, 3)}");

    }
}

