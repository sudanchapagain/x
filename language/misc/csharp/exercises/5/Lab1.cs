namespace Lab05;

public abstract class Shape
{
    public abstract double Area();
}

public class Rectangle(double w, double h) : Shape
{
    public override double Area()
    {
        return w * h;
    }
}

public static class Lab1
{
    public static void Run()
    {
        var rect = new Rectangle(5, 10);
        Console.WriteLine($"Rectangle area: {rect.Area()}");
    }
}