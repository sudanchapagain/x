namespace Lab04;

public class Circle
{
    private float radius;

    public Circle()
    {
        radius = 1.0f;
    }

    public Circle(float r)
    {
        radius = r;
    }

    public float FindArea()
    {
        return 3.14159f * radius * radius;
    }

    public float FindCircumference()
    {
        return 2 * 3.14159f * radius;
    }
}

public static class MyCircle
{
    public static void Run()
    {
        var c1 = new Circle();
        var c2 = new Circle(5.5f);

        Console.WriteLine($"Circle 1 -> Radius: default, Area: {c1.FindArea()}, Circumference: {c1.FindCircumference()}");
        Console.WriteLine($"Circle 2 -> Radius: 5.5, Area: {c2.FindArea()}, Circumference: {c2.FindCircumference()}");

    }
}
