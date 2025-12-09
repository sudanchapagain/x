namespace Lab04;

public class Box
{
    private float width;
    private float height;
    private float depth;

    public Box()
    {
        width = height = depth = 1.0f;
    }

    public Box(float w, float h, float d)
    {
        width = w;
        height = h;
        depth = d;
    }

    public float Volume()
    {
        return width * height * depth;
    }

    public float SurfaceArea()
    {
        return 2 * (width * height + height * depth + depth * width);
    }
}

public static class MyBox
{
    public static void Run()
    {
        var box1 = new Box(2.0f, 3.0f, 4.0f);
        var box2 = new Box(5.0f, 6.0f, 7.0f);

        Console.WriteLine($"Box 1 -> Volume: {box1.Volume()}, Surface Area: {box1.SurfaceArea()}");
        Console.WriteLine($"Box 2 -> Volume: {box2.Volume()}, Surface Area: {box2.SurfaceArea()}");

    }
}
