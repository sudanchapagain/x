namespace Lab01;

public static class Lab3
{
    public static void Run()
    {
        Console.Write("Enter length: ");
        var length = Convert.ToInt32(Console.ReadLine());
        Console.Write("Enter breadth: ");
        var width = Convert.ToInt32(Console.ReadLine());
        Console.WriteLine($"Area of rectangle is: {length * width}");
        Console.WriteLine($"Perimeter of rectangle is: {length + width}");
    }
}