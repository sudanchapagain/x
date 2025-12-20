namespace Lab02;

public static class Lab1
{
    public static void Run()
    {
        Console.Write("Enter number: ");
        var number = int.Parse(Console.ReadLine()!);
        Console.WriteLine(number % 2 == 0 ? "number is even" : "number is odd");
    }
}
