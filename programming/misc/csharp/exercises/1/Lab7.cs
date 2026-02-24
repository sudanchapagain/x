namespace Lab01;

public static class Lab7
{
    public static void Run()
    {
        Console.Write("Enter the temperature in Celsius: ");
        var celsius = double.Parse(Console.ReadLine()!);
        var fahrenheit = (celsius * 9 / 5) + 32;
        Console.WriteLine($"{celsius} Celsius is equal to {fahrenheit} Fahrenheit.");
    }
}
