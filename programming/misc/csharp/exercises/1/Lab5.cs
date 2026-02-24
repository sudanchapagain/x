namespace Lab01;

public static class Lab5
{
    public static void Run()
    {
        Console.Write("Enter the first number: ");
        var num1 = int.Parse(Console.ReadLine()!);

        Console.Write("Enter the second number: ");
        var num2 = int.Parse(Console.ReadLine()!);

        Console.Write("Enter the third number: ");
        var num3 = int.Parse(Console.ReadLine()!);

        var largest = (num1 >= num2 && num1 >= num3) ? num1 :
            (num2 >= num1 && num2 >= num3) ? num2 : num3;

        var smallest = (num1 <= num2 && num1 <= num3) ? num1 :
            (num2 <= num1 && num2 <= num3) ? num2 : num3;

        Console.WriteLine($"Largest number: {largest}");
        Console.WriteLine($"Smallest number: {smallest}");
    }
}
