namespace Lab01;

public class Lab8
{
    public static void Run()
    {
        Console.Write("Enter the principal amount (P): ");
        var principal = double.Parse(Console.ReadLine()!);

        Console.Write("Enter the rate of interest (R) in percentage: ");
        var rateOfInterest = double.Parse(Console.ReadLine()!);

        Console.Write("Enter the time period (T) in years: ");
        var timePeriod = double.Parse(Console.ReadLine()!);

        var simpleInterest = (principal * rateOfInterest * timePeriod) / 100;
        Console.WriteLine($"The Simple Interest is: {simpleInterest}");
    }
}
