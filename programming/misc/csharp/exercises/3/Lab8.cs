namespace Lab03;

public static class Lab8
{
    public static void Run()
    {
        var sum1 = Sum(1, 2, 3);
        var sum2 = Sum(4, 5);
        var sum3 = Sum();

        Console.WriteLine($"Sum1: {sum1}");
        Console.WriteLine($"Sum2: {sum2}");
        Console.WriteLine($"Sum3: {sum3}");

    }

    private static int Sum(params int[] numbers)
    {
        var sum = 0;
        foreach (var num in numbers)
        {
            sum += num;
        }

        return sum;
    }
}