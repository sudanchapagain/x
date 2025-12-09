namespace Lab02;

public static class Lab8
{
    public static void Run()
    {
        Console.Write("Enter the position (n) of the Fibonacci number: ");
        var input = Console.ReadLine()!;

        if (int.TryParse(input, out var n) && n >= 0)
        {
            int a = 0, b = 1, result = 0;

            if (n == 0)
            {
                result = 0;
            }
            else if (n == 1)
            {
                result = 1;
            }
            else
            {
                for (var i = 2; i <= n; i++)
                {
                    result = a + b;
                    a = b;
                    b = result;
                }
            }

            Console.WriteLine($"Fibonacci term at position {n} is {result}");
        }
        else
        {
            Console.WriteLine("Invalid input. Please enter a non-negative integer.");
        }
    }
}
