namespace Lab02;

public static class Lab10
{
    public static void Run()
    {
        Console.Write("Enter a positive integer n: ");
        var input = Console.ReadLine()!;

        if (int.TryParse(input, out var n) && n > 0)
        {
            var i = 1;

            start:
            if (i <= n)
            {
                var cube = i * i * i;
                Console.WriteLine($"Cube of {i} is {cube}");
                i++;
                goto start;
            }
        }
        else
        {
            Console.WriteLine("Invalid input. Please enter a positive integer.");
        }
        
    }
}