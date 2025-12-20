namespace Lab02;

public static class Lab9
{
    public static void Run()
    {
        var sum = 0;

        while (true)
        {
            Console.Write("Enter an integer (negative number to stop): ");
            var input = Console.ReadLine()!;

            if (int.TryParse(input, out var number))
            {
                if (number < 0)
                    break;

                sum += number;
            }
            else
            {
                Console.WriteLine("Invalid input. Please enter a valid integer.");
            }
        }

        Console.WriteLine($"Sum of entered integers: {sum}");
        
    }
}