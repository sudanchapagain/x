public static class Lab7
{
    public static void Run()
    {
        Console.Write("Enter a positive integer: ");
        var input = Console.ReadLine()!;

        if (int.TryParse(input, out var number) && number > 0)
        {
            var sum = 0;

            for (var i = 1; i < number; i++)
            {
                if (number % i == 0)
                {
                    sum += i;
                }
            }

            if (sum == number)
            {
                Console.WriteLine($"{number} is a perfect number.");
            }
            else
            {
                Console.WriteLine($"{number} is not a perfect number.");
            }
        }
        else
        {
            Console.WriteLine("Invalid input. Please enter a positive integer.");
        }
        
    }
}
