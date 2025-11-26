namespace Lab02;

public static class Lab5
{
    public static void Run()
    {
        Console.Write("Enter number: ");
        var number = Convert.ToInt32(Console.ReadLine());

        if (number < 2)
        {
            Console.WriteLine($"{number} is not a prime number.");
        }
        else
        {
            var isPrime = true;
            for (var i = 2; i * i <= number; i++)
            {
                if (number % i != 0) continue;
                isPrime = false;
                break;
            }

            Console.WriteLine(isPrime ? $"{number} is a prime number." : $"{number} is not a prime number.");
        }
    }
}
