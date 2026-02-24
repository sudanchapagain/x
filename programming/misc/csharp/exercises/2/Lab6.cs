namespace Lab02;

public static class Lab6
{
    public static void Run()
    {
        Console.Write("Enter a number: ");
        var input = Console.ReadLine()!;

        if (int.TryParse(input, out var number))
        {
            var originalNumber = number;
            var numDigits = input.Length;
            var sum = 0;

            while (number > 0)
            {
                var digit = number % 10;
                sum += (int)Math.Pow(digit, numDigits);
                number /= 10;
            }

            if (sum == originalNumber)
            {
                Console.WriteLine($"{originalNumber} is an Armstrong number.");
            }
            else
            {
                Console.WriteLine($"{originalNumber} is not an Armstrong number.");
            }
        }
        else
        {
            Console.WriteLine("Invalid input. Please enter a valid integer.");
        }
        
    }
}
