namespace Lab02;

public static class Lab2
{
    public static void Run()
    {
        Console.Write("Enter an integer: ");
        var input = Console.ReadLine()!;

        if (int.TryParse(input, out var number))
        {
            if (number > 0)
            {
                Console.WriteLine("The number is positive.");
            }
            else if (number < 0)
            {
                Console.WriteLine("The number is negative.");
            }
            else
            {
                Console.WriteLine("The number is zero.");
            }
        }
        else
        {
            Console.WriteLine("Invalid input. Please enter a valid integer.");
        }
        
    }
}
