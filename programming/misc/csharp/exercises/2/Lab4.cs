namespace Lab02;

public static class Lab4
{
    public static void Run()
    {
        Console.Write("Enter a digit (0-9): ");
        var input = Console.ReadLine()!;

        if (int.TryParse(input, out var digit) && digit >= 0 && digit <= 9)
        {
            switch (digit)
            {
                case 0:
                    Console.WriteLine("Zero");
                    break;
                case 1:
                    Console.WriteLine("One");
                    break;
                case 2:
                    Console.WriteLine("Two");
                    break;
                case 3:
                    Console.WriteLine("Three");
                    break;
                case 4:
                    Console.WriteLine("Four");
                    break;
                case 5:
                    Console.WriteLine("Five");
                    break;
                case 6:
                    Console.WriteLine("Six");
                    break;
                case 7:
                    Console.WriteLine("Seven");
                    break;
                case 8:
                    Console.WriteLine("Eight");
                    break;
                case 9:
                    Console.WriteLine("Nine");
                    break;
            }
        }
        else
        {
            Console.WriteLine("Invalid input. Please enter a digit between 0 and 9.");
        }
        
    }
}
