namespace Lab02;

public static class Lab3
{
    public static void Run()
    {
        Console.Write("Enter number 1: ");
        var number1 = int.Parse(Console.ReadLine()!);
        Console.Write("Enter number 2: ");
        var number2 = int.Parse(Console.ReadLine()!);
        Console.Write("Enter number 3: ");
        var number3 = int.Parse(Console.ReadLine()!);

        if (number1 > number2)
        {
            if (number1 > number3)
            {
                Console.WriteLine("number 1 is the largest number");
            }
            else if (number3 > number2)
            {
                Console.WriteLine("number 3 is the largest number");
            }
        }
        else if (number2 > number3)
        {
            Console.WriteLine("number 2 is the largest number");
        }
        else if (number3 > number1)
        {
            Console.WriteLine("number 3 is the largest number");
        }
    }
}
