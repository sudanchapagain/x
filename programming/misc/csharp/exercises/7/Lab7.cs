namespace Lab07;

public static class Lab7
{
    public static void Run()
    {
        Console.Write("Enter your name: ");
        string name = Console.ReadLine()!;

        try
        {
            if (name.Length > 10)
                throw new Exception("Name length cannot exceed 10 characters.");

            Console.WriteLine("Your name is: " + name);
        }
        catch (Exception ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }

    }
}
