public static class Lab6
{
    public static void Run()
    {
        Console.Write("Enter balance: ");
        decimal balance = decimal.Parse(Console.ReadLine()!);

        Console.Write("Enter withdraw amount: ");
        decimal withdraw = decimal.Parse(Console.ReadLine()!);

        try
        {
            if (withdraw > balance)
                throw new Exception("Withdrawal amount exceeds balance!");

            decimal remaining = balance - withdraw;
            Console.WriteLine("Remaining Balance: " + remaining);
        }
        catch (Exception ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }

    }
}

