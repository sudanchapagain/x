public static class Lab5
{
    public static void Run()
    {
        try
        {
            int x = 10, y = 0;
            Console.WriteLine(x / y);
        }
        catch (DivideByZeroException ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }

        try
        {
            int[] arr = { 1, 2, 3 };
            Console.WriteLine(arr[5]);
        }
        catch (IndexOutOfRangeException ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }

        try
        {
            object obj = "Hello";
            int num = (int)obj; // invalid cast
        }
        catch (InvalidCastException ex)
        {
            Console.WriteLine("Error: " + ex.Message);
        }

    }
}

