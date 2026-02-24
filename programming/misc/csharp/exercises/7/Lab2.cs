public static class Lab2
{
    public static void Run()
    {
        Func<int, int, int> add = delegate(int a, int b)
        {
            return a + b;
        };
        Console.WriteLine("Anonymous Method: 3 + 4 = " + add(3, 4));

        Func<int, int, int> multiply = (a, b) => a * b;
        Console.WriteLine("Lambda Expression: 3 * 4 = " + multiply(3, 4));

    }
}

