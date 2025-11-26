public static class Lab3
{
    public static void Run()
    {
        Func<int, int> square = x => x * x;
        Console.WriteLine("square= " + square(5));

        Action<string> greet = name => Console.WriteLine("Hello, " + name);
        greet("Sudan");

        Predicate<int> isEven = x => x % 2 == 0;
        Console.WriteLine("isEven= " + isEven(10));

    }
}

