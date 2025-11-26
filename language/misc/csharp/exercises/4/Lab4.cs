namespace Lab04;

public static class StaticDemo
{
    private static int Count;

    static StaticDemo()
    {
        Count = 0;
        Console.WriteLine("Static constructor called");
    }

    public static void Increment()
    {
        Count++;
        Console.WriteLine($"Count is now: {Count}");
    }
}

public static class MyStaticDemo
{
    public static void Run()
    {
        StaticDemo.Increment();
        StaticDemo.Increment();

    }
}
