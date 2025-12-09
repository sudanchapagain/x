namespace Lab03;

public static class Lab6
{
    public static void Run()
    {
        var x = 5;
        Console.WriteLine($"Original x before PassByValue: {x}");
        PassByValue(x);
        Console.WriteLine($"x after PassByValue: {x}");

        Console.WriteLine($"Original x before PassByReference: {x}");
        PassByReference(ref x);
        Console.WriteLine($"x after PassByReference: {x}");

    }

    private static void PassByValue(int a)
    {
        a = a + 10;
        Console.WriteLine($"Inside PassByValue: {a}");
    }

    private static void PassByReference(ref int a)
    {
        a = a + 10;
        Console.WriteLine($"Inside PassByReference: {a}");
    }
}