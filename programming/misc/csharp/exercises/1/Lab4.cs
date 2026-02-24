namespace Lab01;

public static class Lab4
{
    public static void Run()
    {
        var a = 20;
        var b = 20;
        var c = a;
        a = b;
        b = c;
        Console.WriteLine($"a: {a}, b: {b}, c: {c}");

        var d = 30;
        var e = 20;
        (d, e) = (e, d);
        Console.WriteLine($"d: {d}, e: {e}");

    }
}