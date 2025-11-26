namespace Lab06;

public class Number(int value)
{
    public int Value { get; private set; } = value;

    // a. ++ and --
    public static Number operator ++(Number n)
    {
        n.Value++;
        return n;
    }

    public static Number operator --(Number n)
    {
        n.Value--;
        return n;
    }

    // b. * and /
    public static Number operator *(Number a, Number b)
    {
        return new Number(a.Value * b.Value);
    }

    public static Number operator /(Number a, Number b)
    {
        return new Number(a.Value / b.Value);
    }

    // c. == and !=
    public static bool operator ==(Number a, Number b)
    {
        return a.Value == b.Value;
    }

    public static bool operator !=(Number a, Number b)
    {
        return a.Value != b.Value;
    }

    public override bool Equals(object? obj)
    {
        return obj is Number n && Value == n.Value;
    }

    public override int GetHashCode()
    {
        return Value.GetHashCode();
    }
}

public static class Lab3
{
    public static void Run()
    {
        var a = new Number(10);
        var b = new Number(2);
        var c = a * b;
        var d = a / b;

        Console.WriteLine($"Multiplication: {c.Value}");
        Console.WriteLine($"Division: {d.Value}");

        ++a;
        --b;

        Console.WriteLine($"Incremented a: {a.Value}");
        Console.WriteLine($"Decremented b: {b.Value}");

        Console.WriteLine($"a == b: {a == b}");
        Console.WriteLine($"a != b: {a != b}");

    }
}