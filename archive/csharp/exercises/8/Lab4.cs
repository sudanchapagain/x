using System;
using System.Linq;
using System.Collections.Generic;

public static class Lab4
{
    public static void Run()
    {
        List<int> numbers = new List<int> { 2, 4, 6, 8, 10 };

        Console.WriteLine("Sum: " + numbers.Sum());
        Console.WriteLine("Average: " + numbers.Average());
        Console.WriteLine("Minimum: " + numbers.Min());
        Console.WriteLine("Maximum: " + numbers.Max());
        Console.WriteLine("Count: " + numbers.Count());

    }
}
