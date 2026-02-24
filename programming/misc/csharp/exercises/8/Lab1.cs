using System;
using System.Linq;
using System.Collections.Generic;

public static class Lab1
{
    public static void Run()
    {
        List<int> numbers = new List<int> { 1, 3, 6, 7, 9, 12, 15, 18, 21 };

        var result = from n in numbers
                     where n % 2 != 0 && n % 3 == 0
                     select n;

        foreach (var num in result)
            Console.WriteLine(num);

    }
}
