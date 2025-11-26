namespace Lab03;

public static class Lab5
{
    public static void Run()
    {
        var jagged = new int[3][];
        jagged[0] = [1, 2, 3];
        jagged[1] = [4, 5];
        jagged[2] = [6, 7, 8, 9];

        Console.WriteLine("jagged array elements:");
        foreach (var t in jagged)
        {
            foreach (var t1 in t)
            {
                Console.Write($"{t1} ");
            }

            Console.WriteLine();
        }

    }
}