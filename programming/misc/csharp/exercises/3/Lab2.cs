namespace Lab03;

public static class Lab2
{
    public static void Run()
    {
        Console.Write("Enter size of array: ");
        var input = Console.ReadLine()!;
        if (int.TryParse(input, out var n) && n > 0)
        {
            var arr = new int[n];

            for (var i = 0; i < n; i++)
            {
                Console.Write($"Enter element {i + 1}: ");
                var eleInput = Console.ReadLine()!;
                if (int.TryParse(eleInput, out var ele))
                {
                    arr[i] = ele;
                }
                else
                {
                    Console.WriteLine("Invalid input, treated as 0.");
                    arr[i] = 0;
                }
            }

            var min = arr[0];
            var max = arr[0];

            for (var i = 1; i < n; i++)
            {
                if (arr[i] < min)
                    min = arr[i];
                if (arr[i] > max)
                    max = arr[i];
            }

            Console.WriteLine($"Minimum element: {min}");
            Console.WriteLine($"Maximum element: {max}");
        }
        else
        {
            Console.WriteLine("Invalid size input.");
        }

    }
}