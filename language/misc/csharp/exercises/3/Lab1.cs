namespace Lab03;

public static class Lab1
{
    public static void Run()
    {
        Console.Write("Enter size of array: ");
        var input = Console.ReadLine()!;
        if (int.TryParse(input, out var n) && n > 0)
        {
            var arr = new int[n];
            var sum = 0;

            for (var i = 0; i < n; i++)
            {
                Console.Write($"Enter element {i + 1}: ");
                var eleInput = Console.ReadLine()!;
                if (int.TryParse(eleInput, out var ele))
                {
                    arr[i] = ele;
                    if (ele % 2 != 0)
                        sum += ele;
                }
                else
                {
                    Console.WriteLine("Invalid input, treated as 0.");
                    arr[i] = 0;
                }
            }

            Console.WriteLine($"Sum of odd elements: {sum}");
        }
        else
        {
            Console.WriteLine("Invalid size input.");
        }

    }
}
