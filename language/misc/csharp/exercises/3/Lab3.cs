namespace Lab03;

public static class Lab3
{
    public static void Run()
    {
        var matrix = new int[3, 3];
        Console.WriteLine("Enter elements of 3x3 matrix:");

        for (var i = 0; i < 3; i++)
		{
	        for (var j = 0; j < 3; j++)
	        {
	            Console.Write($"Element [{i},{j}]: ");
	            var input = Console.ReadLine()!;
	            if (int.TryParse(input, out var val))
	                matrix[i, j] = val;
	            else
	                matrix[i, j] = 0;
	        }
		}

        var sum = 0;
        for (var i = 0; i < 3; i++)
        {
            sum += matrix[i, i];
        }

        Console.WriteLine($"Sum of diagonal elements: {sum}");

    }
}
