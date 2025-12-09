namespace Lab03;

public static class Lab4
{
    public static void Run()
    {
        var matrixA = new int[3, 3];
        var matrixB = new int[3, 3];
        var result = new int[3, 3];

        Console.WriteLine("Enter elements of matrix A (3x3):");
        for (var i = 0; i < 3; i++)
        for (var j = 0; j < 3; j++)
        {
            Console.Write($"A[{i},{j}]: ");
            var input = Console.ReadLine()!;
            matrixA[i, j] = int.TryParse(input, out var val) ? val : 0;
        }

        Console.WriteLine("Enter elements of matrix B (3x3):");
        for (var i = 0; i < 3; i++)
        for (var j = 0; j < 3; j++)
        {
            Console.Write($"B[{i},{j}]: ");
            var input = Console.ReadLine()!;
            matrixB[i, j] = int.TryParse(input, out var val) ? val : 0;
        }

        for (var i = 0; i < 3; i++)
        for (var j = 0; j < 3; j++)
        {
            var sum = 0;
            for (var k = 0; k < 3; k++)
            {
                sum += matrixA[i, k] * matrixB[k, j];
            }

            result[i, j] = sum;
        }

        Console.WriteLine("Resultant matrix (A x B):");
        for (var i = 0; i < 3; i++)
        {
            for (var j = 0; j < 3; j++)
                Console.Write(result[i, j] + "\t");
            Console.WriteLine();
        }

    }
}