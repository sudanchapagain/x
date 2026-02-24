namespace Lab03;

public static class Lab10
{
    public static void Run()
    {
        var matrix = new int[3, 3];
        Console.WriteLine("Enter elements of 3x3 matrix:");

        for (var i = 0; i < 3; i++)
        for (var j = 0; j < 3; j++)
        {
            Console.Write($"Element [{i},{j}]: ");
            var input = Console.ReadLine()!;
            matrix[i, j] = int.TryParse(input, out var val) ? val : 0;
        }

        var determinant =
            matrix[0, 0] * (matrix[1, 1] * matrix[2, 2] - matrix[1, 2] * matrix[2, 1]) -
            matrix[0, 1] * (matrix[1, 0] * matrix[2, 2] - matrix[1, 2] * matrix[2, 0]) +
            matrix[0, 2] * (matrix[1, 0] * matrix[2, 1] - matrix[1, 1] * matrix[2, 0]);

        Console.WriteLine($"Determinant of the matrix is: {determinant}");

    }
}