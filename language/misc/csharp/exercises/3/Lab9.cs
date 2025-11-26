namespace Lab03;

public static class Lab9
{
    public static void Run()
    {
        int[] arr = [2, 3, 4, 5, 6, 7, 8, 9, 10];

        Console.WriteLine("Prime elements in the array:");
        foreach (var num in arr)
        {
            var isPrime = true;
            if (num <= 1)
                isPrime = false;
            else
            {
                for (var i = 2; i <= Math.Sqrt(num); i++)
                {
                    if (num % i == 0)
                    {
                        isPrime = false;
                        break;
                    }
                }
            }

            if (isPrime)
                Console.Write($"{num} ");
        }

        Console.WriteLine();

    }
}