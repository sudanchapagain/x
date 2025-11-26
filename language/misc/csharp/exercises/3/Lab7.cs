namespace Lab03;

public static class Lab7
{
    public static void Run()
    {
        PrintDetails("Sudan");
        PrintDetails("Sudan", age: 20);
        PrintDetails(age: 21, name: "Prince");

    }

    private static void PrintDetails(string name, int age = 25)
    {
        Console.WriteLine($"Name: {name}, Age: {age}");
    }
}