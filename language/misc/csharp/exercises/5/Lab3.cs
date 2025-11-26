namespace Lab05;

public class Person
{
    private string _name = "Unknown";

    public int Age { get; set; } = 0; // auto property

    public string Name => _name; // read-only property

    private string _password = "";
    public string Password { set => _password = value; } // write-only property
}

public static class Lab3
{
    public static void Run()
    {
        var p = new Person();
        p.Age = 25;
        Console.WriteLine($"Age: {p.Age}");
        Console.WriteLine($"Name: {p.Name}");
        p.Password = "secret";

    }
}