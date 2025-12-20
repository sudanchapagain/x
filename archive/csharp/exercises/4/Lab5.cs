namespace Lab04;

public struct Company
{
    public string Name;
    public string Address;
    public string Phone;
    public float Salary;
}

public static class MyCompany
{
    public static void Run()
    {
        var comp = new Company();

        Console.Write("Enter company name: ");
        comp.Name = Console.ReadLine()!;

        Console.Write("Enter company address: ");
        comp.Address = Console.ReadLine()!;

        Console.Write("Enter company phone: ");
        comp.Phone = Console.ReadLine()!;

        Console.Write("Enter company salary: ");
        var salaryInput = Console.ReadLine()!;
        comp.Salary = float.TryParse(salaryInput, out var sal) ? sal : 0;

        Console.WriteLine("\nCompany details:");
        Console.WriteLine($"Name: {comp.Name}");
        Console.WriteLine($"Address: {comp.Address}");
        Console.WriteLine($"Phone: {comp.Phone}");
        Console.WriteLine($"Salary: {comp.Salary}");

    }
}
