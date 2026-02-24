namespace Lab05;

public class BcaSemester5
{
    private string[] subjects =
    [
        "Dotnet Technology",
        "MIS and Ecommerce",
        "Introduction to Management",
        "Computer Graphics",
        "Computer Networks"
    ];

    public string this[int index]
    {
        get
        {
            if (index >= 0 && index < subjects.Length)
                return subjects[index];
            else
                return "Invalid index";
        }
    }
}

public static class Lab4
{
    public static void Run()
    {
        var sem5 = new BcaSemester5();
        Console.WriteLine("BCA 5th Semester Subjects:");
        for (var i = 0; i < 5; i++)
        {
            Console.WriteLine($"{i + 1}. {sem5[i]}");
        }

    }
}