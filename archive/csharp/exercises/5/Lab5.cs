using System.Collections.Generic;

namespace Lab05;

public class ContactManager
{
    private Dictionary<string, string> contacts = new Dictionary<string, string>();

    public string this[string name]
    {
        get => contacts.ContainsKey(name) ? contacts[name] : "Contact not found";
        set => contacts[name] = value;
    }

    public void DisplayAll()
    {
        foreach (var c in contacts)
            Console.WriteLine($"{c.Key} : {c.Value}");
    }
}

public static class Lab5
{
    public static void Run()
    {
        var cm = new ContactManager();

        cm["Sudan"] = "123-456-7890";
        cm["Maya"] = "987-654-3210";

        // Console.WriteLine($"Sudan's phone: {cm["Sudan"]}");
        // Console.WriteLine($"Maya's phone: {cm["Maya"]}");

        Console.WriteLine("\nAll contacts:");
        cm.DisplayAll();

    }
}
