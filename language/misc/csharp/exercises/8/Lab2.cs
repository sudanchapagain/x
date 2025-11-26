using System;
using System.Linq;
using System.Collections.Generic;

public class Employee
{
    public int Id { get; set; }
    public string Name { get; set; } = "";
    public string Department { get; set; } = "";
}

public static class Lab2
{
    public static void Run()
    {
        List<Employee> employees = new List<Employee>
        {
            new Employee { Id = 1, Name = "Sudan", Department = "IT" },
            new Employee { Id = 2, Name = "Ram", Department = "HR" },
        };

        var sorted = employees.OrderByDescending(e => e.Name);

        foreach (var emp in sorted)
            Console.WriteLine($"{emp.Id} - {emp.Name} - {emp.Department}");

    }
}

