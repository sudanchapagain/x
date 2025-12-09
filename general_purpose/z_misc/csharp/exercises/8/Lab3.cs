using System;
using System.Linq;
using System.Collections.Generic;

    public class Student
    {
        public string Name { get; set; } = "";
        public string Address { get; set; } = "";
        public string Campus { get; set; } = "";
    }

    public static class Lab3
    {
        public static void Run()
        {
            List<Student> students = new List<Student>
            {
                new Student { Name="Sita", Address="Kirtipur", Campus="Patan Campus" },
                new Student { Name="Ram", Address="Lalitpur", Campus="Patan Campus" },
                new Student { Name="Hari", Address="Kirtipur", Campus="TU Campus" }
            };

            var filtered = students.Where(s => s.Address == "Kirtipur" && s.Campus == "Patan Campus");

            foreach (var s in filtered)
                Console.WriteLine($"{s.Name} - {s.Address} - {s.Campus}");
        }
    }


