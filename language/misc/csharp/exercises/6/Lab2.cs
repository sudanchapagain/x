namespace Lab06;

public class Animal
{
    public virtual void Sound()
    {
        Console.WriteLine("Animal makes sound");
    }
}

public class Dog : Animal
{
    public override void Sound()
    {
        Console.WriteLine("Dog barks");
    }
}

public static class Lab2
{
    public static void Run()
    {
        Animal a = new Dog();
        a.Sound();

    }
}