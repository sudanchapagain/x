namespace Lab05;

public class Animal
{
    public static void Eat()
    {
        Console.WriteLine("Eating...");
    }
}

public class Dog : Animal
{
    public static void Bark()
    {
        Console.WriteLine("Barking...");
    }
}

public class Puppy : Dog
{
    public static void Weep()
    {
        Console.WriteLine("Weeping...");
    }
}

public class Cat : Animal
{
    public static void Meow()
    {
        Console.WriteLine("Meowing...");
    }
}

public interface IFlyable
{
    void Fly();
}

public interface ISwimmable
{
    void Swim();
}

public class Duck : IFlyable, ISwimmable
{
    public void Fly()
    {
        Console.WriteLine("Flying...");
    }

    public void Swim()
    {
        Console.WriteLine("Swimming...");
    }
}

public static class Lab2
{
    public static void Run()
    {
        var dog = new Dog();
        Animal.Eat();
        Dog.Bark();

        var puppy = new Puppy();
        Animal.Eat();
        Dog.Bark();
        Puppy.Weep();

        var cat = new Cat();
        Animal.Eat();
        Cat.Meow();

        var duck = new Duck();
        duck.Fly();
        duck.Swim();

    }
}