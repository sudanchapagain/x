using System;

delegate void MyDelegate(string message);

public class Lab1
{
    static void Greet(string msg)
    {
        Console.WriteLine("Greeting: " + msg);
    }

    static void Farewell(string msg)
    {
        Console.WriteLine("Farewell: " + msg);
    }

    public static void Run()
    {
        MyDelegate d = Greet;
        d("Hello");

        d = Farewell;
        d("Bye");


        MyDelegate multi = Greet;
        multi += Farewell;

        multi("Everyone");

        multi -= Greet;
        multi("Sudan");
    }
}
