namespace Lab04;

public class Distance
{
    private int feet;
    private int inches;

    public Distance()
    {
        feet = 0;
        inches = 0;
    }

    public Distance(int f, int i)
    {
        feet = f;
        inches = i;
        Normalize();
    }

    private void Normalize()
    {
        if (inches >= 12)
        {
            feet += inches / 12;
            inches = inches % 12;
        }
    }

    public Distance AddDistance(Distance d)
    {
        int totalFeet = feet + d.feet;
        int totalInches = inches + d.inches;
        Distance result = new Distance(totalFeet, totalInches);
        result.Normalize();
        return result;
    }

    public void DisplayDistance()
    {
        Console.WriteLine($"{feet} feet {inches} inches");
    }
}

public static class MyDistance
{
    public static void Run()
    {
        var d1 = new Distance(5, 10);
        var d2 = new Distance(3, 4);

        Console.Write("Distance 1: ");
        d1.DisplayDistance();

        Console.Write("Distance 2: ");
        d2.DisplayDistance();

        var d3 = d1.AddDistance(d2);
        Console.Write("Sum of distances: ");
        d3.DisplayDistance();

    }
}