namespace Lab04;

public class Time
{
    private int hours;
    private int minutes;
    private int seconds;

    public Time()
    {
        hours = minutes = seconds = 0;
    }

    public Time(int h, int m, int s)
    {
        hours = h;
        minutes = m;
        seconds = s;
        Normalize();
    }

    private void Normalize()
    {
        if (seconds >= 60)
        {
            minutes += seconds / 60;
            seconds %= 60;
        }

        if (minutes >= 60)
        {
            hours += minutes / 60;
            minutes %= 60;
        }

        hours %= 24;
    }

    public void DisplayTime()
    {
        Console.WriteLine($"{hours:D2}:{minutes:D2}:{seconds:D2}");
    }

    public Time Add(Time t)
    {
        var h = hours + t.hours;
        var m = minutes + t.minutes;
        var s = seconds + t.seconds;
        return new Time(h, m, s);
    }

    public Time Subtract(Time t)
    {
        var h = hours - t.hours;
        var m = minutes - t.minutes;
        var s = seconds - t.seconds;

        if (s < 0)
        {
            s += 60;
            m--;
        }
        if (m < 0)
        {
            m += 60;
            h--;
        }
        if (h < 0)
        {
            h += 24;
        }

        return new Time(h, m, s);
    }
}

public static class MyTime
{
    public static void Run()
    {
        var t1 = new Time(5, 45, 30);
        var t2 = new Time(2, 20, 40);

        Console.Write("Time 1: ");
        t1.DisplayTime();

        Console.Write("Time 2: ");
        t2.DisplayTime();

        var sum = t1.Add(t2);
        Console.Write("Sum: ");
        sum.DisplayTime();

        var diff = t1.Subtract(t2);
        Console.Write("Difference: ");
        diff.DisplayTime();

    }
}