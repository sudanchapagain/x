namespace Lab06;

public static class Lab4
{
    public static void Run()
    {
        // List<T>
        var list = new List<int> { 1, 2, 3 };
        Console.WriteLine("List<T>:");
        list.ForEach(i => Console.Write(i + " "));
        Console.WriteLine();

        // Stack<T>
        var stack = new Stack<string>();
        stack.Push("One");
        stack.Push("Two");
        Console.WriteLine("Stack<T>:");
        while (stack.Count > 0)
            Console.WriteLine(stack.Pop());

        // Queue<T>
        var queue = new Queue<double>();
        queue.Enqueue(1.1);
        queue.Enqueue(2.2);
        Console.WriteLine("Queue<T>:");
        while (queue.Count > 0)
            Console.WriteLine(queue.Dequeue());

        // LinkedList<T>
        var linked = new LinkedList<char>();
        linked.AddLast('A');
        linked.AddLast('B');
        Console.WriteLine("LinkedList<T>:");
        foreach (var c in linked)
            Console.WriteLine(c);

        // Dictionary<TKey, TValue>
        var dict = new Dictionary<int, string>
        {
            [1] = "One",
            [2] = "Two"
        };
        Console.WriteLine("Dictionary<TKey, TValue>:");
        foreach (var kvp in dict)
        {
            Console.WriteLine($"{kvp.Key} = {kvp.Value}");
        }

    }
}
