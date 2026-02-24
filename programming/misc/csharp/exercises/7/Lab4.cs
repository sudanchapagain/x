public static class Lab4
{
    public delegate void Notify(string message);

    public class Process
    {
        public event Notify? OnCompleted;

        public void Start()
        {
            Console.WriteLine("Process started...");
            Thread.Sleep(1000);
            OnCompleted?.Invoke("Process finished successfully!");
        }
    }

    public static void Run()
    {
        Process process = new Process();
        process.OnCompleted += msg => Console.WriteLine("Event received: " + msg);

        process.Start();

    }
}

