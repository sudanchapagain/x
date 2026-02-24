public class Lab2
{
    public static void Run()
    {
        Console.Write("Input the first number: ");
        var firstNumber = Convert.ToInt32(Console.ReadLine());
        Console.Write("Input the second number: ");
        var secondNumber = Convert.ToInt32(Console.ReadLine());
        Console.Write("Input the operator: ");
        var op = Console.ReadLine();
        switch (op)
        {
            case "+":
                Console.WriteLine(firstNumber + secondNumber);
                break;
            case "-":
                Console.WriteLine(firstNumber - secondNumber);
                break;
            case "*":
                Console.WriteLine(firstNumber * secondNumber);
                break;
            case "/":
                Console.WriteLine(firstNumber / secondNumber);
                break;
            default:
                Console.WriteLine("Invalid operator");
                break;
        }
    }
}
