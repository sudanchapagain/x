namespace Lab01;

public static class Lab6
{
    public static void Run()
    {
        Console.Write("Enter the first number: ");
        var num1 = int.Parse(Console.ReadLine());

        Console.Write("Enter the second number: ");
        var num2 = int.Parse(Console.ReadLine());

        var andResult = num1 & num2;
        Console.WriteLine($"Bitwise AND: {num1} & {num2} = {andResult}");

        var orResult = num1 | num2;
        Console.WriteLine($"Bitwise OR: {num1} | {num2} = {orResult}");

        var xorResult = num1 ^ num2;
        Console.WriteLine($"Bitwise XOR: {num1} ^ {num2} = {xorResult}");

        var notResultNum1 = ~num1;
        Console.WriteLine($"Bitwise NOT on num1 (~{num1}) = {notResultNum1}");

        var notResultNum2 = ~num2;
        Console.WriteLine($"Bitwise NOT on num2 (~{num2}) = {notResultNum2}");

        var leftShiftNum1 = num1 << 1;
        Console.WriteLine($"Left Shift on num1 ({num1} << 1) = {leftShiftNum1}");

        var rightShiftNum1 = num1 >> 1;
        Console.WriteLine($"Right Shift on num1 ({num1} >> 1) = {rightShiftNum1}");
    }
}
