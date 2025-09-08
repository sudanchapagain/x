<?php
if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $num1 = $_POST["num1"];
    $num2 = $_POST["num2"];

    $bigger_num = max($num1, $num2);

    echo "<h3>Multiplication Table of $bigger_num</h3>";
    for ($i = 1; $i <= 10; $i++) {
        echo "$bigger_num x $i = " . $bigger_num * $i . "<br>";
    }

    $operator ='+';
    switch ($operator) {
        case "+":
            $result = $num1 + $num2;
            break;
        case "-":
            $result = $num1 - $num2;
            break;
        case "*":
            $result = $num1 * $num2;
            break;
        case "/":
            $result = $num2 != 0 ? $num1 / $num2 : "Error: Division by zero!";
            break;
        case "%":
            $result = $num1 % $num2;
            break;
        default:
            $result = "Invalid operator";
    }
    echo "<h3>Result: $result</h3>";
}

$indexed_array = [10, 20, 30, 40, 50];
$associative_array = ["name" => "Sudan", "age" => 21, "city" => "Kathmandu"];
$multi_array = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];

echo "<h3>indexed array</h3>";
var_dump($indexed_array); echo "<br>";
print_r($indexed_array); echo "<br><br>";

echo "<h3>associative array</h3>";
var_dump($associative_array); echo "<br>";
print_r($associative_array); echo "<br><br>";

echo "<h3>multidimensional array</h3>";
var_dump($multi_array); echo "<br>";
print_r($multi_array);

$person = ["name" => "Sudan", "age" => 21, "city" => "Kathmandu"];

echo "<h3>using for loop</h3>";
$keys = array_keys($person);
for ($i = 0; $i < count($person); $i++) {
    echo $keys[$i] . ": " . $person[$keys[$i]] . "<br>";
}

echo "<h3>using foreach loop</h3>";
foreach ($person as $key => $value) {
    echo $key . ": " . $value . "<br>";
}

$array = [5, 10, 15, 20];
array_push($array, 25, 30);
array_shift($array);
$found = array_search(15, $array);
echo "Found 15 at index: $found<br>";
array_unshift($array, 0);
sort($array);
echo "Sorted Array: ";
print_r($array);
echo "<br>";
$assoc = ["b" => 3, "a" => 2, "c" => 1];
asort($assoc);
echo "sorted associative array (by value): ";
print_r($assoc);
echo "<br>";
ksort($assoc);
echo "sorted associative array (by key): ";
print_r($assoc);
echo "<br>";
rsort($array);
echo "reverse sorted array: ";
print_r($array);
echo "<br>";
arsort($assoc);
echo "associative array sorted by value in reverse: ";
print_r($assoc);
echo "<br>";
krsort($assoc);
echo "associative array sorted by key in reverse: ";
print_r($assoc);
echo "<br>";
echo "current key-value pair: ";
print_r(each($array));
echo "<br>";
echo "current element: " . current($array) . "<br>";
reset($array);
echo "first element: " . current($array) . "<br>";
end($array);
echo "last element: " . current($array) . "<br>";
next($array);
echo "next element: " . current($array) . "<br>";
echo "pos: " . pos($array) . "<br>";
prev($array);
echo "previous element: " . current($array) . "<br>";
shuffle($array);
echo "shuffled array: ";
print_r($array);
echo "<br>";
$reversed = array_reverse($array);
echo "reversed array: ";
print_r($reversed);

$nameErr = $usernameErr = $ageErr = $passwordErr = $confirmPasswordErr = "";
$name = $username = $age = $password = $confirmPassword = "";

if ($_SERVER["REQUEST_METHOD"] == "POST") {
    if (empty($_POST["name"])) {
        $nameErr = "Name is required.";
    } else {
        $name = $_POST["name"];
        if (!preg_match("/^[a-zA-Z\s]+$/", $name)) {
            $nameErr = "Name can only contain letters and white spaces.";
        }
    }

    if (empty($_POST["username"])) {
        $usernameErr = "Username is required.";
    } else {
        $username = $_POST["username"];
        if (strlen($username) < 8) {
            $usernameErr = "Username must be at least 8 characters.";
        } elseif (!preg_match("/^[a-zA-Z]/", $username)) {
            $usernameErr = "Username must begin with a letter.";
        }
    }

    if (empty($_POST["age"])) {
        $ageErr = "Age is required.";
    } else {
        $age = $_POST["age"];
        if ($age < 16) {
            $ageErr = "Age must be 16 or older.";
        }
    }

    if (empty($_POST["password"])) {
        $passwordErr = "Password is required.";
    } else {
        $password = $_POST["password"];
        if (strlen($password) < 8) {
            $passwordErr = "Password must be at least 8 characters.";
        }
    }

    if (empty($_POST["confirmPassword"])) {
        $confirmPasswordErr = "Confirm password is required.";
    } else {
        $confirmPassword = $_POST["confirmPassword"];
        if ($password !== $confirmPassword) {
            $confirmPasswordErr = "Passwords do not match.";
        }
    }

    if (
        empty($nameErr) &&
        empty($usernameErr) &&
        empty($ageErr) &&
        empty($passwordErr) &&
        empty($confirmPasswordErr)
    ) {
        echo "<h2>Form submitted successfully!</h2>";
        echo "Name: $name<br>";
        echo "Username: $username<br>";
        echo "Age: $age<br>";
        echo "Password: $password<br>";
    }
}

$pattern = "/\d+/";
$string = "There are 123 apples and 456 oranges.";
if (preg_match($pattern, $string, $matches)) {
    echo "First match found: " . $matches[0] . "<br>";
} else {
    echo "No match found.<br>";
}

$pattern = "/\d+/";
$string = "123 456 789";
if (preg_match_all($pattern, $string, $matches_all)) {
    echo "All matches found: ";
    print_r($matches_all[0]);
    echo "<br>";
} else {
    echo "No matches found.<br>";
}

$pattern = "/\d+/";
$string = "The price is 100 dollars and 50 cents.";
$replacement = "[number]";
$result = preg_replace($pattern, $replacement, $string);
echo "String after replacement: $result<br>";


function pass_by_value($num) {
    $num = $num + 10;
    echo "Inside function (pass by value): $num<br>";
}

function pass_by_reference(&$num) {
    $num = $num + 10;
    echo "Inside function (pass by reference): $num<br>";
}

$value = 5;
echo "Original value before pass by value: $value<br>";
pass_by_value($value);
echo "Original value after pass by value: $value<br>";

$reference = 5;
echo "Original value before pass by reference: $reference<br>";
pass_by_reference($reference);
echo "Original value after pass by reference: $reference<br>";

function greet($name = "Guest") {
    return "Hello, $name!";
}

echo greet();
echo "<br>";
echo greet("Sudan");

function sum_second_largest_smallest($arr) {
    sort($arr);
    $smallest = $arr[0];
    $second_largest = $arr[count($arr) - 2];
    return $smallest + $second_largest;
}

$numbers = [4, 7, 2, 9, 5];
echo "Array: ";
print_r($numbers);
echo "<br>";
echo "Sum of second largest and smallest: " . sum_second_largest_smallest($numbers);

function check_age($age) {
    if ($age < 18) {
        return "You are restricted.";
    } else {
        return "Welcome.";
    }
}

$age = 16;
echo "Age: $age<br>";
echo check_age($age);

$age = 20;
echo "<br>Age: $age<br>";
echo check_age($age);

if (isset($_GET["logout"])) {
    session_unset();
    session_destroy();
    setcookie("username", "", time() - 3600, "/");
    header("Location: ?");
    exit();
}

if (isset($_SESSION["username"]) || isset($_COOKIE["username"])) {
    $_SESSION["username"] = $_SESSION["username"] ?? $_COOKIE["username"];
    echo "Welcome, " . htmlspecialchars($_SESSION["username"]) . "!";
    echo '<br><a href="?logout=true">Logout</a>';
    exit();
}

if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $username = $_POST["username"];
    $password = $_POST["password"];
    $remember = isset($_POST["remember"]);

    require "db_connection.php";
    $stmt = $conn->prepare(
        "SELECT * FROM users WHERE username = ? AND password = ?"
    );
    $stmt->bind_param("ss", $username, $password);
    $stmt->execute();
    $result = $stmt->get_result();
    if ($result->num_rows > 0) {
        $_SESSION["username"] = $username;
        if ($remember) {
            setcookie("username", $username, time() + 86400 * 30, "/");
        }
        header("Location: ?");
        exit();
    } else {
        $error = "Invalid username or password!";
    }
    $stmt->close();
    $conn->close();
}

$timestamp = mktime(14, 30, 0, 5, 12, 2025);
echo "Timestamp for 2:30 PM on May 12, 2025: " . $timestamp . "<br>";
echo "Formatted Date: " . date("l, F j, Y, g:i a", $timestamp) . "<br>";

$timestamp_str = strtotime("next Monday");
echo "timestamp for next monday: " . $timestamp_str . "<br>";
echo "formatted Date: " . date("l, F j, Y, g:i a", $timestamp_str) . "<br>";

echo "current date: " . date("Y-m-d") . "<br>";
echo "current time: " . date("H:i:s") . "<br>";
echo "Full date and rime: " . date("l, F j, Y, g:i a") . "<br>";
echo "Day of the week: " . date("l") . "<br>";
echo "Month: " . date("F") . "<br>";
echo "day of the Month: " . date("d") . "<br>";
echo "current timestamp: " . date("U") . "<br>";

echo "current unix timestamp: " . time() . "<br>";

$to = "recipient@example.com";
$subject = "test";
$message = "woah php can do this.";
$headers = "From: sender@php.com";

if (mail($to, $subject, $message, $headers)) {
    echo "email sennt!";
} else {
    echo "failed.";
}

class Student {
    private $name;
    private $age;
    private $grade;

    public function __construct($name, $age, $grade) {
        $this->name = $name;
        $this->age = $age;
        $this->grade = $grade;
    }

    public function displayValues() {
        echo "name: " . $this->name . "<br>";
        echo "age: " . $this->age . "<br>";
        echo "grade: " . $this->grade . "<br>";
    }

    public function updateName($name) {
        $this->name = $name;
    }

    public function updateAge($age) {
        $this->age = $age;
    }

    public function updateGrade($grade) {
        $this->grade = $grade;
    }
}

class Employee {
    private $name;
    private $salary;

    public function __construct($name, $salary) {
        $this->name = $name;
        $this->salary = $salary;
    }

    public function getName() {
        return $this->name;
    }

    public function getSalary() {
        return $this->salary;
    }

    public function setSalary($salary) {
        $this->salary = $salary;
    }
}

class Animal {
    public function sound() {
        echo "animal makes a sound.<br>";
    }
}

class Dog extends Animal {
    public function sound() {
        echo "dog barks.<br>";
    }
}

class Bird extends Animal {
    public function sound() {
        echo "bird chirps.<br>";
    }
}

class Parrot extends Bird {
    public function sound() {
        echo "parrot squawks.<br>";
    }
}

class Cat extends Animal {
    public function sound() {
        echo "cat meows.<br>";
    }
}

class Person {
    public function __construct($name) {
        echo "name: " . $name . "<br>";
    }
}

class EmployeePerson extends Person {
    public function __construct($name, $salary) {
        parent::__construct($name);
        echo "salary: " . $salary . "<br>";
    }
}

class Counter {
    public static $count = 0;

    public static function increment() {
        self::$count++;
    }

    public static function getCount() {
        return self::$count;
    }
}

class ParentClass {
    public function greet() {
        echo "ello from parent class!<br>";
    }
}

class ChildClass extends ParentClass {
    public function greet() {
        echo "ello from child class!<br>";
    }
}

abstract class AnimalAbs {
    abstract public function sound();
}

class DogAbs extends AnimalAbs {
    public function sound() {
        echo "dog barks.<br>";
    }
}

interface AnimalInterface {
    public function sound();
}

class DogInterface implements AnimalInterface {
    public function sound() {
        echo "dog barks.<br>";
    }
}

class AnimalPolymorphism {
    public function sound() {
        echo "animal makes a sound.<br>";
    }
}

class DogPolymorphism extends AnimalPolymorphism {
    public function sound() {
        echo "dog barks.<br>";
    }
}

class CatPolymorphism extends AnimalPolymorphism {
    public function sound() {
        echo "cat meows.<br>";
    }
}

interface AnimalMultiple {
    public function sound();
}

interface BirdMultiple {
    public function fly();
}

class Bat implements AnimalMultiple, BirdMultiple {
    public function sound() {
        echo "bat makes a sound.<br>";
    }

    public function fly() {
        echo "bat flies.<br>";
    }
}

echo "<p>class with member vars, constructor, and methods</p>";
$student1 = new Student("me", 18, "A");
$student1->displayValues();
$student1->updateName("Johnny");
$student1->displayValues();

echo "<p>encapsulation in PHP</p>";
$emp = new Employee("me", 50000);
echo "employee Name: " . $emp->getName() . "<br>";
echo "employee Salary: " . $emp->getSalary() . "<br>";
$emp->setSalary(55000);
echo "updated Salary: " . $emp->getSalary() . "<br>";

echo "<p>inheritance (single, multi-level, and hierarchical)</p>";
$dog = new Dog();
$dog->sound();
$parrot = new Parrot();
$parrot->sound();
$cat = new Cat();
$cat->sound();

echo "<p>calling parent constructor from child class</p>";
$empPerson = new EmployeePerson("John", 50000);

echo "<p>static variables and methods</p>";
Counter::increment();
Counter::increment();
echo "Counter Value: " . Counter::getCount() . "<br>";

echo "<p>method overriding in PHP</p>";
$childClass = new ChildClass();
$childClass->greet();

echo "<p>abstract class in PHP</p>";
$dogAbs = new DogAbs();
$dogAbs->sound();

echo "<p>interface in PHP</p>";
$dogInterface = new DogInterface();
$dogInterface->sound();

echo "<p>dynamic polymorphism in PHP</p>";
$animalPoly = new AnimalPolymorphism();
$animalPoly->sound();
$dogPoly = new DogPolymorphism();
$dogPoly->sound();
$catPoly = new CatPolymorphism();
$catPoly->sound();

echo "<p>multiple inheritance with interface</p>";
$bat = new Bat();
$bat->sound();
$bat->fly();
?>
