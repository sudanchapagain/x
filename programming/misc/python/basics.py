# variables and printing
full_name = "Sudan Chapagain"
print(f"Hello {full_name}")
print(type(full_name))

age = 10
age = int(age)
name = input("Enter your name: ")

num1 = float(input("enter num 1"))
num2 = float(input("enter num 2"))

result = 0.1 + 0.2  # 0.30000000000000004

complex_num = complex(1, 4)
comp_num = 2 + 3j

calculationResult = num1 + num2
print(calculationResult)

calculationResult = num1 - num2
print(calculationResult)

calculationResult = num1 % num2
print(f"{calculationResult:,}")

CalculationResult = num1 / num2
print(f"{calculationResult:.2f}")

a = 1
b = 2
print(a + b)
print(a - b)
print(a * b)
print(a / b)
print(a // b)
print(a % b)
print(a**b)

strings_are_cool_specially_with_type_hints: str = "a"
long_strings_are_nice_too = """start from here
end on new line"""
repeater = "repeat" * 3

"""
Multiline strings can be written
using three "s, and are often used 
as documentation.
"""

wait_there_is_no_value = None

# conditional branching
if age >= 10:
    print(age)
elif age == 0:
    print(age)
else:
    print(age)

# loops
while age > 5:
    print(age)
    age -= 1

for i in range(5, 11):  # from 5 to 11
    print(i)

print(3 + 2)  # addition(+)
print(3 - 2)  # subtraction(-)
print(3 * 2)  # multiplication(*)
print(3 / 2)  # division(/)
print(3**2)  # exponential(**)
print(3 % 2)  # modulus(%)
print(3 // 2)  # Floor division operator(//)

print(type(10))  # Int
print(type(3.14))  # Float
print(type(1 + 3j))  # Complex
print(type("Sudan"))  # String
print(type([1, 2, 3]))  # List
print(type({"name": "Sudan"}))  # Dictionary
print(type({9.8, 3.14, 2.7}))  # Set
print(type((9.8, 3.14, 2.7)))  # Tuple
print(type(3 == 3))  # Bool
print(type(3 >= 3))  # Bool

first_name = "Sudan"
last_name = "Chapagain"
country = "Nepal"
city = "Kathmandui"
age = 21
is_married = False
skills = ["HTML", "CSS", "JS", "Nix", "Kotlin"]
person_info = {
    "firstname": "Sudan",
    "lastname": "Chapagain",
    "country": "Nepal",
    "city": "Kathmandui",
}

print("First name:", first_name)
print("First name length:", len(first_name))
print("Last name: ", last_name)
print("Last name length: ", len(last_name))
print("Country: ", country)
print("City: ", city)
print("Age: ", age)
print("Married: ", is_married)
print("Skills: ", skills)
print("Person information: ", person_info)

first_name, last_name, country, age, is_married = (
    "Sudan",
    "Chapagain",
    "Kathmandu",
    21,
    False,
)

print(first_name, last_name, country, age, is_married)
print("First name:", first_name)
print("Last name: ", last_name)
print("Country: ", country)
print("Age: ", age)
print("Married: ", is_married)

print("Addition: ", 1 + 2)
print("Subtraction: ", 2 - 1)
print("Multiplication: ", 2 * 3)
print("Division: ", 4 / 2)  # Division in Kotlin gives floating number
print("Division: ", 6 / 2)
print("Division: ", 7 / 2)
print(
    "Division without the remainder: ", 7 // 2
)  # gives without the floating number or without the remaining
print("Modulus: ", 3 % 2)  # Gives the remainder
print("Division without the remainder: ", 7 // 3)
print("Exponential: ", 3**2)  # it means 3 * 3

# Floating numbers
print("Floating Number,PI", 3.14)
print("Floating Number, gravity", 9.81)

# Complex numbers
print("Complex number: ", 1 + 1j)
print("Multiplying complex number: ", (1 + 1j) * (1 - 1j))

# Declaring the variable at the top first

a = 3  # a is a variable name and 3 is an integer data type
b = 2  # b is a variable name and 3 is an integer data type

# Arithmetic operations and assigning the result to a variable
total = a + b
diff = a - b
product = a * b
division = a / b
remainder = a % b
floor_division = a // b
exponential = a**b

# I should have used sum instead of total but sum is a built-in function try to avoid overriding builtin functions
print(
    total
)  # if you don't label your print with some string, you never know from where is  the result is coming
print("a + b = ", total)
print("a - b = ", diff)
print("a * b = ", product)
print("a / b = ", division)
print("a % b = ", remainder)
print("a // b = ", floor_division)
print("a ** b = ", exponential)

# Declaring values and organizing them together
num_one = 3
num_two = 4

# Arithmetic operations
total = num_one + num_two
diff = num_two - num_one
product = num_one * num_two
div = num_two / num_two
remainder = num_two % num_one

# Printing values with label
print("total: ", total)
print("difference: ", diff)
print("product: ", product)
print("division: ", div)
print("remainder: ", remainder)


# Calculating area of a circle
radius = 10  # radius of a circle
area_of_circle = 3.14 * radius**2  # two * sign means exponent or power
print("Area of a circle:", area_of_circle)

# Calculating area of a rectangle
length = 10
width = 20
area_of_rectangle = length * width
print("Area of rectangle:", area_of_rectangle)

# Calculating a weight of an object
mass = 75
gravity = 9.81
weight = mass * gravity
print(weight, "N")

print(3 > 2)  # True, because 3 is greater than 2
print(3 >= 2)  # True, because 3 is greater than 2
print(3 < 2)  # False,  because 3 is greater than 2
print(2 < 3)  # True, because 2 is less than 3
print(2 <= 3)  # True, because 2 is less than 3
print(3 == 2)  # False, because 3 is not equal to 2
print(3 != 2)  # True, because 3 is not equal to 2
print(len("mango") == len("avocado"))  # False
print(len("mango") != len("avocado"))  # True
print(len("mango") < len("avocado"))  # True
print(len("milk") != len("meat"))  # False
print(len("milk") == len("meat"))  # True
print(len("tomato") == len("potato"))  # True
print(len("Kotlin") > len("dragon"))  # False

# Boolean comparison
print("True == True: ", True == True)
print("True == False: ", True == False)
print("False == False:", False == False)
print("True and True: ", True and True)
print("True or False:", True or False)

# Another way comparison
print("1 is 1", 1 is 1)  # True - because the data values are the same
print("1 is not 2", 1 is not 2)  # True - because 1 is not 2
print("A in Sudan", "A" in "Sudan")  # True - A found in the string
print("B in Sudan", "B" in "Sudan")  # False -there is no uppercase B
print("coding" in "coding for all")  # True - because coding for all has the word coding
print("a in an:", "a" in "an")  # True
print("4 is 2 ** 2:", 4 is 2**2)  # True

print(3 > 2 and 4 > 3)  # True - because both statements are true
print(3 > 2 and 4 < 3)  # False - because the second statement is false
print(3 < 2 and 4 < 3)  # False - because both statements are false
print(3 > 2 or 4 > 3)  # True - because both statements are true
print(3 > 2 or 4 < 3)  # True - because one of the statement is true
print(3 < 2 or 4 < 3)  # False - because both statements are false
print(not 3 > 2)  # False - because 3 > 2 is true, then not True gives False
print(not True)  # False - Negation, the not operator turns true to false
print(not False)  # True
print(not not True)  # True
print(not not False)  # False

# Single line comment
letter = "P"  # A string could be a single character or a bunch of texts
print(letter)  # P
print(len(letter))  # 1
greeting = "Hello, World!"  # String could be  a single or double quote,"Hello, World!"
print(greeting)  # Hello, World!
print(len(greeting))  # 13
sentence = "this is a short and sweet sentence."
print(sentence)

# Multiline String
multiline_string = """look
at
this
thing
woah
"""
print(multiline_string)
# Another way of doing the same thing
multiline_string = """hehe
haha
what"""
print(multiline_string)
