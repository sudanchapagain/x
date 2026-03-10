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

# String Concatenation
first_name = "Sudan"
last_name = "Chapagain"
space = " "
full_name = first_name + space + last_name
print(full_name)  # Sudan Chapagain
# Checking length of a string using len() builtin function
print(len(first_name))  # 8
print(len(last_name))  # 7
print(len(first_name) > len(last_name))  # True
print(len(full_name))  # 15

#### Unpacking characters
language = "Kotlin"
a, b, c, d, e, f = language  # unpacking sequence characters into variables
print(a)  # P
print(b)  # y
print(c)  # t
print(d)  # h
print(e)  # o
print(f)  # n

# Accessing characters in strings by index
language = "Kotlin"
first_letter = language[0]
print(first_letter)  # P
second_letter = language[1]
print(second_letter)  # y
last_index = len(language) - 1
last_letter = language[last_index]
print(last_letter)  # n

# If we want to start from right end we can use negative indexing. -1 is the last index
language = "Kotlin"
last_letter = language[-1]
print(last_letter)  # n
second_last = language[-2]
print(second_last)  # o

# Slicing

language = "Kotlin"
first_three = language[0:3]  # starts at zero index and up to 3 but not include 3
last_three = language[3:6]
print(last_three)  # hon
# Another way
last_three = language[-3:]
print(last_three)  # hon
last_three = language[3:]
print(last_three)  # hon

# Skipping character while splitting Kotlin strings
language = "Kotlin"
pto = language[0:6:2]  #
print(pto)  # pto

# Escape sequence
print("I hope every one enjoying the Kotlin challenge.\nDo you ?")  # line break
print("Days\tTopics\tExercises")
print("Day 1\t3\t5")
print("Day 2\t3\t5")
print("Day 3\t3\t5")
print("Day 4\t3\t5")
print("This is a back slash  symbol (\\)")  # To write a back slash
print('In every programming language it starts with "Hello, World!"')

## String Methods
# capitalize(): Converts the first character the string to Capital Letter

challenge = "thirty days of Kotlin"
print(challenge.capitalize())  # 'Thirty days of Kotlin'

# count(): returns occurrences of substring in string, count(substring, start=.., end=..)

challenge = "thirty days of Kotlin"
print(challenge.count("y"))  # 3
print(challenge.count("y", 7, 14))  # 1
print(challenge.count("th"))  # 2`

# endswith(): Checks if a string ends with a specified ending

challenge = "thirty days of Kotlin"
print(challenge.endswith("on"))  # True
print(challenge.endswith("tion"))  # False

# expandtabs(): Replaces tab character with spaces, default tab size is 8. It takes tab size argument

challenge = "thirty\tdays\tof\tKotlin"
print(challenge.expandtabs())  # 'thirty  days    of      Kotlin'
print(challenge.expandtabs(10))  # 'thirty    days      of        Kotlin'

# find(): Returns the index of first occurrence of substring

challenge = "thirty days of Kotlin"
print(challenge.find("y"))  # 5
print(challenge.find("th"))  # 0

# format()	formats string into nicer output
first_name = "Sudan"
last_name = "Chapagain"
job = "learner"
country = "Nepal"
sentence = "I am {} {}. I am a {}. I live in {}.".format(
    first_name, last_name, job, country
)
print(sentence)  # I am Sudan Chapagain. I am a learner. I live in Nepal.

radius = 10
pi = 3.14
area = pi  # radius ## 2
result = "The area of circle with {} is {}".format(str(radius), str(area))
print(result)  # The area of circle with 10 is 314.0

# index(): Returns the index of substring
challenge = "thirty days of Kotlin"
print(challenge.find("y"))  # 5
print(challenge.find("th"))  # 0

# isalnum(): Checks alphanumeric character

challenge = "ThirtyDaysKotlin"
print(challenge.isalnum())  # True

challenge = "30DaysKotlin"
print(challenge.isalnum())  # True

challenge = "thirty days of Kotlin"
print(challenge.isalnum())  # False

challenge = "thirty days of Kotlin 2019"
print(challenge.isalnum())  # False

# isalpha(): Checks if all characters are alphabets

challenge = "thirty days of Kotlin"
print(challenge.isalpha())  # True
num = "123"
print(num.isalpha())  # False

# isdecimal(): Checks Decimal Characters

challenge = "thirty days of Kotlin"
print(challenge.find("y"))  # 5
print(challenge.find("th"))  # 0

# isdigit(): Checks Digit Characters

challenge = "Thirty"
print(challenge.isdigit())  # False
challenge = "30"
print(challenge.digit())  # True

# isdecimal():Checks decimal characters

num = "10"
print(num.isdecimal())  # True
num = "10.5"
print(num.isdecimal())  # False


# isidentifier():Checks for valid identifier means it check if a string is a valid variable name

challenge = "30DaysOfKotlin"
print(challenge.isidentifier())  # False, because it starts with a number
challenge = "thirty_days_of_Kotlin"
print(challenge.isidentifier())  # True


# islower():Checks if all alphabets in a string are lowercase

challenge = "thirty days of Kotlin"
print(challenge.islower())  # True
challenge = "Thirty days of Kotlin"
print(challenge.islower())  # False

# isupper(): returns if all characters are uppercase characters

challenge = "thirty days of Kotlin"
print(challenge.isupper())  #  False
challenge = "THIRTY DAYS OF Kotlin"
print(challenge.isupper())  # True


# isnumeric():Checks numeric characters

num = "10"
print(num.isnumeric())  # True
print("ten".isnumeric())  # False

# join(): Returns a concatenated string

web_tech = ["HTML", "CSS", "JavaScript", "Nix"]
result = "#, ".join(web_tech)
print(result)  # 'HTML# CSS# JavaScript# Nix'

# strip(): Removes both leading and trailing characters

challenge = " thirty days of Kotlin "
print(challenge.strip("y"))  # 5

# replace(): Replaces substring inside

challenge = "thirty days of Kotlin"
print(challenge.replace("Kotlin", "coding"))  # 'thirty days of coding'

# split():Splits String from Left

challenge = "thirty days of Kotlin"
print(challenge.split())  # ['thirty', 'days', 'of', 'Kotlin']

# title(): Returns a Title Cased String

challenge = "thirty days of Kotlin"
print(challenge.title())  # Thirty Days Of Kotlin

# swapcase(): Checks if String Starts with the Specified String

challenge = "thirty days of Kotlin"
print(challenge.swapcase())  # THIRTY DAYS OF Kotlin
challenge = "Thirty Days Of Kotlin"
print(challenge.swapcase())  # tHIRTY dAYS oF Kotlin

# startswith(): Checks if String Starts with the Specified String

challenge = "thirty days of Kotlin"
print(challenge.startswith("thirty"))  # True
challenge = "30 days of Kotlin"
print(challenge.startswith("thirty"))  # False

empty_list = list()  # this is an empty list, no item in the list
print(len(empty_list))  # 0

fruits = ["passion fruit", "orange", "mango", "lemon"]  # list of fruits
vegetables = ["Tomato", "Potato", "Cabbage", "Onion", "Carrot"]  # list of vegetables
animal_products = ["milk", "meat", "butter", "yoghurt"]  # list of animal products
web_techs = [
    "HTML",
    "CSS",
    "JS",
    "Nix",
    "Redux",
    "Node",
    "MongDB",
]  # list of web technologies
countries = ["Nepal", "Estonia", "Sri Lanka", "Kenya", "Ethiopia"]

# Print the lists and it length
print("Fruits:", fruits)
print("Number of fruits:", len(fruits))
print("Vegetables:", vegetables)
print("Number of vegetables:", len(vegetables))
print("Animal products:", animal_products)
print("Number of animal products:", len(animal_products))
print("Web technologies:", web_techs)
print("Number of web technologies:", len(web_techs))
print("Number of countries:", len(countries))

# Modifying list

fruits = ["passion fruit", "orange", "mango", "lemon"]
first_fruit = fruits[0]  # we are accessing the first item using its index
print(first_fruit)  # passion fruit
second_fruit = fruits[1]
print(second_fruit)  # orange
last_fruit = fruits[3]
print(last_fruit)  # lemon
# Last index
last_index = len(fruits) - 1
last_fruit = fruits[last_index]

# Accessing items
fruits = ["passion fruit", "orange", "mango", "lemon"]
last_fruit = fruits[-1]
second_last = fruits[-2]
print(last_fruit)  # lemon
print(second_last)  # mango

# Slicing items
fruits = ["passion fruit", "orange", "mango", "lemon"]
all_fruits = fruits[0:4]  # it returns all the fruits
# this is also give the same result as the above
all_fruits = fruits[0:]  # if we don't set where to stop it takes all the rest
orange_and_mango = fruits[1:3]  # it does not include the end index
orange_mango_lemon = fruits[1:]

fruits = ["passion fruit", "orange", "mango", "lemon"]
all_fruits = fruits[-4:]  # it returns all the fruits
# this is also give the same result as the above
orange_and_mango = fruits[-3:-1]  # it does not include the end index
orange_mango_lemon = fruits[-3:]


fruits = ["passion fruit", "orange", "mango", "lemon"]
fruits[0] = "Avocado"
print(fruits)  #  ['avocado', 'orange', 'mango', 'lemon']
fruits[1] = "apple"
print(fruits)  #  ['avocado', 'apple', 'mango', 'lemon']
last_index = len(fruits) - 1
fruits[last_index] = "lime"
print(fruits)  #  ['avocado', 'apple', 'mango', 'lime']

# checking items
fruits = ["passion fruit", "orange", "mango", "lemon"]
does_exist = "passion fruit" in fruits
print(does_exist)  # True
does_exist = "lime" in fruits
print(does_exist)  # False

# Append
fruits = ["passion fruit", "orange", "mango", "lemon"]
fruits.append("apple")
print(fruits)  # ['passion fruit', 'orange', 'mango', 'lemon', 'apple']
fruits.append("lime")  # ['passion fruit', 'orange', 'mango', 'lemon', 'apple', 'lime]
print(fruits)

# insert
fruits = ["passion fruit", "orange", "mango", "lemon"]
fruits.insert(2, "apple")  # insert apple between orange and mango
print(fruits)  # ['passion fruit', 'orange', 'apple', 'mango', 'lemon']
fruits.insert(
    3, "lime"
)  # ['passion fruit', 'orange', 'apple', 'mango', 'lime','lemon',]
print(fruits)

# remove
fruits = ["passion fruit", "orange", "mango", "lemon"]
fruits.remove("passion fruit")
print(fruits)  # ['orange', 'mango', 'lemon']
fruits.remove("lemon")
print(fruits)  # ['orange', 'mango']

# pop
fruits = ["passion fruit", "orange", "mango", "lemon"]
fruits.pop()
print(fruits)  # ['passion fruit', 'orange', 'mango']

fruits.pop(0)
print(fruits)  # ['orange', 'mango']

# del
fruits = ["passion fruit", "orange", "mango", "lemon"]
del fruits[0]
print(fruits)  # ['orange', 'mango', 'lemon']

del fruits[1]
print(fruits)  # ['orange', 'lemon']
del fruits
print(fruits)  # This should give: NameError: name 'fruits' is not defined

# clear
fruits = ["passion fruit", "orange", "mango", "lemon"]
fruits.clear()
print(fruits)  # []

# copying a lits

fruits = ["passion fruit", "orange", "mango", "lemon"]
fruits_copy = fruits.copy()
print(fruits_copy)  # ['passion fruit', 'orange', 'mango', 'lemon']

# join
positive_numbers = [1, 2, 3, 4, 5]
zero = [0]
negative_numbers = [-5, -4, -3, -2, -1]
integers = negative_numbers + zero + positive_numbers
print(integers)
fruits = ["passion fruit", "orange", "mango", "lemon"]
vegetables = ["Tomato", "Potato", "Cabbage", "Onion", "Carrot"]
fruits_and_vegetables = fruits + vegetables
print(fruits_and_vegetables)

# join with extend
num1 = [0, 1, 2, 3]
num2 = [4, 5, 6]
num1.extend(num2)
print("Numbers:", num1)
negative_numbers = [-5, -4, -3, -2, -1]
positive_numbers = [1, 2, 3, 4, 5]
zero = [0]

negative_numbers.extend(zero)
negative_numbers.extend(positive_numbers)
print("Integers:", negative_numbers)
fruits = ["passion fruit", "orange", "mango", "lemon"]
vegetables = ["Tomato", "Potato", "Cabbage", "Onion", "Carrot"]
fruits.extend(vegetables)
print("Fruits and vegetables:", fruits)

# count
fruits = ["passion fruit", "orange", "mango", "lemon"]
print(fruits.count("orange"))  # 1
ages = [22, 19, 24, 25, 26, 24, 25, 24]
print(ages.count(24))  # 3

# index
fruits = ["passion fruit", "orange", "mango", "lemon"]
print(fruits.index("orange"))  # 1
ages = [22, 19, 24, 25, 26, 24, 25, 24]
print(ages.index(24))
# Reverse
fruits = ["passion fruit", "orange", "mango", "lemon"]
fruits.reverse()
print(fruits)
ages = [22, 19, 24, 25, 26, 24, 25, 24]
ages.reverse()
print(ages)

# sort
fruits = ["passion fruit", "orange", "mango", "lemon"]
fruits.sort()
print(fruits)
fruits.sort(reverse=True)
print(fruits)
ages = [22, 19, 24, 25, 26, 24, 25, 24]
ages.sort()
print(ages)
ages.sort(reverse=True)
print(ages)

# A tuple is a collection of different data types which is ordered and unchangeable (immutable). Tuples are written with round brackets, (). Once a tuple is created, we cannot change its values. We cannot use add, insert, remove methods in a tuple because it is not modifiable (mutable). Unlike list, tuple has few methods. Methods related to tuples:

empty_tuple = ()
# or using the tuple constructor
empty_tuple = tuple()

tpl = ("item1", "item2", "item3")

fruits = ("passion fruit", "orange", "mango", "lemon")

tpl = ("item1", "item2", "item3")
len(tpl)

tpl = ("item1", "item2", "item3")
first_item = tpl[0]
second_item = tpl[1]

fruits = ("passion fruit", "orange", "mango", "lemon")
first_fruit = fruits[0]
second_fruit = fruits[1]
last_index = len(fruits) - 1
last_fruit = fruits[last_index]

tpl = ("item1", "item2", "item3", "item4")
first_item = tpl[-4]
second_item = tpl[-3]

fruits = ("passion fruit", "orange", "mango", "lemon")
first_fruit = fruits[-4]
second_fruit = fruits[-3]
last_fruit = fruits[-1]

tpl = ("item1", "item2", "item3", "item4")
all_items = tpl[0:4]  # all items
all_items = tpl[0:]  # all items
middle_two_items = tpl[1:3]  # does not include item at index 3

fruits = ("passion fruit", "orange", "mango", "lemon")
all_fruits = fruits[0:4]  # all items
all_fruits = fruits[0:]  # all items
orange_mango = fruits[1:3]  # doesn't include item at index 3
orange_to_the_rest = fruits[1:]

tpl = ("item1", "item2", "item3", "item4")
all_items = tpl[-4:]  # all items
middle_two_items = tpl[-3:-1]  # does not include item at index 3 (-1)

fruits = ("passion fruit", "orange", "mango", "lemon")
all_fruits = fruits[-4:]  # all items
orange_mango = fruits[-3:-1]  # doesn't include item at index 3
orange_to_the_rest = fruits[-3:]

tpl = ("item1", "item2", "item3", "item4")
lst = list(tpl)

fruits = ("passion fruit", "orange", "mango", "lemon")
fruits = list(fruits)
fruits[0] = "apple"
print(fruits)  # ['apple', 'orange', 'mango', 'lemon']
fruits = tuple(fruits)
print(fruits)  # ('apple', 'orange', 'mango', 'lemon')


tpl = ("item1", "item2", "item3", "item4")
"item2" in tpl  # True

fruits = ("passion fruit", "orange", "mango", "lemon")
print("orange" in fruits)  # True
print("apple" in fruits)  # False
fruits[0] = "apple"  # TypeError: 'tuple' object does not support item assignment

tpl1 = ("item1", "item2", "item3")
tpl2 = ("item4", "item5", "item6")
tpl3 = tpl1 + tpl2

fruits = ("passion fruit", "orange", "mango", "lemon")
vegetables = ("Tomato", "Potato", "Cabbage", "Onion", "Carrot")
fruits_and_vegetables = fruits + vegetables

tpl1 = ("item1", "item2", "item3")
del tpl1

fruits = ("passion fruit", "orange", "mango", "lemon")
del fruits

# Set is a collection of items. Let me take you back to your elementary or high school Mathematics lesson. The Mathematics definition of a set can be applied also in Kotlin. Set is a collection of unordered and un-indexed distinct elements. In Kotlin set is used to store unique items, and it is possible to find the _union_, _intersection_, _difference_, _symmetric difference_, _subset_, _super set_ and _disjoint set_ among sets.

st = set()

st = {"item1", "item2", "item3", "item4"}

fruits = {"passion fruit", "orange", "mango", "lemon"}

st = {"item1", "item2", "item3", "item4"}
len(st)

fruits = {"passion fruit", "orange", "mango", "lemon"}
len(fruits)

st = {"item1", "item2", "item3", "item4"}
print("Does set st contain item3? ", "item3" in st)  # Does set st contain item3? True

fruits = {"passion fruit", "orange", "mango", "lemon"}
print("mango" in fruits)  # True

st = {"item1", "item2", "item3", "item4"}
st.add("item5")

fruits = {"passion fruit", "orange", "mango", "lemon"}
fruits.add("lime")

st = {"item1", "item2", "item3", "item4"}
st.update(["item5", "item6", "item7"])

fruits = {"passion fruit", "orange", "mango", "lemon"}
vegetables = ("tomato", "potato", "cabbage", "onion", "carrot")
fruits.update(vegetables)

st = {"item1", "item2", "item3", "item4"}
st.remove("item2")

fruits = {"passion fruit", "orange", "mango", "lemon"}
fruits.pop()  # removes a random item from the set

fruits = {"passion fruit", "orange", "mango", "lemon"}
removed_item = fruits.pop()

st = {"item1", "item2", "item3", "item4"}
st.clear()

fruits = {"passion fruit", "orange", "mango", "lemon"}
fruits.clear()
print(fruits)  # set()

st = {"item1", "item2", "item3", "item4"}
del st

fruits = {"passion fruit", "orange", "mango", "lemon"}
del fruits

lst = ["item1", "item2", "item3", "item4", "item1"]
st = set(
    lst
)  # {'item2', 'item4', 'item1', 'item3'} - the order is random, because sets in general are unordered

fruits = ["passion fruit", "orange", "mango", "lemon", "orange", "passion fruit"]
fruits = set(fruits)  # {'mango', 'lemon', 'passion fruit', 'orange'}

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item5", "item6", "item7", "item8"}
st3 = st1.union(st2)

fruits = {"passion fruit", "orange", "mango", "lemon"}
vegetables = {"tomato", "potato", "cabbage", "onion", "carrot"}
print(
    fruits.union(vegetables)
)  # {'lemon', 'carrot', 'tomato', 'passion fruit', 'mango', 'orange', 'cabbage', 'potato', 'onion'}

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item5", "item6", "item7", "item8"}
st1.update(st2)  # st2 contents are added to st1

fruits = {"passion fruit", "orange", "mango", "lemon"}
vegetables = {"tomato", "potato", "cabbage", "onion", "carrot"}
fruits.update(vegetables)
print(
    fruits
)  # {'lemon', 'carrot', 'tomato', 'passion fruit', 'mango', 'orange', 'cabbage', 'potato', 'onion'}

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item3", "item2"}
st1.intersection(st2)  # {'item3', 'item2'}

whole_numbers = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
even_numbers = {0, 2, 4, 6, 8, 10}
whole_numbers.intersection(even_numbers)  # {0, 2, 4, 6, 8, 10}

Kotlin = {"p", "y", "t", "h", "o", "n"}
dragon = {"d", "r", "a", "g", "o", "n"}
Kotlin.intersection(dragon)  # {'o', 'n'}

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item2", "item3"}
st2.issubset(st1)  # True
st1.issuperset(st2)  # True

whole_numbers = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
even_numbers = {0, 2, 4, 6, 8, 10}
whole_numbers.issubset(even_numbers)  # False, because it is a super set
whole_numbers.issuperset(even_numbers)  # True

Kotlin = {"p", "y", "t", "h", "o", "n"}
dragon = {"d", "r", "a", "g", "o", "n"}
Kotlin.issubset(dragon)  # False

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item2", "item3"}
st2.difference(st1)  # set()
st1.difference(st2)  # {'item1', 'item4'} => st1\st2

whole_numbers = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
even_numbers = {0, 2, 4, 6, 8, 10}
whole_numbers.difference(even_numbers)  # {1, 3, 5, 7, 9}

Kotlin = {"p", "y", "t", "o", "n"}
dragon = {"d", "r", "a", "g", "o", "n"}
Kotlin.difference(
    dragon
)  # {'p', 'y', 't'}  - the result is unordered (characteristic of sets)
dragon.difference(Kotlin)  # {'d', 'r', 'a', 'g'}

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item2", "item3"}
# it means (A\B)∪(B\A)
st2.symmetric_difference(st1)  # {'item1', 'item4'}

whole_numbers = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
some_numbers = {1, 2, 3, 4, 5}
whole_numbers.symmetric_difference(some_numbers)  # {0, 6, 7, 8, 9, 10}

Kotlin = {"p", "y", "t", "h", "o", "n"}
dragon = {"d", "r", "a", "g", "o", "n"}
Kotlin.symmetric_difference(dragon)  # {'r', 't', 'p', 'y', 'g', 'a', 'd', 'h'}

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item2", "item3"}
st2.isdisjoint(st1)  # False

even_numbers = {0, 2, 4, 6, 8}
odd_numbers = {1, 3, 5, 7, 9}
even_numbers.isdisjoint(odd_numbers)  # True, because no common item

Kotlin = {"p", "y", "t", "h", "o", "n"}
dragon = {"d", "r", "a", "g", "o", "n"}
Kotlin.isdisjoint(dragon)  # False, there are common items {'o', 'n'}

empty_dict = {}
# Dictionary with data values
dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Node", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
print(len(dct))  # 4

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_married": True,
    "skills": ["JavaScript", "Nix", "Node", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}
print(len(person))  # 7

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
print(dct["key1"])  # value1
print(dct["key4"])  # value4

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Node", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}

print(person["first_name"])  # Sudan
print(person["country"])  # Nepal
print(person["skills"])  # ['JavaScript', 'Nix', 'Node', 'Postgres', 'Kotlin']
print(person["skills"][0])  # JavaScript
print(person["address"]["street"])  # Space street
print(person["city"])  # Error

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Node", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}
print(person.get("first_name"))  # Sudan
print(person.get("country"))  # Nepal
print(
    person.get("skills")
)  # ['HTML','CSS','JavaScript', 'Nix', 'Node', 'Postgres', 'Kotlin']
print(person.get("city"))  # None

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
dct["key5"] = "value5"

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Node", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}
person["job_title"] = "Instructor"
person["skills"].append("HTML")
print(person)

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
dct["key1"] = "value-one"

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Node", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}
person["first_name"] = "Eyob"
person["age"] = 252

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
print("key2" in dct)  # True
print("key5" in dct)  # False

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
dct.pop("key1")  # removes key1 item
dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
dct.popitem()  # removes the last item
del dct["key2"]  # removes key2 item

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Node", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}
person.pop("first_name")  # Removes the firstname item
person.popitem()  # Removes the address item
del person["is_married"]  # Removes the is_married item

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
print(
    dct.items()
)  # dict_items([('key1', 'value1'), ('key2', 'value2'), ('key3', 'value3'), ('key4', 'value4')])

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
print(dct.clear())  # None

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
del dct

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
dct_copy = (
    dct.copy()
)  # {'key1':'value1', 'key2':'value2', 'key3':'value3', 'key4':'value4'}

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
keys = dct.keys()
print(keys)  # dict_keys(['key1', 'key2', 'key3', 'key4'])

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
values = dct.values()
print(values)  # dict_values(['value1', 'value2', 'value3', 'value4'])

a = 3
if a > 0:
    print("A is a positive number")
# A is a positive number

a = 3
if a < 0:
    print("A is a negative number")
else:
    print("A is a positive number")

a = 0
if a > 0:
    print("A is a positive number")
elif a < 0:
    print("A is a negative number")
else:
    print("A is zero")

a = 3
print("A is positive") if a > 0 else print(
    "A is negative"
)  # first condition met, 'A is positive' will be printed

a = 0
if a > 0:
    if a % 2 == 0:
        print("A is a positive and even integer")
    else:
        print("A is a positive number")
elif a == 0:
    print("A is zero")
else:
    print("A is a negative number")


a = 0
if a > 0 and a % 2 == 0:
    print("A is an even and positive integer")
elif a > 0 and a % 2 != 0:
    print("A is a positive integer")
elif a == 0:
    print("A is zero")
else:
    print("A is negative")

user = "James"
access_level = 3
if user == "admin" or access_level >= 4:
    print("Access granted!")
else:
    print("Access denied!")

count = 0
while count < 5:
    print(count)
    count = count + 1
# prints from 0 to 4

count = 0
while count < 5:
    print(count)
    count = count + 1
else:
    print(count)

count = 0
while count < 5:
    print(count)
    count = count + 1
    if count == 3:
        break

count = 0
while count < 5:
    if count == 3:
        count = count + 1
        continue
    print(count)
    count = count + 1

numbers = [0, 1, 2, 3, 4, 5]
for number in (
    numbers
):  # number is temporary name to refer to the list's items, valid only inside this loop
    print(number)  # the numbers will be printed line by line, from 0 to 5

language = "Kotlin"
for letter in language:
    print(letter)


for i in range(len(language)):
    print(language[i])

numbers = (0, 1, 2, 3, 4, 5)
for number in numbers:
    print(number)

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Node", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}
for key in person:
    print(key)

for key, value in person.items():
    print(key, value)  # this way we get both keys and values printed out

it_companies = {"Facebook", "Google", "Microsoft", "Apple", "IBM", "Oracle", "Amazon"}
for company in it_companies:
    print(company)

numbers = (0, 1, 2, 3, 4, 5)
for number in numbers:
    print(number)
    if number == 3:
        break

numbers = (0, 1, 2, 3, 4, 5)
for number in numbers:
    print(number)
    if number == 3:
        continue
    print("Next number should be ", number + 1) if number != 5 else print(
        "loop's end"
    )  # for short hand conditions need both if and else statements
print("outside the loop")

lst = list(range(11))
print(lst)  # [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
st = set(
    range(1, 11)
)  # 2 arguments indicate start and end of the sequence, step set to default 1
print(st)  # {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

lst = list(range(0, 11, 2))
print(lst)  # [0, 2, 4, 6, 8, 10]
st = set(range(0, 11, 2))
print(st)  #  {0, 2, 4, 6, 8, 10}

for number in range(11):
    print(number)  # prints 0 to 10, not including 11

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Node", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}
for key in person:
    if key == "skills":
        for skill in person["skills"]:
            print(skill)

for number in range(11):
    print(number)  # prints 0 to 10, not including 11
else:
    print("The loop stops at", number)


def generate_full_name_0():
    first_name = "Sudan"
    last_name = "Chapagain"
    space = " "
    full_name = first_name + space + last_name
    print(full_name)


generate_full_name_0()  # calling a function


def add_two_numbers_1():
    num_one = 2
    num_two = 3
    total = num_one + num_two
    print(total)


add_two_numbers_1()


def generate_full_name_1():
    first_name = "Sudan"
    last_name = "Chapagain"
    space = " "
    full_name = first_name + space + last_name
    return full_name


print(generate_full_name_1())


def add_two_numbers_2():
    num_one = 2
    num_two = 3
    total = num_one + num_two
    return total


print(add_two_numbers_2())


def greetingss(name):
    message = name + ", welcome to Kotlin for Everyone!"
    return message


print(greetingss("Sudan"))


def add_ten(num):
    ten = 10
    return num + ten


print(add_ten(90))


def square_number(x):
    return x * x


print(square_number(2))


def area_of_circle(r):
    PI = 3.14
    area = PI * r**2
    return area


print(area_of_circle(10))


def sum_of_numbers(n):
    total = 0
    for i in range(n + 1):
        total += i
    print(total)


print(sum_of_numbers(10))  # 55
print(sum_of_numbers(100))  # 5050


def generate_full_name_3(first_name, last_name):
    space = " "
    full_name = first_name + space + last_name
    return full_name


print("Full Name: ", generate_full_name_3("Sudan", "Chapagain"))


def sum_two_numbers(num_one, num_two):
    sum = num_one + num_two
    return sum


print("Sum of two numbers: ", sum_two_numbers(1, 9))


def calculate_age(current_year, birth_year):
    age = current_year - birth_year
    return age


print("Age: ", calculate_age(2021, 1819))


def weight_of_object(mass, gravity):
    weight = str(mass * gravity) + " N"  # the value has to be changed to a string first
    return weight


print("Weight of an object in Newtons: ", weight_of_object(100, 9.81))


def print_fullname(firstname, lastname):
    space = " "
    full_name = firstname + space + lastname
    print(full_name)


print(print_fullname(firstname="Sudan", lastname="Chapagain"))


def add_two_numbers(num1, num2):
    total = num1 + num2
    print(total)


print(add_two_numbers(num2=3, num1=2))  # Order does not matter


def print_name(firstname):
    return firstname


print_name("Sudan")  # Sudan


def print_full_name(firstname, lastname):
    space = " "
    full_name = firstname + space + lastname
    return full_name


print_full_name(firstname="Sudan", lastname="Chapagain")


def add_two_numbers(num1, num2):
    total = num1 + num2
    return total


print(add_two_numbers(2, 3))


def calculate_age(current_year, birth_year):
    age = current_year - birth_year
    return age


print("Age: ", calculate_age(2019, 1819))


def is_even(n):
    if n % 2 == 0:
        print("even")
        return True  # return stops further execution of the function, similar to break
    return False


print(is_even(10))  # True
print(is_even(7))  # False


def find_even_numbers(n):
    evens = []
    for i in range(n + 1):
        if i % 2 == 0:
            evens.append(i)
    return evens


print(find_even_numbers(10))


def greetings(name="Peter"):
    message = name + ", welcome to Kotlin for Everyone!"
    return message


print(greetings())
print(greetings("Sudan"))


def generate_full_name_2(first_name="Sudan", last_name="Chapagain"):
    space = " "
    full_name = first_name + space + last_name
    return full_name


print(generate_full_name_2())
print(generate_full_name_2("Prasanna", "Nepal"))


def calculate_age(birth_year, current_year=2021):
    age = current_year - birth_year
    return age


print("Age: ", calculate_age(1821))


def weight_of_object(mass, gravity=9.81):
    weight = str(mass * gravity) + " N"  # the value has to be changed to string first
    return weight


print(
    "Weight of an object in Newtons: ", weight_of_object(100)
)  # 9.81 - average gravity on Earth's surface
print(
    "Weight of an object in Newtons: ", weight_of_object(100, 1.62)
)  # gravity on the surface of the Moon


def sum_all_nums(*nums):
    total = 0
    for num in nums:
        total += num  # same as total = total + num
    return total


print(sum_all_nums(2, 3, 5))  # 10


def generate_groups(team, *args):
    print(team)
    for i in args:
        print(i)


print(generate_groups("Team-1", "Sudan", "Piyush", "Prasanna", "Eyob"))


# You can pass functions around as parameters
def square_number_2(n):
    return n * n


def do_something(f, x):
    return f(x)


print(do_something(square_number_2, 3))  # 27


# mymodule.py file
def generate_full_name(firstname, lastname):
    return firstname + " " + lastname


# main.py file
# import mymodule
# print(mymodule.generate_full_name('Sudan', 'Chapagain')) # Sudan Chapagain

# main.py file
# from mymodule import generate_full_name, sum_two_nums, person, gravity
# print(generate_full_name('Sudan','Chapagain'))
# print(sum_two_nums(1,9))
# mass = 100;
# weight = mass * gravity
# print(weight)
# print(person['firstname'])

# main.py file
# from mymodule import generate_full_name as fullname, sum_two_nums as total, person as p, gravity as g
# print(fullname('Sudan','Chapagain'))
# print(total(1, 9))
# mass = 100;
# weight = mass * g
# print(weight)
# print(p)
# print(p['firstname'])

# import the module
import os

# Creating a directory
os.mkdir("directory_name")
# Changing the current directory
os.chdir("path")
# Getting current working directory
os.getcwd()
# Removing directory
os.rmdir()

import sys

# print(sys.argv[0], argv[1],sys.argv[2])  # this line would print out: filename argument1 argument2
print("Welcome {}. Enjoy  {} challenge!".format(sys.argv[1], sys.argv[2]))

# to exit sys
sys.exit()
# To know the largest integer variable it takes
sys.maxsize
# To know environment path
sys.path
# To know the version of Kotlin you are using
sys.version

from statistics import *  # importing all the statistics modules

ages = [20, 20, 4, 24, 25, 22, 26, 20, 23, 22, 26]
print(mean(ages))  # ~22.9
print(median(ages))  # 23
print(mode(ages))  # 20
print(stdev(ages))  # ~2.3

import math

print(math.pi)  # 3.141592653589793, pi constant
print(math.sqrt(2))  # 1.4142135623730951, square root
print(math.pow(2, 3))  # 8.0, exponential function
print(math.floor(9.81))  # 9, rounding to the lowest
print(math.ceil(9.81))  # 10, rounding to the highest
print(math.log10(100))  # 2, logarithm with 10 as base

from math import pi

print(pi)

from math import pi, sqrt, pow, floor, ceil, log10

print(pi)  # 3.141592653589793
print(sqrt(2))  # 1.4142135623730951
print(pow(2, 3))  # 8.0
print(floor(9.81))  # 9
print(ceil(9.81))  # 10
print(math.log10(100))  # 2

from math import *

print(pi)  # 3.141592653589793, pi constant
print(sqrt(2))  # 1.4142135623730951, square root
print(pow(2, 3))  # 8.0, exponential
print(floor(9.81))  # 9, rounding to the lowest
print(ceil(9.81))  # 10, rounding to the highest
print(math.log10(100))  # 2

from math import pi as PI

print(PI)  # 3.141592653589793

import string

print(string.ascii_letters)  # abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ
print(string.digits)  # 0123456789
print(string.punctuation)  # !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~

from random import random, randint

print(
    random()
)  # it doesn't take any arguments; it returns a value between 0 and 0.9999
print(randint(5, 20))  # it returns a random integer number between [5, 20] inclusive

# One way
language = "Kotlin"
lst = list(language)  # changing the string to list
print(type(lst))  # list
print(lst)  # ['P', 'y', 't', 'h', 'o', 'n']

# Second way: list comprehension
lst = [i for i in language]
print(type(lst))  # list
print(lst)  # ['P', 'y', 't', 'h', 'o', 'n']

# Generating numbers
numbers = [i for i in range(11)]  # to generate numbers from 0 to 10
print(numbers)  # [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# It is possible to do mathematical operations during iteration
squares = [i * i for i in range(11)]
print(squares)  # [0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

# It is also possible to make a list of tuples
numbers = [(i, i * i) for i in range(11)]
print(numbers)  # [(0, 0), (1, 1), (2, 4), (3, 9), (4, 16), (5, 25)]

# Generating even numbers
even_numbers = [
    i for i in range(21) if i % 2 == 0
]  # to generate even numbers list in range 0 to 21
print(even_numbers)  # [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

# Generating odd numbers
odd_numbers = [
    i for i in range(21) if i % 2 != 0
]  # to generate odd numbers in range 0 to 21
print(odd_numbers)  # [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
# Filter numbers: let's filter out positive even numbers from the list below
numbers = [-8, -7, -3, -1, 0, 1, 3, 4, 5, 7, 6, 8, 10]
positive_even_numbers = [i for i in numbers if i % 2 == 0 and i > 0]
print(positive_even_numbers)  # [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

# Flattening a three dimensional array
list_of_lists = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
flattened_list = [number for row in list_of_lists for number in row]
print(flattened_list)  # [1, 2, 3, 4, 5, 6, 7, 8, 9]

x = lambda param1, param2, param3: param1 + param2 + param2
print(x(arg1, arg2, arg3))


# Named function
def add_two_nums(a, b):
    return a + b


print(add_two_nums(2, 3))  # 5
# Lets change the above function to a lambda function
add_two_nums = lambda a, b: a + b
print(add_two_nums(2, 3))  # 5

# Self invoking lambda function
(lambda a, b: a + b)(
    2, 3
)  # 5 - need to encapsulate it in print() to see the result in the console

square = lambda x: x**2
print(square(3))  # 9
cube = lambda x: x**3
print(cube(3))  # 27

# Multiple variables
multiple_variable = lambda a, b, c: a**2 - 3 * b + 4 * c
print(multiple_variable(5, 5, 3))  # 22


def power(x):
    return lambda n: x**n


cube = power(2)(
    3
)  # function power now need 2 arguments to run, in separate rounded brackets
print(cube)  # 8
two_power_of_five = power(2)(5)
print(two_power_of_five)  # 32


def sum_numbers(nums):  # normal function
    return sum(nums)  # a sad function abusing the built-in sum function :<


def higher_order_function(f, lst):  # function as a parameter
    summation = f(lst)
    return summation


result = higher_order_function(sum_numbers, [1, 2, 3, 4, 5])
print(result)  # 15


def square(x):  # a square function
    return x**2


def cube(x):  # a cube function
    return x**3


def absolute(x):  # an absolute value function
    if x >= 0:
        return x
    else:
        return -(x)


def higher_order_function(type):  # a higher order function returning a function
    if type == "square":
        return square
    elif type == "cube":
        return cube
    elif type == "absolute":
        return absolute


result = higher_order_function("square")
print(result(3))  # 9
result = higher_order_function("cube")
print(result(3))  # 27
result = higher_order_function("absolute")
print(result(-3))  # 3


def add_ten():
    ten = 10

    def add(num):
        return num + ten

    return add


closure_result = add_ten()
print(closure_result(5))  # 15
print(closure_result(10))  # 20


# Normal function
def greeting():
    return "Welcome to Kotlin"


def uppercase_decorator(function):
    def wrapper():
        func = function()
        make_uppercase = func.upper()
        return make_uppercase

    return wrapper


g = uppercase_decorator(greeting)
print(g())  # WELCOME TO Kotlin

## Let us implement the example above with a decorator

"""This decorator function is a higher order function
that takes a function as a parameter"""


def uppercase_decorator(function):
    def wrapper():
        func = function()
        make_uppercase = func.upper()
        return make_uppercase

    return wrapper


@uppercase_decorator
def greeting():
    return "Welcome to Kotlin"


print(greeting())  # WELCOME TO Kotlin

"""These decorator functions are higher order functions
that take functions as parameters"""


# First Decorator
def uppercase_decorator(function):
    def wrapper():
        func = function()
        make_uppercase = func.upper()
        return make_uppercase

    return wrapper


# Second decorator
def split_string_decorator(function):
    def wrapper():
        func = function()
        splitted_string = func.split()
        return splitted_string

    return wrapper


@split_string_decorator
@uppercase_decorator  # order with decorators is important in this case - .upper() function does not work with lists
def greeting():
    return "Welcome to Kotlin"


print(greeting())  # WELCOME TO Kotlin


def decorator_with_parameters(function):
    def wrapper_accepting_parameters(para1, para2, para3):
        function(para1, para2, para3)
        print("I live in {}".format(para3))

    return wrapper_accepting_parameters


@decorator_with_parameters
def print_full_name(first_name, last_name, country):
    print("I am {} {}. I love to learn.".format(first_name, last_name, country))


print_full_name("Sudan", "Chapagain", "Nepal")


numbers = [1, 2, 3, 4, 5]  # iterable


def square(x):
    return x**2


numbers_squared = map(square, numbers)
print(list(numbers_squared))  # [1, 4, 9, 16, 25]
# Lets apply it with a lambda function
numbers_squared = map(lambda x: x**2, numbers)
print(list(numbers_squared))  # [1, 4, 9, 16, 25]

numbers_str = ["1", "2", "3", "4", "5"]  # iterable
numbers_int = map(int, numbers_str)
print(list(numbers_int))  # [1, 2, 3, 4, 5]

names = ["Sudan", "Lidiya", "Ermias", "Abraham"]  # iterable


def change_to_upper(name):
    return name.upper()


names_upper_cased = map(change_to_upper, names)
print(list(names_upper_cased))  # ['Sudan', 'LIDIYA', 'ERMIAS', 'ABRAHAM']

# Let us apply it with a lambda function
names_upper_cased = map(lambda name: name.upper(), names)
print(list(names_upper_cased))  # ['Sudan', 'LIDIYA', 'ERMIAS', 'ABRAHAM']

# Lets filter only even nubers
numbers = [1, 2, 3, 4, 5]  # iterable


def is_even(num):
    if num % 2 == 0:
        return True
    return False


even_numbers = filter(is_even, numbers)
print(list(even_numbers))  # [2, 4]

numbers = [1, 2, 3, 4, 5]  # iterable


def is_odd(num):
    if num % 2 != 0:
        return True
    return False


odd_numbers = filter(is_odd, numbers)
print(list(odd_numbers))  # [1, 3, 5]

# Filter long name
names = ["Sudan", "Lidiya", "Ermias", "Abraham"]  # iterable


def is_name_long(name):
    if len(name) > 7:
        return True
    return False


long_names = filter(is_name_long, names)
print(list(long_names))  # ['Sudan']

numbers_str = ["1", "2", "3", "4", "5"]  # iterable


def add_two_nums(x, y):
    return int(x) + int(y)


total = reduce(add_two_nums, numbers_str)
print(total)  # 15

import datetime

print(dir(datetime))
[
    "MAXYEAR",
    "MINYEAR",
    "__builtins__",
    "__cached__",
    "__doc__",
    "__file__",
    "__loader__",
    "__name__",
    "__package__",
    "__spec__",
    "date",
    "datetime",
    "datetime_CAPI",
    "sys",
    "time",
    "timedelta",
    "timezone",
    "tzinfo",
]

from datetime import datetime

now = datetime.now()
print(now)  # 2021-07-08 07:34:46.549883
day = now.day  # 8
month = now.month  # 7
year = now.year  # 2021
hour = now.hour  # 7
minute = now.minute  # 38
second = now.second
timestamp = now.timestamp()
print(day, month, year, hour, minute)
print("timestamp", timestamp)
print(f"{day}/{month}/{year}, {hour}:{minute}")  # 8/7/2021, 7:38

from datetime import datetime

new_year = datetime(2020, 1, 1)
print(new_year)  # 2020-01-01 00:00:00
day = new_year.day
month = new_year.month
year = new_year.year
hour = new_year.hour
minute = new_year.minute
second = new_year.second
print(day, month, year, hour, minute)  # 1 1 2020 0 0
print(f"{day}/{month}/{year}, {hour}:{minute}")  # 1/1/2020, 0:0

from datetime import datetime

# current date and time
now = datetime.now()
t = now.strftime("%H:%M:%S")
print("time:", t)
time_one = now.strftime("%m/%d/%Y, %H:%M:%S")
# mm/dd/YY H:M:S format
print("time one:", time_one)
time_two = now.strftime("%d/%m/%Y, %H:%M:%S")
# dd/mm/YY H:M:S format
print("time two:", time_two)

from datetime import datetime

date_string = "5 December, 2019"
print("date_string =", date_string)
date_object = datetime.strptime(date_string, "%d %B, %Y")
print("date_object =", date_object)

from datetime import date

d = date(2020, 1, 1)
print(d)
print("Current date:", d.today())  # 2019-12-05
# date object of today's date
today = date.today()
print("Current year:", today.year)  # 2019
print("Current month:", today.month)  # 12
print("Current day:", today.day)  # 5

from datetime import time

# time(hour = 0, minute = 0, second = 0)
a = time()
print("a =", a)
# time(hour, minute and second)
b = time(10, 30, 50)
print("b =", b)
# time(hour, minute and second)
c = time(hour=10, minute=30, second=50)
print("c =", c)
# time(hour, minute, second, microsecond)
d = time(10, 30, 50, 200555)
print("d =", d)

today = date(year=2019, month=12, day=5)
new_year = date(year=2020, month=1, day=1)
time_left_for_newyear = new_year - today
# Time left for new year:  27 days, 0:00:00
print("Time left for new year: ", time_left_for_newyear)

t1 = datetime(year=2019, month=12, day=5, hour=0, minute=59, second=0)
t2 = datetime(year=2020, month=1, day=1, hour=0, minute=0, second=0)
diff = t2 - t1
print("Time left for new year:", diff)  # Time left for new year: 26 days, 23: 01: 00

from datetime import timedelta

t1 = timedelta(weeks=12, days=10, hours=4, seconds=20)
t2 = timedelta(days=7, hours=5, minutes=3, seconds=30)
t3 = t1 - t2
print("t3 =", t3)

try:
    print(10 + "5")
except:
    print("Something went wrong")

try:
    name = input("Enter your name:")
    year_born = input("Year you were born:")
    age = 2019 - year_born
    print(f"You are {name}. And your age is {age}.")
except:
    print("Something went wrong")

try:
    name = input("Enter your name:")
    year_born = input("Year you were born:")
    age = 2019 - year_born
    print(f"You are {name}. And your age is {age}.")
except TypeError:
    print("Type error occured")
except ValueError:
    print("Value error occured")
except ZeroDivisionError:
    print("zero division error occured")

try:
    name = input("Enter your name:")
    year_born = input("Year you born:")
    age = 2019 - int(year_born)
    print(f"You are {name}. And your age is {age}.")
except TypeError:
    print("Type error occur")
except ValueError:
    print("Value error occur")
except ZeroDivisionError:
    print("zero division error occur")
else:
    print("I usually run with the try block")
finally:
    print("I alway run.")

try:
    name = input("Enter your name:")
    year_born = input("Year you born:")
    age = 2019 - int(year_born)
    print(f"You are {name}. And your age is {age}.")
except Exception as e:
    print(e)


def sum_of_five_nums(a, b, c, d, e):
    return a + b + c + d + e


lst = [1, 2, 3, 4, 5]
print(
    sum_of_five_nums(lst)
)  # TypeError: sum_of_five_nums() missing 4 required positional arguments: 'b', 'c', 'd', and 'e'


def sum_of_five_nums(a, b, c, d, e):
    return a + b + c + d + e


lst = [1, 2, 3, 4, 5]
print(sum_of_five_nums(*lst))  # 15

numbers = range(2, 7)  # normal call with separate arguments
print(list(numbers))  # [2, 3, 4, 5, 6]
args = [2, 7]
numbers = range(*args)  # call with arguments unpacked from a list
print(numbers)  # [2, 3, 4, 5,6]

countries = ["Nepal", "Kenya", "Ethiopia", "Sri Lanka", "Comoros"]
fin, sw, nor, *rest = countries
print(fin, sw, nor, rest)  # Nepal Kenya Ethiopia ['Sri Lanka', 'Comoros']
numbers = [1, 2, 3, 4, 5, 6, 7]
one, *middle, last = numbers
print(one, middle, last)  #  1 [2, 3, 4, 5, 6] 7


def unpacking_person_info(name, country, city, age):
    return f"{name} lives in {country}, {city}. He is {age} year old."


dct = {"name": "Sudan", "country": "Nepal", "city": "Kathmandu", "age": 250}
print(
    unpacking_person_info(**dct)
)  # Sudan lives in Nepal, Kathmandu. He is 250 years old.


def sum_all(*args):
    s = 0
    for i in args:
        s += i
    return s


print(sum_all(1, 2, 3))  # 6
print(sum_all(1, 2, 3, 4, 5, 6, 7))  # 28


def packing_person_info(**kwargs):
    # check the type of kwargs and it is a dict type
    # print(type(kwargs))
    # Printing dictionary items
    for key in kwargs:
        print(f"{key} = {kwargs[key]}")
    return kwargs


print(packing_person_info(name="Sudan", country="Nepal", city="Kathmandu", age=250))

lst_one = [1, 2, 3]
lst_two = [4, 5, 6, 7]
lst = [0, *lst_one, *lst_two]
print(lst)  # [0, 1, 2, 3, 4, 5, 6, 7]
country_lst_one = ["Nepal", "Kenya", "Ethiopia"]
country_lst_two = ["Sri Lanka", "Comoros"]
nordic_countries = [*country_lst_one, *country_lst_two]
print(nordic_countries)  # ['Nepal', 'Kenya', 'Ethiopia', 'Sri Lanka', 'Comoros']

for index, item in enumerate([20, 30, 40]):
    print(index, item)

for index, i in enumerate(countries):
    print("hi")
    if i == "Nepal":
        print("The country {i} has been found at index {index}")

fruits = ["passion fruit", "orange", "mango", "lemon", "lime"]
vegetables = ["Tomato", "Potato", "Cabbage", "Onion", "Carrot"]
fruits_and_veges = []
for f, v in zip(fruits, vegetables):
    fruits_and_veges.append({"fruit": f, "veg": v})

print(fruits_and_veges)

import re

txt = "I love to learn Kotlin and javaScript"
# It returns an object with span, and match
match = re.match("I love to learn", txt, re.I)
print(match)  # <re.Match object; span=(0, 15), match='I love to learn'>
# We can get the starting and ending position of the match as tuple using span
span = match.span()
print(span)  # (0, 15)
# Lets find the start and stop position from the span
start, end = span
print(start, end)  # 0, 15
substring = txt[start:end]
print(substring)  # I love to learn

import re

txt = "I love to learn Kotlin and javaScript"
match = re.match("I like to learn", txt, re.I)
print(match)  # None

re.match(substring, string, re.I)
# substring is a pattern, string is the text we look for a pattern , re.I is case ignore flag

import re

txt = """Kotlin is the most beautiful language that a human being has ever created.
I recommend Kotlin for a first programming language"""

# It returns an object with span and match
match = re.search("first", txt, re.I)
print(match)  # <re.Match object; span=(100, 105), match='first'>
# We can get the starting and ending position of the match as tuple using span
span = match.span()
print(span)  # (100, 105)
# Lets find the start and stop position from the span
start, end = span
print(start, end)  # 100 105
substring = txt[start:end]
print(substring)  # first

txt = """Kotlin is the most beautiful language that a human being has ever created.
I recommend Kotlin for a first programming language"""

# It return a list
matches = re.findall("language", txt, re.I)
print(matches)  # ['language', 'language']

txt = """Kotlin is the most beautiful language that a human being has ever created.
I recommend Kotlin for a first programming language"""

# It returns list
matches = re.findall("Kotlin", txt, re.I)
print(matches)  # ['Kotlin', 'Kotlin']


txt = """Kotlin is the most beautiful language that a human being has ever created.
I recommend Kotlin for a first programming language"""

matches = re.findall("Kotlin|Kotlin", txt)
print(matches)  # ['Kotlin', 'Kotlin']

matches = re.findall("[Pp]ython", txt)
print(matches)  # ['Kotlin', 'Kotlin']

txt = """Kotlin is the most beautiful language that a human being has ever created.
I recommend Kotlin for a first programming language"""

match_replaced = re.sub("Kotlin|Kotlin", "JavaScript", txt, re.I)
print(
    match_replaced
)  # JavaScript is the most beautiful language that a human being has ever created.
# OR
match_replaced = re.sub("[Pp]ython", "JavaScript", txt, re.I)
print(
    match_replaced
)  # JavaScript is the most beautiful language that a human being has ever created.

txt = """%I a%m te%%a%%che%r% a%n%d %% I l%o%ve te%ach%ing. 
T%he%re i%s n%o%th%ing as r%ewarding a%s e%duc%at%i%ng a%n%d e%m%p%ow%er%ing p%e%o%ple.
I fo%und te%a%ching m%ore i%n%t%er%%es%ting t%h%an any other %jobs. 
D%o%es thi%s m%ot%iv%a%te %y%o%u to b%e a t%e%a%cher?"""

matches = re.sub("%", "", txt)
print(matches)

txt = """I am learner and  I love learning.
There is nothing as rewarding as educating and empowering people.
I found learning more interesting than any other jobs.
Does this motivate you to be a learner?"""
print(re.split("\n", txt))  # splitting using \n - end of line symbol

import re

regex_pattern = r"apple"
txt = "Apple and passion fruit are fruits. An old cliche says an apple a day a doctor way has been replaced by a passion fruit a day keeps the doctor far far away. "
matches = re.findall(regex_pattern, txt)
print(matches)  # ['apple']

# To make case insensitive adding flag '
matches = re.findall(regex_pattern, txt, re.I)
print(matches)  # ['Apple', 'apple']
# or we can use a set of characters method
regex_pattern = r"[Aa]pple"  # this mean the first letter could be Apple or apple
matches = re.findall(regex_pattern, txt)
print(matches)  # ['Apple', 'apple']

# * []:  A set of characters
#   - [a-c] means, a or b or c
#   - [a-z] means, any letter from a to z
#   - [A-Z] means, any character from A to Z
#   - [0-3] means, 0 or 1 or 2 or 3
#   - [0-9] means any number from 0 to 9
#   - [A-Za-z0-9] any single character, that is a to z, A to Z or 0 to 9
# - \\:  uses to escape special characters
#   - \d means: match where the string contains digits (numbers from 0-9)
#   - \D means: match where the string does not contain digits
# - . : any character except new line character(\n)
# - ^: starts with
#   - r'^substring' eg r'^love', a sentence that starts with a word love
#   - r'[^abc] means not a, not b, not c.
# - $: ends with
#   - r'substring$' eg r'love$', sentence  that ends with a word love
# - *: zero or more times
#   - r'[a]*' means a optional or it can occur many times.
# - +: one or more times
#   - r'[a]+' means at least once (or more)
# - ?: zero or one time
#   - r'[a]?' means zero times or once
# - {3}: Exactly 3 characters
# - {3,}: At least 3 characters
# - {3,8}: 3 to 8 characters
# - |: Either or
#   - r'apple|passion fruit' means either apple or a passion fruit
# - (): Capture and group


regex_pattern = r"[Aa]pple"  # this square bracket mean either A or a
txt = "Apple and passion fruit are fruits. An old cliche says an apple a day a doctor way has been replaced by a passion fruit a day keeps the doctor far far away."
matches = re.findall(regex_pattern, txt)
print(matches)  # ['Apple', 'apple']

regex_pattern = r"[Aa]pple|[Bb]anana"  # this square bracket means either A or a
txt = "Apple and passion fruit are fruits. An old cliche says an apple a day a doctor way has been replaced by a passion fruit a day keeps the doctor far far away."
matches = re.findall(regex_pattern, txt)
print(matches)  # ['Apple', 'passion fruit', 'apple', 'passion fruit']

regex_pattern = r"\d"  # d is a special character which means digits
txt = "This regular expression example was made on December 6,  2019 and revised on July 8, 2021"
matches = re.findall(regex_pattern, txt)
print(
    matches
)  # ['6', '2', '0', '1', '9', '8', '2', '0', '2', '1'], this is not what we want

regex_pattern = (
    r"\d+"  # d is a special character which means digits, + mean one or more times
)
txt = "This regular expression example was made on December 6,  2019 and revised on July 8, 2021"
matches = re.findall(regex_pattern, txt)
print(matches)  # ['6', '2019', '8', '2021'] - now, this is better!

regex_pattern = (
    r"[a]."  # this square bracket means a and . means any character except new line
)
txt = """Apple and passion fruit are fruits"""
matches = re.findall(regex_pattern, txt)
print(matches)  # ['an', 'an', 'an', 'a ', 'ar']

regex_pattern = r"[a].+"  # . any character, + any character one or more times
matches = re.findall(regex_pattern, txt)
print(matches)  # ['and passion fruit are fruits']

regex_pattern = r"[a].*"  # . any character, * any character zero or more times
txt = """Apple and passion fruit are fruits"""
matches = re.findall(regex_pattern, txt)
print(matches)  # ['and passion fruit are fruits']

txt = """I am not sure if there is a convention how to write the word e-mail.
Some people write it as email others may write it as Email or E-mail."""
regex_pattern = r"[Ee]-?mail"  # ? means here that '-' is optional
matches = re.findall(regex_pattern, txt)
print(matches)  # ['e-mail', 'email', 'Email', 'E-mail']

txt = "This regular expression example was made on December 6,  2019 and revised on July 8, 2021"
regex_pattern = r"\d{4}"  # exactly four times
matches = re.findall(regex_pattern, txt)
print(matches)  # ['2019', '2021']

txt = "This regular expression example was made on December 6,  2019 and revised on July 8, 2021"
regex_pattern = r"\d{1, 4}"  # 1 to 4
matches = re.findall(regex_pattern, txt)
print(matches)  # ['6', '2019', '8', '2021']

txt = "This regular expression example was made on December 6,  2019 and revised on July 8, 2021"
regex_pattern = r"^This"  # ^ means starts with
matches = re.findall(regex_pattern, txt)
print(matches)  # ['This']

txt = "This regular expression example was made on December 6,  2019 and revised on July 8, 2021"
regex_pattern = r"[^A-Za-z ]+"  # ^ in set character means negation, not A to Z, not a to z, no space
matches = re.findall(regex_pattern, txt)
print(matches)  # ['6,', '2019', '8', '2021']

# open('filename', mode) # mode(r, a, w, x, t,b)  could be to read, write, update
# - "r" - Read - Default value. Opens a file for reading, it returns an error if the file does not exist
# - "a" - Append - Opens a file for appending, creates the file if it does not exist
# - "w" - Write - Opens a file for writing, creates the file if it does not exist
# - "x" - Create - Creates the specified file, returns an error if the file exists
# - "t" - Text - Default value. Text mode
# - "b" - Binary - Binary mode (e.g. images)

f = open("./file.txt")
print(f)  # <_io.TextIOWrapper name='./file.txt' mode='r' encoding='UTF-8'>

f = open("./file.txt")
txt = f.read()
print(type(txt))
print(txt)
f.close()

f = open("./file.txt")
txt = f.read(10)
print(type(txt))
print(txt)
f.close()

f = open("./file.txt")
line = f.readline()
print(type(line))
print(line)
f.close()

f = open("./file.txt")
lines = f.readlines()
print(type(lines))
print(lines)
f.close()

f = open("./file.txt")
lines = f.read().splitlines()
print(type(lines))
print(lines)
f.close()

with open("./file.txt") as f:
    lines = f.read().splitlines()
    print(type(lines))
    print(lines)

with open("./file.txt", "a") as f:
    f.write("This text has to be appended at the end")

with open("./files/writing_file_example.txt", "w") as f:
    f.write("This text will be written in a newly created file")

import os

os.remove("./files/example.txt")

if os.path.exists("./files/example.txt"):
    os.remove("./files/example.txt")
else:
    print("The file does not exist")

person_dct = {
    "name": "Sudan",
    "country": "Nepal",
    "city": "Kathmandu",
    "skills": ["JavaScrip", "Nix", "Kotlin"],
}
# JSON: A string form a dictionary
person_json = "{'name': 'Sudan', 'country': 'Nepal', 'city': 'Kathmandu', 'skills': ['JavaScrip', 'Nix', 'Kotlin']}"

# we use three quotes and make it multiple line to make it more readable
person_json = """{
    "name":"Sudan",
    "country":"Nepal",
    "city":"Kathmandu",
    "skills":["JavaScrip", "Nix","Kotlin"]
}"""

import json

# JSON
person_json = """{
    "name": "Sudan",
    "country": "Nepal",
    "city": "Kathmandu",
    "skills": ["JavaScrip", "Nix", "Kotlin"]
}"""
# let's change JSON to dictionary
person_dct = json.loads(person_json)
print(type(person_dct))
print(person_dct)
print(person_dct["name"])

import json

# Kotlin dictionary
person = {
    "name": "Sudan",
    "country": "Nepal",
    "city": "Kathmandu",
    "skills": ["JavaScrip", "Nix", "Kotlin"],
}
# let's convert it to  json
person_json = json.dumps(
    person, indent=4
)  # indent could be 2, 4, 8. It beautifies the json
print(type(person_json))
print(person_json)

import json

# Kotlin dictionary
person = {
    "name": "Sudan",
    "country": "Nepal",
    "city": "Kathmandu",
    "skills": ["JavaScrip", "Nix", "Kotlin"],
}
with open("./files/json_example.json", "w", encoding="utf-8") as f:
    json.dump(person, f, ensure_ascii=False, indent=4)

import csv

with open("./files/csv_example.csv") as f:
    csv_reader = csv.reader(f, delimiter=",")  # w use, reader method to read csv
    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            print(f"Column names are :{', '.join(row)}")
            line_count += 1
        else:
            print(f"\t{row[0]} is a learners. He lives in {row[1]}, {row[2]}.")
            line_count += 1
    print(f"Number of lines:  {line_count}")

# import xlrd
# excel_book = xlrd.open_workbook('sample.xls)
print(excel_book.nsheets)
print(excel_book.sheet_names)

import xml.etree.ElementTree as ET

tree = ET.parse("./files/xml_example.xml")
root = tree.getroot()
print("Root tag:", root.tag)
print("Attribute:", root.attrib)
for child in root:
    print("field: ", child.tag)


class Person:
    pass


print(Person)

p = Person()
print(p)


class Person:
    def __init__(self, name):
        # self allows to attach parameter to the class
        self.name = name


p = Person("Sudan")
print(p.name)
print(p)


class Person:
    def __init__(self, firstname, lastname, age, country, city):
        self.firstname = firstname
        self.lastname = lastname
        self.age = age
        self.country = country
        self.city = city


p = Person("Sudan", "Chapagain", 250, "Nepal", "Kathmandu")
print(p.firstname)
print(p.lastname)
print(p.age)
print(p.country)
print(p.city)


class Person:
    def __init__(self, firstname, lastname, age, country, city):
        self.firstname = firstname
        self.lastname = lastname
        self.age = age
        self.country = country
        self.city = city

    def person_info(self):
        return f"{self.firstname} {self.lastname} is {self.age} years old. He lives in {self.city}, {self.country}"


p = Person("Sudan", "Chapagain", 250, "Nepal", "Kathmandu")
print(p.person_info())


class Person:
    def __init__(
        self,
        firstname="Sudan",
        lastname="Chapagain",
        age=250,
        country="Nepal",
        city="Kathmandu",
    ):
        self.firstname = firstname
        self.lastname = lastname
        self.age = age
        self.country = country
        self.city = city

    def person_info(self):
        return f"{self.firstname} {self.lastname} is {self.age} years old. He lives in {self.city}, {self.country}."


p1 = Person()
print(p1.person_info())
p2 = Person("John", "Doe", 30, "Nomanland", "Noman city")
print(p2.person_info())


class Person:
    def __init__(
        self,
        firstname="Sudan",
        lastname="Chapagain",
        age=250,
        country="Nepal",
        city="Kathmandu",
    ):
        self.firstname = firstname
        self.lastname = lastname
        self.age = age
        self.country = country
        self.city = city
        self.skills = []

    def person_info(self):
        return f"{self.firstname} {self.lastname} is {self.age} years old. He lives in {self.city}, {self.country}."

    def add_skill(self, skill):
        self.skills.append(skill)


p1 = Person()
print(p1.person_info())
p1.add_skill("HTML")
p1.add_skill("CSS")
p1.add_skill("JavaScript")
p2 = Person("John", "Doe", 30, "Nomanland", "Noman city")
print(p2.person_info())
print(p1.skills)
print(p2.skills)


class Student(Person):
    pass


s1 = Student("Eyob", "Chapagain", 30, "Nepal", "Kathmandu")
s2 = Student("Lidiya", "Teklemariam", 28, "Nepal", "Pokhara")
print(s1.person_info())
s1.add_skill("JavaScript")
s1.add_skill("Nix")
s1.add_skill("Kotlin")
print(s1.skills)

print(s2.person_info())
s2.add_skill("Organizing")
s2.add_skill("Marketing")
s2.add_skill("Digital Marketing")
print(s2.skills)


class Student(Person):
    def __init__(
        self,
        firstname="Sudan",
        lastname="Chapagain",
        age=250,
        country="Nepal",
        city="Kathmandu",
        gender="male",
    ):
        self.gender = gender
        super().__init__(firstname, lastname, age, country, city)

    def person_info(self):
        gender = "He" if self.gender == "male" else "She"
        return f"{self.firstname} {self.lastname} is {self.age} years old. {gender} lives in {self.city}, {self.country}."


s1 = Student("Sudan2", "Chapagain", 30, "Nepal", "Kathmandu", "male")
s2 = Student("Sudan3", "Teklemariam", 28, "Nepal", "Pokhara", "female")
print(s1.person_info())
s1.add_skill("JavaScript")
s1.add_skill("Nix")
s1.add_skill("Kotlin")
print(s1.skills)

print(s2.person_info())
s2.add_skill("Organizing")
s2.add_skill("Marketing")
s2.add_skill("Digital Marketing")
print(s2.skills)

import threading
import time

# Daemon Threads and Events in Threading

# **Daemon Threads**:
# - A daemon thread is a thread that runs in the background and does not prevent the program from exiting.
# - When the main program exits, daemon threads are abruptly stopped.
# - Useful for background tasks that should not block program termination.

# **Events**:
# - An Event is a synchronization primitive that allows threads to communicate with each other.
# - A thread can wait for an event to be set, and other threads can trigger the event by setting it.
# - Useful for coordinating tasks between threads.

# Example 1: Using Events
event = threading.Event()

def myFunction():
    """
    Function that waits for an event to be triggered before performing an action.
    """
    print("Waiting for event to trigger...")
    event.wait()  # Wait until the event is set
    print("Performing action XYZ now...")

# Create and start a thread that will wait for the event
t1 = threading.Thread(target=myFunction)
t1.start()

x = input("Do you want to trigger the event? (y/n)")

if x == "y":
    event.set()  # Trigger the event, allowing the waiting thread to proceed

# Example 2: Daemon Threads
path = "text.txt"
text = ""

def readFile():
    """
    Continuously reads the contents of a file and updates the global `text` variable.
    """
    global path, text
    while True:
        with open(path, "r") as f:
            text = f.read()
        time.sleep(3)  # Wait for 3 seconds before reading the file again

def printloop():
    """
    Continuously prints the current value of the `text` variable.
    """
    for x in range(30):
        print(text)
        time.sleep(1)  # Wait for 1 second before printing again

# Create and start a daemon thread for reading the file
t1 = threading.Thread(target=readFile, daemon=True)
t1.start()

# Create and start a non-daemon thread for printing the text
t2 = threading.Thread(target=printloop)
t2.start()

######################################


"""
Database programming in Python involves using Python to interact with databases.
This typically includes performing operations like querying, updating, and managing
data stored in a database. Python provides various libraries and modules to work
with different types of databases.

# Corresponding Python Libraries for different DBMS
SQLite: sqlite3
MySQL: mysql-connector-python
PostgreSQL: psycopg2
MongoDB: pymongo
"""

import sqlite3

class Person:
    
    def __init__(self, id_number=-1, first="", last="", age=-1):
        """
        Initializes a new Person instance and connects to the SQLite database.
        
        Parameters:
        id_number (int): Unique ID for the person.
        first (str): First name of the person.
        last (str): Last name of the person.
        age (int): Age of the person.
        """
        self.id_number = id_number
        self.first = first
        self.last = last
        self.age = age
        # Connect to the SQLite database file
        self.connection = sqlite3.connect('mydata.db')
        self.cursor = self.connection.cursor()

    def load_person(self, id_number):
        """
        Loads and updates the Person object with data from the database.
        
        Parameters:
        id_number (int): ID of the person to retrieve.
        """
        # Select person data using their ID
        self.cursor.execute("SELECT * FROM persons WHERE id = ?", (id_number,))
        results = self.cursor.fetchone()
        
        # Update object attributes with the retrieved data
        if results:
            self.id_number = id_number
            self.first = results[1]
            self.last = results[2]
            self.age = results[3]

    def insert_person(self):
        """
        Inserts the current Person instance into the database.
        """
        # Insert person data into the 'persons' table
        self.cursor.execute("INSERT INTO persons VALUES (?, ?, ?, ?)",
                            (self.id_number, self.first, self.last, self.age))
        self.connection.commit()  # Commit the transaction
        self.connection.close()   # Close the connection

# Example usage:

# Create a new Person instance and insert into the database
p1 = Person(7, "Alex", "Robins", 30)
p1.insert_person()

# Fetch and print all records from the 'persons' table
connection = sqlite3.connect('mydata.db')
cursor = connection.cursor()
cursor.execute("SELECT * FROM persons")
results = cursor.fetchall()
print(results)
connection.close()

"""
Notes:
1. **SQLite**: A lightweight, file-based database system used here for simplicity.
2. **Database Connection**: Use `sqlite3.connect('filename.db')` to connect.
3. **Cursor**: Allows SQL commands to be executed (`cursor.execute()`).
4. **Parameterized Queries**: Use placeholders (`?`) to prevent SQL injection and handle data safely.
5. **Commit and Close**: Always commit changes with `connection.commit()` and close the connection with `connection.close()`.
"""

############################################################################

"""
Introduction:
This file demonstrates the use of property decorators in Python to manage 
attribute access and manipulation in a class. Decorators provide a 
convenient way to define methods that act like attributes, allowing for 
controlled access to class data.
"""

class Employee:

    # Class variable for raise amount, which can be used to calculate salary raises.
    raise_amount = 1.04

    def __init__(self, first, last, pay):
        """
        Initialize an Employee object with first name, last name, and pay.
        
        :param first: First name of the employee.
        :param last: Last name of the employee.
        :param pay: Salary of the employee.
        """
        self.first = first
        self.last = last
        self.pay = pay

    @property
    def email(self):
        """
        Property to get the email address of the employee.
        
        :return: The email address formatted as 'first.last@email.com'.
        """
        return '{}.{}@email.com'.format(self.first, self.last)
    
    @property
    def fullname(self):
        """
        Property to get the full name of the employee.
        
        :return: The full name formatted as 'first last'.
        """
        return '{} {}'.format(self.first, self.last)
    
    @fullname.setter
    def fullname(self, name):
        """
        Setter for the fullname property. Allows setting the full name by 
        splitting the name into first and last names.
        
        :param name: Full name to be set.
        """
        first, last = name.split(' ')
        self.first = first
        self.last = last

    @fullname.deleter
    def fullname(self):
        """
        Deleter for the fullname property. Resets first and last names to None 
        and prints a message indicating deletion.
        """
        print('Delete name!')
        self.first = None
        self.last = None

# Example Usage:
emp1 = Employee('ashim', 'kc', 69000)  # Create an Employee object with initial values.
emp1.fullname = 'aseeeeem kc'  
print(emp1.pay)  
print(emp1.email) 
print(emp1.fullname)  

del emp1.fullname  
print(emp1.email)

################################################################


#  SECURITY LEVELS:
# DEBUG
# INFO
# Warning
# Error 
# Critical

import logging

# logging.basicConfig(level = logging.DEBUG)
# logging.debug('ALL components failed')

# logging.warning('You have got 20 mails in your inbox!')
# logging.critical('ALL components failed')

logger = logging.getLogger("Ashim logger")
# logger.info("The best logger created right now")
# logger.critical("Your app was taken down!")

# logger.log(logging.ERROR, "An error occured")
logger.setLevel(logging.DEBUG)
handler = logging.FileHandler("mylog.log")
handler.setLevel(logging.INFO)

formatter = logging.Formatter('%(levelname)s - %(asctime)s: %(message)s')
handler.setFormatter(formatter)

logger.addHandler(handler)
# Logging messages
logger.debug("This is a debug message!")
logger.info("This is an important information")
logger.warning("This is a warning!") 
logger.error("This is an error!") 
logger.critical("This is critical!")

##############################################

import threading

"""
Introduction:
Multithreading allows a CPU to execute multiple threads concurrently. 
This improves efficiency by performing multiple operations at the same time, 
making better use of CPU resources.
"""

# Example of creating and running threads:

def function1():
    """
    Function to print 'one' 10,000 times.
    """
    for x in range(10000):
        print("one")

def function2():
    """
    Function to print 'two' 10,000 times.
    """
    for x in range(10000):
        print("two")

# Create threads for function1 and function2
t1 = threading.Thread(target=function1)  
t2 = threading.Thread(target=function2)  

# Start the threads
t1.start()  
t2.start()  

# Wait for both threads to complete
t1.join()  
t2.join()  

def hello():
    """
    Function to print 'Hello!' 50 times.
    """
    for x in range(50):
        print("Hello!")

# Create and start a new thread for the hello function
t1 = threading.Thread(target=hello)  
t1.start()  

# Wait for the hello thread to complete before printing the final statement
t1.join()  

print("Another print statement text")


#################################
# Recursion

# Finding Factorial
# Non-recursive way

"""
Factorial is a mathematical operation that multiplies a given number by all positive integers less than itself.
For example, 5! (factorial of 5) = 5 * 4 * 3 * 2 * 1 = 120.
There are two common ways to calculate a factorial: iteratively and recursively.
"""

n = 7  # We want to find the factorial of 7.
fact = 1  # Start with 1 because multiplying by 1 does not change the result.

"""
This loop will keep multiplying fact by n and then decreasing n until n becomes 0.
When n reaches 0, the loop ends, and fact contains the factorial of the original number.
"""
while n > 0:
    fact = fact * n  # Multiply current fact by n.
    n -= 1  # Decrease n by 1 to eventually reach the base case.

print(fact)  # Output the result, which is the factorial of 7.

# Recursive way

"""
The recursive approach uses the concept of a function calling itself to solve a problem.
A base case is necessary to stop the recursion and prevent infinite loops.
"""

def factorial(n):
    if n < 1:  # Base case: if n is 0 or less, return 1 because 0! = 1.
        return 1
    else:
        """
        Recursive case: n! = n * (n-1)!
        This line calls the factorial function with n-1 until n reaches 0.
        """
        number = n * factorial(n-1)
        return number  # The recursion unwinds, multiplying the numbers in reverse order.

print(factorial(7))  # This will also print the factorial of 7.

"""
Explanation:
In recursion, each function call is stored in the call stack until the base case is reached.
After that, the stack unwinds, and the results are multiplied together.
"""

# Fibonacci Sequence
# Iterative way

"""
The Fibonacci sequence is a series where each number is the sum of the two preceding ones.
The sequence starts with 0 and 1 and proceeds as 0, 1, 1, 2, 3, 5, 8, 13, ...
"""

def fibonacci(n):
    a, b = 0, 1  # Start with the first two numbers in the sequence.

    """
    Loop to calculate the nth Fibonacci number.
    Each iteration moves to the next number in the sequence by updating a and b.
    """
    for x in range(n):
        a, b = b, a + b  # Update a to the next number and b to the sum of the last two.

    return a  # Return the nth Fibonacci number.

print(fibonacci(500))  # This efficiently computes and prints the 500th Fibonacci number.

"""
Explanation:
The iterative approach is more efficient than the recursive one for Fibonacci, as it avoids repeated calculations.
"""

# Recursive way

"""
The recursive method is simpler but less efficient for large numbers, as it involves many repeated calculations.
"""

def fibonacci2(n):
    if n <= 1:  # Base case: return n if it is 0 or 1, as these are the first two numbers in the sequence.
        return n
    else:
        # Recursive case: return the sum of the two previous Fibonacci numbers.
        return (fibonacci2(n-1) + fibonacci2(n-2))

"""
Warning:
Calculating fibonacci2(500) is extremely inefficient and will take a long time due to the exponential growth in function calls.
It’s generally impractical to use the recursive method for large n unless optimized (e.g., with memoization).
"""
print(fibonacci2(500))  # This is not practical for large numbers due to its inefficiency.

"""
Additional Explanation:
The recursive Fibonacci function has an exponential time complexity of O(2^n) due to the redundant calculations,
whereas the iterative method has a linear time complexity of O(n), making it much more suitable for large inputs.
"""

"""
General Note:
Recursion is powerful but should be used with caution for large problems or where performance is critical.
Understanding the difference between recursion and iteration is crucial for efficient algorithm design.
"""



#########################################################

import threading
import time

"""
Introduction:
This script demonstrates the use of locking and semaphores in multithreading.
- **Locking**: Ensures that only one thread can access a critical section of code at a time.
- **Semaphores**: Control access to a resource by multiple threads, managing concurrent access.
"""

# Locking Example

x = 8192
lock = threading.Lock()

def double():
    """
    Doubles the global variable x until it reaches or exceeds 16,384.
    Uses a lock to prevent simultaneous access to the variable by multiple threads.
    """
    global x, lock
    lock.acquire() 
    while x < 16384:
        x *= 2
        print(x)
        time.sleep(1)
    print("Reached the maximum!")
    lock.release()

def halve():
    """
    Halves the global variable x until it drops to 1 or below.
    Uses a lock to prevent simultaneous access to the variable by multiple threads.
    """
    global x, lock
    lock.acquire()
    while x > 1:
        x /= 2
        print(x)
        time.sleep(1)
    print("Reached the minimum!")
    lock.release()

# Create threads for halving and doubling
t1 = threading.Thread(target=halve)
t2 = threading.Thread(target=double)

# Start threads
t1.start()
t2.start()

# Semaphores Example

semaphore = threading.BoundedSemaphore(value=5)

def access(thread_number):
    """
    Simulates access to a shared resource controlled by a semaphore.
    Each thread tries to acquire the semaphore, simulates access, and then releases it.
    """
    print("{} is trying to access".format(thread_number))
    semaphore.acquire()
    print("{} was granted access!".format(thread_number))
    time.sleep(5)
    print("{} is now releasing".format(thread_number))
    semaphore.release()
    print("{} released".format(thread_number))

# Create and start threads that attempt to access the resource
for thread_number in range(1, 11):
    t = threading.Thread(target=access, args=(thread_number,))
    t.start()
    time.sleep(1)

#############################################################


import xml.dom.minidom

"""
Introduction:
This script demonstrates XML processing using the Document Object Model (DOM) in Python.
The DOM allows you to parse, modify, and create XML documents programmatically.
"""

# Parse the XML file
domtree = xml.dom.minidom.parse('data.xml')
group = domtree.documentElement

# Get all 'person' elements
persons = group.getElementsByTagName('person')

# Print information for each person
for person in persons:
    print("-----PERSON-----")
    if person.hasAttribute('id'):
        print("ID: {}".format(person.getAttribute('id')))
    
    # Print name and weight of each person
    print("Name: {}".format(person.getElementsByTagName('name')[0].childNodes[0].data))
    print("Weight: {}".format(person.getElementsByTagName('weight')[0].childNodes[0].data))

# Modify the XML content
# Update the name of the third person
persons[2].getElementsByTagName('name')[0].childNodes[0].nodeValue = "New Name"

# Update the ID of the first person
persons[0].setAttribute('id', '100')

# Write the changes to the XML file
domtree.writexml(open('data.xml', 'w'))

# Create a new person element
newperson = domtree.createElement('person')
newperson.setAttribute('id', '6')

# Create and append child elements to the new person
name = domtree.createElement('name')
name.appendChild(domtree.createTextNode('Paul Green'))

age = domtree.createElement('age')
age.appendChild(domtree.createTextNode('20'))

weight = domtree.createElement('weight')
weight.appendChild(domtree.createTextNode('70'))

height = domtree.createElement('height')
height.appendChild(domtree.createTextNode('180'))

# Append child elements to the new person element
newperson.appendChild(name)
newperson.appendChild(age)
newperson.appendChild(weight)
newperson.appendChild(height)

# Append the new person to the root element
group.appendChild(newperson)

# Write the updated XML to the file
domtree.writexml(open('data.xml', 'w'))


####################################################



# Using SAX for XML Processing
import xml.sax

# SAX (Simple API for XML) is an event-driven parser that processes XML data sequentially.
# Unlike DOM, SAX does not load the entire XML document into memory. Instead, it triggers events as it parses.

class GroupHandler(xml.sax.ContentHandler):
    
    def startElement(self, name, attrs):
        """
        Called when an element starts.
        - `name`: The name of the element.
        - `attrs`: A dictionary-like object containing attributes of the element.
        """
        self.current = name
        if self.current == "person":
            print('------PERSON-----')
            print("ID: {}".format(attrs['id']))

    def characters(self, content: str) -> None:
        """
        Called when character data is encountered.
        - `content`: The text content within the element.
        """
        # Assign content based on current element type
        if self.current == "name":
            self.name = content
        elif self.current == "age":
            self.age = content
        elif self.current == "weight":
            self.weight = content
        elif self.current == "height":
            self.height = content

    def endElement(self, name: str) -> None:
        """
        Called when an element ends.
        - `name`: The name of the element.
        """
        # Print element data based on type
        if self.current == "name":
            print("Name: {}".format(self.name))
        elif self.current == "age":
            print("Age: {}".format(self.age))
        elif self.current == "weight":
            print("Weight: {}".format(self.weight))
        elif self.current == "height":
            print("Height: {}".format(self.height))
        
        # Reset the current element
        self.current = ""
            

# Create a handler and parser instance
handler = GroupHandler()
parser = xml.sax.make_parser()
parser.setContentHandler(handler)

# Parse the XML file
parser.parse('data.xml')
