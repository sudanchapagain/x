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
print(first_letter)  # K
second_letter = language[1]
print(second_letter)  # o
last_index = len(language) - 1
last_letter = language[last_index]
print(last_letter)  # n

# If we want to start from right end we can use negative indexing. -1 is the last index
language = "Kotlin"
last_letter = language[-1]
print(last_letter)  # n
second_last = language[-2]
print(second_last)  # i

# Slicing
language = "Kotlin"
first_three = language[0:3]  # starts at zero index and up to 3 but not include 3
last_three = language[3:6]
print(last_three)  # lin
# Another way
last_three = language[-3:]
print(last_three)  # lin
last_three = language[3:6]
print(last_three)  # lin

# Skipping character while splitting Kotlin strings
language = "Kotlin"
pto = language[0:6:2]  #
print(pto)  # kti

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
