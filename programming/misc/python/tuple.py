# A tuple is a collection of different data types which is ordered and unchangeable (immutable).
# Tuples are written with round brackets, (). Once a tuple is created, we cannot change its values.
# We cannot use add, insert, remove methods in a tuple because it is not modifiable (mutable).
# Unlike list, tuple has few methods. Methods related to tuples:

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
