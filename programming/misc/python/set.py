# Set is a collection of items. Let me take you back to your elementary or high school Mathematics lesson.
# The Mathematics definition of a set can be applied also in Python.
# Set is a collection of unordered and un-indexed distinct elements.
# In Python set is used to store unique items, and it is possible to find the
# _union_, _intersection_, _difference_, _symmetric difference_, _subset_, _super set_ and _disjoint set_ among sets.

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

python = {"p", "y", "t", "h", "o", "n"}
dragon = {"d", "r", "a", "g", "o", "n"}
python.intersection(dragon)  # {'o', 'n'}

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item2", "item3"}
st2.issubset(st1)  # True
st1.issuperset(st2)  # True

whole_numbers = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
even_numbers = {0, 2, 4, 6, 8, 10}
whole_numbers.issubset(even_numbers)  # False, because it is a super set
whole_numbers.issuperset(even_numbers)  # True

python = {"p", "y", "t", "h", "o", "n"}
dragon = {"d", "r", "a", "g", "o", "n"}
python.issubset(dragon)  # False

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item2", "item3"}
st2.difference(st1)  # set()
st1.difference(st2)  # {'item1', 'item4'} => st1\st2

whole_numbers = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
even_numbers = {0, 2, 4, 6, 8, 10}
whole_numbers.difference(even_numbers)  # {1, 3, 5, 7, 9}

python = {"p", "y", "t", "o", "n"}
dragon = {"d", "r", "a", "g", "o", "n"}
python.difference(
    dragon
)  # {'p', 'y', 't'}  - the result is unordered (characteristic of sets)
dragon.difference(python)  # {'d', 'r', 'a', 'g'}

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item2", "item3"}
# it means (A\B)∪(B\A)
st2.symmetric_difference(st1)  # {'item1', 'item4'}

whole_numbers = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
some_numbers = {1, 2, 3, 4, 5}
whole_numbers.symmetric_difference(some_numbers)  # {0, 6, 7, 8, 9, 10}

python = {"p", "y", "t", "h", "o", "n"}
dragon = {"d", "r", "a", "g", "o", "n"}
python.symmetric_difference(dragon)  # {'r', 't', 'p', 'y', 'g', 'a', 'd', 'h'}

st1 = {"item1", "item2", "item3", "item4"}
st2 = {"item2", "item3"}
st2.isdisjoint(st1)  # False

even_numbers = {0, 2, 4, 6, 8}
odd_numbers = {1, 3, 5, 7, 9}
even_numbers.isdisjoint(odd_numbers)  # True, because no common item

python = {"p", "y", "t", "h", "o", "n"}
dragon = {"d", "r", "a", "g", "o", "n"}
python.isdisjoint(dragon)  # False, there are common items {'o', 'n'}
