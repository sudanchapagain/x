empty_dict = {}

# Dictionary with data values
dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Postgres", "Kotlin"],
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
    "skills": ["JavaScript", "Nix", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}

print(person["first_name"])  # Sudan
print(person["country"])  # Nepal
print(person["skills"])  # ['JavaScript', 'Nix', 'Postgres', 'Kotlin']
print(person["skills"][0])  # JavaScript
print(person["address"]["street"])  # Space street
print(person["city"])  # Error

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Postgres", "Kotlin"],
    "address": {"street": "Space street", "zipcode": "02210"},
}
print(person.get("first_name"))  # Sudan
print(person.get("country"))  # Nepal
print(
    person.get("skills")
)  # ['HTML','CSS','JavaScript', 'Nix', 'Postgres', 'Kotlin']
print(person.get("city"))  # None

dct = {"key1": "value1", "key2": "value2", "key3": "value3", "key4": "value4"}
dct["key5"] = "value5"

person = {
    "first_name": "Sudan",
    "last_name": "Chapagain",
    "age": 250,
    "country": "Nepal",
    "is_marred": True,
    "skills": ["JavaScript", "Nix", "Postgres", "Kotlin"],
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
