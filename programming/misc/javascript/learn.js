"use strict";
// directive in JavaScript that enables strict mode
// must use let or var (if none, js assumes global)
// must not have duplicate params

// select DOM elements
document.getElementById("heading").innerHTML = "Hello World";

// diff between var & let
function varEX() {
    var _x = 12;
    if (true) {
        var x = 13;
        console.log("Inside if block with var:" + x);
    }
    console.log("Outside if block with var" + x);
}
function letEx() {
    let y = 1;
    if (true) {
        let y = 2;
        console.log("Inside if block with let: " + y);
    }
    console.log("Outside if block with let: " + y);
}

varEX();
letEx();

// data types
let nums = 41;
let myname = "Sudan";
let isActive = true;
let personnn = { fistname: "Sudan", lastname: "Chapagain" };
let colors = ["red", "green", "blue"];
let sumfunc = function (a, b) {
    return a + b;
};
let nothing = null;
let notDefined;

/*
Primitive data types includes:

1. Numbers - Integers, floats
2. Strings - Any data under single or double quote
3. Booleans - true or false value
4. Null - empty value or no value
5. Undefined - a declared variable without a value

Non-primitive data types includes:

1. Objects
2. Functions
3. Arrays
*/

console.log("Type of nums:", typeof nums);
console.log("Type of myname:", typeof myname);
console.log("Type of isActive:", typeof isActive);
console.log("Type of personnn:", typeof personnn);
console.log("Type of colors:", typeof colors);
console.log("Type of sumfunc:", typeof sumfunc);
console.log("Type of nothing:", typeof nothing);
console.log("Type of notDefined:", typeof notDefined);

// == vs ===
console.log("" == false);
console.log("" === false);

// if else & prompt
const nu = prompt("Enter a number:");
if (nu % 2 === 0) {
    alert("The given number " + nu + " is even");
} else {
    alert("The given number " + nu + " is odd");
}

// logical operators & scope rules without braces
let numb = prompt("Enter a number:");
numb = parseInt(numb);

if (numb >= 0 && numb <= 9) alert("The given number is single digit");
else if (numb >= 10 && numb <= 99) alert("The given number is double digit");
else if (numb >= 100 && numb <= 999)
    alert("The given number is triple digit number");
else alert("The given number is multi digit number");

// switch
let numbe;
let num1 = parseFloat(prompt("Enter first number"));
let num2 = parseFloat(prompt("Enter second number"));
let ops = prompt("Enter the operator (1 for +, 2 for -, 3 for *, 4 for /)");

switch (ops) {
    case "1":
        numbe = num1 + num2;
        alert("The number after the operation is " + numbe);
        break;
    case "2":
        numbe = num1 - num2;
        alert("The number after the operation is " + numbe);
        break;
    case "3":
        numbe = num1 * num2;
        alert("The number after the opeartion is " + numbe);
        break;
    case "4":
        numbe = num1 / num2;
        alert("The number after the operation is " + numbe);
        break;
    default:
        alert("Invalid input!!!");
}

// for loops
let number = parseInt(prompt("Enter a number:"));
let result1 = "";

for (let i = 1; i <= 10; i++) {
    result1 += number + " * " + i + " = " + number * i + "\n";
}

alert(result1);

// breaking out of labeled loops
outer: for (var i = 0; i < 10; i++) {
    for (var j = 0; j < 10; j++) {
        if (i == 5 && j == 5) {
            break outer;
        }
    }
}

// while & do while
let summ = 0;
let addMore;

do {
    let num = parseFloat(prompt("Enter a number:"));
    summ += num;
    addMore = confirm("Do you want to add another number?");
} while (addMore);

alert("The total sum of all entered numbers is: " + summ);

// func declare
function afunctiondeclaration(argument) {
    return "args: " + argument;
}

// func expression
let funcexpn = function () {
    return 0;
};

// anon func & arrow func
const anonFunc = function () {
    return "Anon called!";
};
const arrowFunc = () => "Arrow called!";
console.log(anonFunc(), arrowFunc());

const square = (n) => n * n;

// Self invoking functions (immediately invoked function)
(function (n) {
    return n * n;
})(2);

const xy = (function (n) {
    return n * n;
})(2);
console.log(xy); // 4

// func invoke
function normalFunc() {
    return "called as function";
}
const objj = {
    method() {
        return "called as method";
    },
};

function ConstructorFunc() {
    this.msg = "called as constructor";
}
const instance = new ConstructorFunc();

console.log(normalFunc(), objj.method(), instance.msg);
console.log(normalFunc.call(null), normalFunc.apply(null));

// implicit invoke
const implicitObj = {
    name: "Implicit",
    sayName() {
        return this.name;
    },
};
console.log(implicitObj.sayName());

// a callback function, the function could be any name
const callback = (n) => {
    return n ** 2;
};

// function take other function as a callback
function cube(callback, n) {
    return callback(n) * n;
}

console.log(cube(callback, 3));

// Higher order function returning an other function
const higherOrder = (n) => {
    const doSomething = (m) => {
        const doWhatEver = (t) => {
            return 2 * n + 3 * m + t;
        };
        return doWhatEver;
    };

    return doSomething;
};
console.log(higherOrder(2)(3)(10));

// array literal
let array1 = [1, 2, 3, 4, 5];
console.log("Array 1 (Array Literal):", array1);

// array constructor
let array2 = new Array(5);
console.log("Array 2 (Array Constructor with length):", array2);

// array constructor with elements
let array3 = new Array(10, 20, 30, 40, 50);
console.log("Array 3 (Array Constructor with elements):", array3);

// array of method
let array4 = Array.of(100, 200, 300);
console.log("Array 4 (Array.of()):", array4);

// array from method
let array5 = Array.from("Hello");
console.log("Array 5 (Array.from() from a string):", array5);

// object to array with array from
let arrayLike = { 0: "a", 1: "b", 2: "c", length: 3 };
let array6 = Array.from(arrayLike);
console.log("Array 6 (Array.from() from an array-like object):", array6);

// spread operator
let array7 = [...array1, ...array3];
console.log("Array 7 (Spread Operator):", array7);

// 2d & 3d
let twoDArray = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9],
];

console.log("Two-Dimensional Array:", twoDArray);

let threeDArray = [
    [
        [1, 2, 3],
        [4, 5, 6],
    ],
    [
        [7, 8, 9],
        [10, 11, 12],
    ],
];

console.log("Three-Dimensional Array:", threeDArray);
console.log("Element at [0][1][2]:", threeDArray[0][1][2]);

// destructing Arrays
const numbersoo = [1, 2, 3];
let [numOne, numTwo, numThree] = numbersoo;
console.log(numOne, numTwo, numThree);

const nameso = ["Sudan", "Nepal", "Russia", "America"];
let [firstPerson, secondPerson, thirdPerson, fourthPerson] = names;
console.log(firstName, secondPerson, thirdPerson, fourthPerson);

const scientificConstants = [2.72, 3.14, 9.81, 37, 100];
let [e, pi, gravity, bodyTemp, boilingTemp] = scientificConstants;
console.log(e, pi, gravity, bodyTemp, boilingTemp);

const numberso = [1, 2, 3];
let [numOneo, , , numThreeo] = numbers;
console.log(numOneo, numThreeo);

let [
    firstoPerson = "Asabeneh",
    secondoPerson,
    thirdoPerson,
    fourthoPerson = "John",
] = nameso;
console.log(firstoPerson, secondoPerson, thirdoPerson, fourthoPerson);

// forEach iterates over each item in the array. does not return anything
// map Iterates over each item & returns a new array with modified elements based on the callback function.
// filter Iterates over each item & returns a new array containing only elements that match the given condition. does not modify the original array.
// reduce iterates over each item & returns a single value (not necessarily an array). it is used for calculations like sum, average, or accumulating values.

// for each
const numberss = [10, 20, 30, 40, 50];

numberss.forEach(function (number, index) {
    console.log("Index: " + index + ", Value: " + number);
});

// arrow func syntax
numberss.forEach((number, index) => {
    console.log(`Index: ${index}, Value: ${number}`);
});

const sumArray = (arr) => {
    let sum = 0;
    const callBack = function (element) {
        sum += element;
    };
    numberss.forEach(callback);
    return sum;
};
console.log(sumArray(numberss));

const sumArrays = (arr) => {
    let sum = 0;
    numbers.forEach(function (element) {
        sum += element;
    });
    return sum;
};
console.log(sumArrays(numbers));

arr.forEach(function (element, index) {
    console.log(index, element);
});

arr.forEach((element, index) => {
    console.log(index, element);
});

const modifiedArray = arr.map(function (element, index) {
    return element;
});

const numbersSquare = numbers.map((num) => num * num);

// iteration over array & objects
const fruitss = ["Apple", "Banana", "Cherry"];
const car = {
    make: "Toyota",
    model: "Corolla",
    year: 2021,
};

// for of
for (const fruit of fruitss) {
    console.log(fruit);
}
// for in properties
for (const key in car) {
    console.log(key + ": " + car[key]);
}
// for in indices
for (const index in fruits) {
    console.log(index + ": " + fruits[index]);
}

// splice
let index = 2;
fruits.splice(index, 1);

let fruits = ["apple", "banana", "mango", "orange", "banana"];

console.log("Includes 'mango'?", fruits.includes("mango")); // true : if exists
console.log(fruits.find((f) => f.startsWith("b"))); // "banana" : first match

console.log(fruits.findIndex((f) => f.startsWith("o"))); // 3 : first match index
console.log(fruits.indexOf("banana")); // 1 : first occurance

console.log(fruits.lastIndexOf("banana")); // 4 : last ,,
console.log(fruits.join(", ")); // "apple, banana, mango, orange, banana"
console.log(fruits.concat(["pineapple", "grape"]));
console.log(fruits.pop(), fruits);

fruits.push("pear");
console.log(fruits);
console.log(fruits.reverse());
console.log(fruits.shift(), fruits);

fruits.unshift("strawberry");
console.log(fruits);
console.log(fruits.slice(1, 3)); // extract portion

console.log(fruits.map((f) => f.toUpperCase())); // transformation with iteration
console.log(fruits.sort());
console.log(fruits.every((f) => /^[aeiou]/i.test(f))); // false : test condition

const countriesContainingLand = countries.filter((country) =>
    country.includes("land"),
);
console.log(countriesContainingLand);

const countriesEndByia = countries.filter((country) => country.includes("ia"));
console.log(countriesEndByia);

const countriesHaveFiveLetters = countries.filter(
    (country) => country.length === 5,
);
console.log(countriesHaveFiveLetters);

const scores = [
    { name: "Sudan", score: 94 },
    { name: "Rupen", score: 79 },
    { name: "Mukunda", score: 49 },
    { name: "Safal", score: 84 },
    { name: "Prince", score: 100 },
];

const scoresGreaterEight = scores.filter((score) => score.score > 79);
console.log(scoresGreaterEight);

const numbers = [1, 2, 3, 4, 5];
const sum = numbers.reduce((accum, curr) => accum + curr);

console.log(sum);

const names = ["Sudan", "Rupen", "Mukunda", "Prince"];
const bools = [true, true, true, true];
const result = bools.every((b) => {
    return b === true;
});

console.log(result);

const checkType = names.every((name) => typeof name === "string");

console.log(checkDataTypes);

const bools = [true, true, true, true];
const result = bools.some((b) => {
    return b === true;
});
console.log(result);
const checkType = names.some((name) => typeof name === "number");
console.log(checkDataTypes);

const ages = [24, 22, 25, 32, 35, 18];
const result = names.find((name) => name.length > 7);
console.log(result);
const age = ages.find((age) => age < 20);
console.log(age);

const ages = [24, 22, 25, 32, 35, 18];
const result = names.findIndex((name) => name.length > 7);
console.log(result);

const age = ages.findIndex((age) => age < 20);
console.log(age);

const products = ["Milk", "Coffee", "Sugar", "Honey", "Apple", "Carrot"];
console.log(products.sort());

const numbers = [9.81, 3.14, 100, 37];
// sort method to sort number items provides wrong result.
// sort sorts strings not numbers
console.log(numbers.sort()); // [100, 3.14, 37, 9.81]

numbers.sort((a, b) => a - b); // Ascending order
numbers.sort((a, b) => b - a); // Descending order

// Splice: modifies the original array by adding or removing elements.
const numbers = [10, 20, 30, 40, 50];
numbers.splice(1, 2); // Removes 20 & 30
console.log(numbers); // [10, 40, 50]

numbers.splice(1, 0, 25, 35);
console.log(numbers); // [10, 25, 35, 40, 50]

// Slice: returns a portion of an array without modifying it.
const numbers = [10, 20, 30, 40, 50];
console.log(numbers.slice(1, 3)); // [20, 30] (excludes index 3)
console.log(numbers.slice(-2)); // [40, 50] (last two elements)

// object sorting
objArr.sort(function (a, b) {
    if (a.key < b.key) return -1;
    if (a.key > b.key) return 1;
    return 0;
});

const users = [
    { name: "abc", age: 150 },
    { name: "def", age: 50 },
    { name: "ghi", age: 100 },
    { name: "jkl", age: 22 },
];
users.sort((a, b) => {
    if (a.age < b.age) return -1;
    if (a.age > b.age) return 1;
    return 0;
});

console.log(users); // sorted ascending
//[{…}, {…}, {…}, {…}]

// pass by val proof
function modify(x) {
    x = 10;
}
let ee = 5;
modify(ee);
console.log(ee);

// default params
const greetDefault = (name = "Guest") => `Hello, ${name}!`;
console.log(greetDefault(), greetDefault("Charlie"));

// arguments object: array-like object available within all non-arrow functions
// has indexed values and a .length property
// it does not have array methods like .map() or .forEach()
function argsExample() {
    return [...arguments].join(", ");
}
console.log(argsExample(1, 2, 3, 4));

// rest params: arbitary number of args.
const restParams = (...nums) => nums.reduce((sum, num) => sum + num, 0);
console.log(restParams(1, 2, 3, 4, 5));

// One of JavaScript's most powerful features is closures. If a function is
// defined inside another function, the inner function has access to all the
// outer function's variables, even after the outer function exits.
function sayHelloInFiveSeconds(name) {
    var prompt = "Hello, " + name + "!";
    // Inner functions are put in the local scope by default, as if they were
    // declared with `var`.
    function inner() {
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout is asynchronous, so the sayHelloInFiveSeconds function will
    // exit immediately, and setTimeout will call inner afterwards. However,
    // because inner is "closed over" sayHelloInFiveSeconds, inner still has
    // access to the `prompt` variable when it is finally called.
}
sayHelloInFiveSeconds("Adam"); // will open a popup with "Hello, Adam!" in 5s

// objects
const objLiteral = {
    name: "Sudan",
    age: 21,
    greet: function () {
        return `Hello, my name is ${this.name}`;
    },
};
console.log(objLiteral.greet());

// with object.create
const objPrototype = {
    type: "Prototype Object",
    describe: function () {
        return `This is a ${this.type}`;
    },
};
const objCreated = Object.create(objPrototype);
objCreated.type = "Custom Object";
console.log(objCreated.describe());

// constructor function & object creation
// use classes instead
function Personn(name, age) {
    this.name = name;
    this.age = age;
}
const person1 = new Personn("Bob", 30);
const person2 = new Personn("Eve", 22);
console.log(person1.name, person2.name);

// deleting a property from an object
const objToDelete = { prop1: "value1", prop2: "value2" };
delete objToDelete.prop1;
console.log(objToDelete);

// object containing a function, another object, and an array
const complexObj = {
    method: function () {
        return "This is a method inside an object";
    },
    nestedObj: {
        key: "Nested Value",
    },
    arr: [1, 2, 3],
};
console.log(complexObj.method());
console.log(complexObj.nestedObj.key);
console.log(complexObj.arr);

// getters and setters in obj
const user = {
    _name: "Default",
    get name() {
        return this._name;
    },
    set name(value) {
        if (value.length > 0) {
            this._name = value;
        }
    },
};
console.log(user.name);
user.name = "Sudan";
console.log(user.name);

// math objs
console.log("Math.PI:", Math.PI);
console.log("Math.E:", Math.E);
console.log("Math.sqrt(16):", Math.sqrt(16));
console.log("Math.floor(4.7):", Math.floor(4.7));
console.log("Math.pow(2,3):", Math.pow(2, 3));

// string objs
let str = "Hello, JavaScript!";
console.log("String length:", str.length);
console.log("Uppercase:", str.toUpperCase());
console.log("Substring:", str.substring(0, 5));
console.log("Replace:", str.replace("JavaScript", "World"));
console.log("Character at index 1:", str.charAt(1));

// number objs
let num = 42.567;
console.log("Fixed to 2 decimals:", num.toFixed(2));
console.log("Converted to string:", num.toString());
console.log("Value of Number.MAX_VALUE:", Number.MAX_VALUE);
console.log("Parsing integer:", Number.parseInt("100px"));
console.log("Parsing float:", Number.parseFloat("12.34abc"));

// Obj Objs
let obj = { key: "value", count: 5 };
console.log("Object keys:", Object.keys(obj));
console.log("Object values:", Object.values(obj));
console.log("Object entries:", Object.entries(obj));
console.log("Object has property 'count':", obj.hasOwnProperty("count"));
console.log("Merging objects:", Object.assign({}, obj, { newKey: "newValue" }));

// array objs
let arr = [10, 20, 30, 40, 50];
console.log("Array length:", arr.length);
console.log("Array first element:", arr[0]);
console.log("Array last element:", arr[arr.length - 1]);
console.log("Array joined:", arr.join(" - "));
console.log("Array sliced:", arr.slice(1, 4));

// destructuring object
const rectangle = {
    width: 20,
    height: 10,
    area: 200,
};
let { width, height, area, perimeter } = rectangle;
console.log(width, height, area, perimeter);

const rectangle = {
    width: 20,
    height: 10,
    area: 200,
};
let { width: w, heigh: h, area: a, perimeter: p } = rectangle;
console.log(w, h, a, p);

const rectangle = {
    width: 20,
    height: 10,
    area: 200,
};
let { width, heigh, area, perimeter = 60 } = rectangle;
console.log(width, height, area, perimeter); //20 10 200 60

//Lets modify the object:width to 30 and perimeter to 80
const rectangle = {
    width: 30,
    height: 10,
    area: 200,
    perimeter: 80,
};
let { width, heigh, area, perimeter = 60 } = rectangle;
console.log(width, height, area, perimeter); //20 10 200 80

// Without destructuring
const rect = {
    width: 20,
    height: 10,
};
const calculatePerimeter = (rectangle) => {
    return 2 * (rectangle.width + rectangle.height);
};
console.log(calculatePerimeter(rect)); // 60
//with destructuring

const calculatePerimeter = ({ width, height }) => {
    return 2 * (width + height);
};

console.log(calculatePerimeter(rect)); // 60

//Another Example
const person = {
    firstName: "Asabeneh",
    lastName: "Yetayeh",
    age: 200,
    country: "Finland",
    job: "Instructor and Developer",
    skills: [
        "HTML",
        "CSS",
        "JavaScript",
        "React",
        "Redux",
        "Node",
        "MongoDB",
        "Python",
        "D3.js",
    ],
    languages: ["Amharic", "English", "Suomi(Finnish)"],
};
// Lets create a function which give information about the person object without destructuring

const getPersonInfo = (obj) => {
    const skills = obj.skills;
    const formattedSkills = skills.slice(0, -1).join(", ");
    const languages = obj.languages;
    const formattedLanguages = languages.slice(0, -1).join(", ");

    return `${obj.firstName} ${obj.lastName} lives in ${obj.country}. He is  ${obj.age
        } years old. He is an ${obj.job}. He teaches ${formattedSkills} and ${skills[skills.length - 1]
        }. He speaks ${formattedLanguages} and a little bit of ${languages[2]}.`;
};
console.log(getPersonInfo(person));
// Lets create a function which give information about the person object with destructuring

const getPersonInfo = ({
    firstName,
    lastName,
    age,
    country,
    job,
    skills,
    languages,
}) => {
    const formattedSkills = skills.slice(0, -1).join(", ");
    const formattedLanguages = languages.slice(0, -1).join(", ");

    return `${firstName} ${lastName} lives in ${country}. He is ${age} years old. He is an ${job}. He teaches ${formattedSkills} and ${skills[skills.length - 1]
        }. He speaks ${formattedLanguages} and a little bit of ${languages[2]}.`;
};
console.log(getPersonInfo(person));
/*
Asabeneh Yetayeh lives in Finland. He is  200 years old. He is an Instructor and Developer. He teaches HTML, CSS, JavaScript, React, Redux, Node, MongoDB, Python and D3.js. He speaks Amharic, English and a little bit of Suomi(Finnish)
*/

/**
 * event handling in JavaScript
 *
 */

// inline calling with onClick="" property

// dom select + .onclick
const traditionalButton = document.getElementById("traditional-btn");
traditionalButton.onclick = function () {
    alert("Traditional event handler clicked!");
};

// dom select + addEventListener()
const modernButton = document.getElementById("modern-btn");
modernButton.addEventListener("click", function () {
    alert("Modern event handler clicked!");
});

// mouse events
const mouseBox = document.getElementById("mouse-box");
if (mouseBox) {
    mouseBox.addEventListener("click", () => alert("Mouse Clicked!"));
    mouseBox.addEventListener("mouseover", () => console.log("Mouse Over"));
    mouseBox.addEventListener("mouseout", () => console.log("Mouse Out"));
    mouseBox.addEventListener("mousedown", () => console.log("Mouse Down"));
    mouseBox.addEventListener("mouseup", () => console.log("Mouse Up"));
    mouseBox.addEventListener("mousemove", () => console.log("Mouse Move"));
}
// keyboard events
document.addEventListener("keydown", (e) => console.log("Key Down:", e.key));
document.addEventListener("keyup", (e) => console.log("Key Up:", e.key));
// form events
const inputField = document.getElementById("input-field");
if (inputField) {
    inputField.addEventListener("focus", () => console.log("Input Focused"));
    inputField.addEventListener("blur", () => console.log("Input Blurred"));
    inputField.addEventListener("change", () => console.log("Input Changed"));
}
// form submission event blocking
const form = document.getElementById("form");
if (form) {
    form.addEventListener("submit", (e) => {
        e.preventDefault();
        alert("Form Submitted!");
    });
}
// window events
["load", "unload", "resize"].forEach((event) =>
    window.addEventListener(event, () =>
        console.log(`Window ${event.charAt(0).toUpperCase() + event.slice(1)}`),
    ),
);
// input event
document
    .getElementById("text-input")
    ?.addEventListener("input", (e) =>
        console.log("Input Event:", e.target.value),
    );

// window properties
alert(
    `Inner: ${window.innerWidth}x${window.innerHeight}, Outer: ${window.outerWidth}x${window.outerHeight}`,
);
let popup = window.open(
    "https://www.example.com",
    "_blank",
    "width=400,height=300",
);
if (popup) {
    popup.close();
}
alert(
    `URL: ${location.href}\nHost: ${location.host}\nPath: ${location.pathname}\nProtocol: ${location.protocol}`,
);
console.log(`History Length: ${history.length}`);
alert(
    `Browser: ${navigator.appName}\nVersion: ${navigator.appVersion}\nLanguage: ${navigator.language}\nOnline: ${navigator.onLine}`,
);
const frame = document.getElementById("myFrame");
alert(`Frame Source: ${frame.src}`);

// setTimeout runs the function once after the specified delay milliseconds.
let timeoutID = setTimeout(() => alert("Timeout executed!"), 2000);

// setInterval repeatedly runs the function every delay milliseconds until
// stopped with clearInterval().
let intervalID = setInterval(() => console.log("Interval Running..."), 1000);

setTimeout(() => {
    clearInterval(intervalID);
    console.log("Interval stopped!");
}, 5000);

// radios with colors in values='' used to change bg-color
document.addEventListener("DOMContentLoaded", function () {
    const radios = document.querySelectorAll("input[name='color']");
    radios.forEach((radio) => {
        radio.addEventListener("change", function () {
            document.body.style.backgroundColor = this.value;
        });
    });
});

let paragraph = document.querySelectorAll("p");
let properties = `
  nodeName: ${paragraph.nodeName} <br>
  nodeType: ${paragraph.nodeType} <br>
  lastChild: ${paragraph.lastChild.nodeValue} <br>
  nodeValue: ${paragraph.firstChild.nodeValue} <br>
  parentNode: ${paragraph.parentNode.nodeName} <br>
  nextSibling: ${paragraph.nextSibling} <br>
  previousSibling: ${paragraph.previousSibling} <br>
  childNodes count: ${paragraph.childNodes.length} <br>
  firstChild: ${paragraph.firstChild.nodeValue} <br>
`;

const regex2 = /\d{3}-\d{2}-\d{4}/g;
const testtString = "My SSN is 123-45-6789 and my friend's SSN is 987-65-4321.";

const execResult = regex2.exec(testtString);
console.log("exec() result:", execResult);
const testResult = regex2.test(testtString);
console.log("test() result:", testResult);

// && and || "short circuit", which is useful for setting default values.
let namees = otherName || "default";

// One of JavaScript's most powerful features is closures. If a function is
// defined inside another function, the inner function has access to all the
// outer function's variables, even after the outer function exits.
function sayHelloInFiveSeconds(name) {
    var prompt = "Hello, " + name + "!";
    // Inner functions are put in the local scope by default, as if they were
    // declared with `var`.
    function inner() {
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout is asynchronous, so the sayHelloInFiveSeconds function will
    // exit immediately, and setTimeout will call inner afterwards. However,
    // because inner is "closed over" sayHelloInFiveSeconds, inner still has
    // access to the `prompt` variable when it is finally called.
}
sayHelloInFiveSeconds("Adam"); // will open a popup with "Hello, Adam!" in 5s

// We can also specify a context for a function to execute in when we invoke it
// using `call` or `apply`.
var anotherFunc = function (s) {
    return this.myString + s;
};
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

// The `apply` function is nearly identical, but takes an array for an argument
// list.
anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

// This is useful when working with a function that accepts a sequence of
// arguments and you want to pass an array.
Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// But, `call` and `apply` are only temporary. When we want it to stick, we can
// use `bind`.

var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

// `bind` can also be used to partially apply (curry) a function.
var product = function (a, b) {
    return a * b;
};
var doubler = product.bind(this, 2);
doubler(8); // = 16

// Unlike most other popular object-oriented languages, JavaScript has no
// concept of 'instances' created from 'class' blueprints; instead, JavaScript
// combines instantiation and inheritance into a single concept: a 'prototype'.

// Every JavaScript object has a 'prototype'. When you go to access a property
// on an object that doesn't exist on the actual object, the interpreter will
// look at its prototype.

// Some JS implementations let you access an object's prototype on the magic
// property `__proto__`. While this is useful for explaining prototypes it's not
// part of the standard; we'll get to standard ways of using prototypes later.
var myObj = {
    myString: "Hello world!",
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function () {
        return this.myString.toLowerCase();
    },
};

myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// This works for functions, too.
myObj.myFunc(); // = "hello world!"

// Of course, if your property isn't on your prototype, the prototype's
// prototype is searched, and so on.
myPrototype.__proto__ = {
    myBoolean: true,
};
myObj.myBoolean; // = true

// There's no copying involved here; each object stores a reference to its
// prototype. This means we can alter the prototype and our changes will be
// reflected everywhere.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// The for/in statement allows iteration over properties of an object,
// walking up the prototype chain until it sees a null prototype.
for (var x in myObj) {
    console.log(myObj[x]);
}
///prints:
// Hello world!
// 43
// [Function: myFunc]
// true

// To only consider properties attached to the object itself
// and not its prototypes, use the `hasOwnProperty()` check.
for (var x in myObj) {
    if (myObj.hasOwnProperty(x)) {
        console.log(myObj[x]);
    }
}
///prints:
// Hello world!

// We mentioned that `__proto__` was non-standard, and there's no standard way to
// change the prototype of an existing object. However, there are two ways to
// create a new object with a given prototype.

// The first is Object.create, which is a recent addition to JS, and therefore
// not available in all implementations yet.
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// The second way, which works anywhere, has to do with constructors.
// Constructors have a property called prototype. This is *not* the prototype of
// the constructor function itself; instead, it's the prototype that new objects
// are given when they're created with that constructor and the new keyword.
MyConstructor.prototype = {
    myNumber: 5,
    getMyNumber: function () {
        return this.myNumber;
    },
};
var myNewObj2 = new MyConstructor();
myNewObj2.getMyNumber(); // = 5
myNewObj2.myNumber = 6;
myNewObj2.getMyNumber(); // = 6

// Built-in types like strings and numbers also have constructors that create
// equivalent wrapper objects.
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// Except, they aren't exactly equivalent.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0) {
    // This code won't execute, because 0 is falsy.
}
if (new Number(0)) {
    // This code will execute, because wrapped numbers are objects, and objects
    // are always truthy.
}

// However, the wrapper objects and the regular builtins share a prototype, so
// you can actually add functionality to a string, for instance.
String.prototype.firstCharacter = function () {
    return this.charAt(0);
};
"abc".firstCharacter(); // = "a"

// This fact is often used in "polyfilling", which is implementing newer
// features of JavaScript in an older subset of JavaScript, so that they can be
// used in older environments such as outdated browsers.

// For instance, we mentioned that Object.create isn't yet available in all
// implementations, but we can still use it with this polyfill:
if (Object.create === undefined) {
    // don't overwrite it if it exists
    Object.create = function (proto) {
        // make a temporary constructor with the right prototype
        var Constructor = function () { };
        Constructor.prototype = proto;
        // then use it to create a new, appropriately-prototyped object
        return new Constructor();
    };
}
