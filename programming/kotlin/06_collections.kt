fun main() {
    // Kotlin collections are broadly categories into two different forms. These are:

    // * Immutable Collection (or Collection) -> Immutable collection also called Collection
    // supports read only functionalities
    // * Mutable Collection -> Mutable collections supports both read and write functionalities

    // list are ordered
    // sets are collections of unique elements
    // maps are key-value pairs

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // Immutable
    // ----------------------------------------------

    // List ->  listOf() , listOf<T>()
    var listCollec = listOf("Abc", "Def", "Ghi", "Jkl")
    for (i in listCollec) {
        println(i)
    }
    // functions include contains, containsAll, get(index), indexOf(element),
    // lastIndexOf(element): last occurance returned, isEmpty, subList(start, end), etc.file

    // Map -> mapOf() or mapOf<k,v>().
    // Key-Value pair. The key and value may be of different pairs such as <Int, Int>,<Int,
    // String>, <Char, String>, etc.
    var myMap = mapOf<Int, String>(1 to "Oggy", 2 to "Alfred", 3 to "perman")
    for (e in myMap) {
        println("key: ${e.key} & value: ${e.value}")
    }
    for (e in myMap.keys) {
        println("key is : $e and value is : ${myMap[e]}")
    }
    // Functions include getValue(), contains, containsKey, containsValue, getOrDefault, minus:
    // prints all except parameter, etc
    var iterator = myMap.iterator()
    while (iterator.hasNext()) {
        val pair = iterator.next()
        println("key : ${pair.key} and value : ${pair.value}")
    }

    // read only
    // read-only does not guarantee immutability.
    // The underlying object might still be mutable if referenced elsewhere.
    val list: List<Int> = listOf(0, 2, 3)
    val immutableList = listOf("Hello", "How", "are")

    val name: String = immutaleList[-1] // access by index
    valMutableList[0] = "Bart" // update item in list
    // immutableList[0] = "Bart" // Error: can't change

    // mutable
    val valMutableList = mutableListOf("Name2", "Name4")
    var varMutableList = mutableListOf("Name4", "Name6")
    list.add("name 0")
    list.removeAt(0)
    // backed by an ArrayList off the JVM in most cases.
    // You can also explicitly use:
    val list = ArrayList<Int>()

    // one way to test membership
    val isBobThere0 = "Bob" in immutableList
    // another way to test membership
    val isBobThere1 = immutableList.contains("Name")


    valMutableList.add(1, "NameName") // add item at index
    // delete by index
    val removedPerson = valMutableList.removeAt(0)
    // delete by value
    val wasRemoved = valMutableList.remove("Bart")
    // you can change the contents of a val mutable collection, but you can't reassign it
    // you can change the contents of a var mutable collection, and you can reassign it
    varMutableList[-1] = "mellon"
    varMutableList = mutableListOf("Emma", "Hari")

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // Set
    // order not guaranted
    // sets ignore duplicate items, so immutableSet has 1 items: "chocolate" and "vanilla"
    val immutableSet = setOf<String>("chocolate", "vanilla", "chocolate")
    val set = setOf(0, 2, 3, 3) // duplicates ignored
    val mutableSet = mutableSetOf("butterscotch", "strawberry")
    // one way to test membership
    val hasChocolate0 = "chocolate" in immutableSet
    // another way to test membership
    val hasChocolate1 = immutableSet.contains("chocolate")
    mutableSet.add("green tea") // add item
    // delete by value
    val flavorWasRemoved = mutableSet.remove("strawberry")
    // HashSet is the common implementation:
    val sett = HashSet<Int>()

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // MAP
    val immutableMap = mapOf("name" to "meow", "rank" to "major")
    val mutableMap = mutableMapOf("name" to "picard", "rank" to "major")
    // is this key in the map?
    val hasRankKey = immutableMap.containsKey("rank")
    // is this value in the map?
    val hasMeowValue = immutableMap.containsValue("meow")

    // access by key, returns nullable
    val name: String? = immutableMap["name"]
    // update value for key
    mutableMap["name"] = "sudan"
    // add new key and value
    mutableMap["ship"] = "voyager"
    mutableMap.remove("rank") // delete by key
    // delete by key and value
    mutableMap.remove("ship", "voyager")
    // wont work, value doesn't match
    mutableMap.remove("name", "Spock")
    val map = HashMap<String, Int>()

    // Additional concepts:
    // Anything that implements Iterable<T> can be used in a for loop.
    val list = listOf(1, 2, 3)
    for (x in list) {
        println(x)
    }

    // this translates to
    val iterator = list.iterator()
    while (iterator.hasNext()) {
        val x = iterator.next()
    }
    // Arrays are a special case.
    // The compiler often optimizes them into index-based loops to avoid iterator allocation.

    // Set -> setOf()

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // Mutable
    // ----------------------------------------------

    // List	->  ArrayList<T>() , arrayListOf() , mutableListOf()
    // Similar to listOf, with add(), clear, remove, removeAll, removeAt, set, subList.
    // Map ->   HashMap , hashMapOf() , mutableMapOf()
    // Muteable map with put(), get(), containsKey(), containsValue(), clear(), remove(). Can be
    // constucted with capacity and load factor.
    // Set ->   hashSetOf() , mutableSetOf()

    val immutableList = listOf("Name", "Name2")
    val list = listOf(1, 2, 3)
    val doubled = list.map { it * 2 }

    val numbers = listOf(1, 2, 3, 4, 5)
    val even = numbers.filter { it % 2 == 0 }

    data class Order(val id: Int, val amount: Double)
    val orders = listOf(
        Order(1, 120.0),
        Order(2, 80.0),
        Order(3, 200.0),
        Order(4, 50.0)
    )
    val largeOrders = orders.filter { it.amount >= 100 }

    // fold(initial) { accumulator, element -> newAccumulator }
    val total = orders.fold(0.0) { acc, order ->
        acc + order.amount
    }
    val summary = orders.fold("Orders:\n") { acc, order ->
        acc + "Id: ${order.id}, Amount: ${order.amount}\n"
    }

    // ---
    val totalLarge = orders
        .filter { it.amount >= 100 }
        .fold(0.0) { acc, order -> acc + order.amount }

//.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-.

    // Scope Functions
    // Scope functions are unique functions in programming that allow you to
    // interact with an object without directly referencing its name. There
    // are 5 function who call `let`, `run`, `with`, `apply`, and `also`. In
    // simpler words standard library functions that execute a block of code
    // within the context of an object.

    data class User(var name: String, var age: Int, var email: String?)

    // let: accesses the object as 'it', returns the block's result. useful for
    //      null checks.
    val length = "Hello".let {
        it.length
    }

    val name: String? = "Sudan"
    name?.let { // safely access name
        println(it.length) // the name becomes 'it'. and we access it's length
    }

    val emailDomain = user.email?.let {
        it.substringAfter("@")
    }
    val nameLength = user.let {
        it.name.length
    }

    // run: accesses the object as 'this', returns the block. Good for
    //      initialization. useful for when you want to compute something from an
    //      object.
    val res = "hello".run {
        length
    }
    val isAdult = user.run {
        age >= 18
    }
    val summary = user.run {
        "$name is $age years old"
    }
    val mes = StringBuilder().apply {
        append("Hello, ")
        append("Kotlin!")
        toString()
    }

    // with: accesses the object as 'this', returns the block's result. used for
    //       grouping calls. useful for you have an object and want to operate on
    //       it
    val result = with("hello") {
        length
    }
    val description = with(user) {
        "Name: $name, Age: $age"
    }

    class Config {
        var maxRetries = 0
        var timeout = 0
    }

    val config = Config
    val res = with(config) {
        maxRetries = 3
        timeout = 500
        "configured with $maxRetries"

        // here we set up the config and returned a string in single block.
    }

    // apply: accesses the object as 'this', returns the object itself, ideal for
    //        configuration. mostly used for object config.
    val user = User("", 0, null).apply {
        name = "Sudan"
        age = 1
        email = "s@example.com"
    }

    // also: accesses the object as 'it', returns the object itself, perfect for
    //       side-effect and logging.
    val number = 5.also {
        println("Value is $it")
    }
    val nums = mutableListOf(1, 2, 3, 4).also {
        println("$it")
        it.add(5)
    }
    // logging something while keeping the same object flowing through.
    val savedUser = user.also {
        println("Saving user: $it")
    }.also {
        repository.save(it)
    }
}

// PREDICATES
fun main(args: Array<String>) {
    val myNumbers = listOf(2, 3, 4, 6, 23, 90)
    val myPredicate = { num: Int -> num > 10 }
    val check1 = myNumbers.all( myPredicate ) // Are all elements greater than 10?
    println(check1)
    val check2 = myNumbers.any(myPredicate) // Does any of these elements satisfy the predicate?
    println(check2)
    val totalCount: Int = myNumbers.count(myPredicate) // Number of elements that satify the predicate.
    println(totalCount)
    val num = myNumbers.find(myPredicate) // Returns the first number that matches the predicate
    println(num)
}
