// Official Documentation: <https://kotlinlang.org/docs/home.html>
// Do look at this too: <https://kotlinlang.org/docs/idioms.html>
// Kotlin in Action: <https://www.manning.com/books/kotlin-in-action>

/*
Kotlin

- is statically typed programming language
- runs on the Java Virtual Machine (JVM)
- can be compiled to JavaScript or native code too

- is designed to be
    - concise,
    - expressive,
    - safe.

---------------------------------------------------------------------

1_variables: Variables, data types, string (properties, equality, templates), nullable types, types(inference, conversion)
2_branching_loops: if-else, when, ranges, loops, jump statements
3_array: array, boxed array vs primitive type array.
4_functions: functions, expressions, higher order functions, infix, tailrec, lambda, closure, it
5_exceptions: try catch, exceptions
6_collections: list, mutability & references, map, set, hashmap, hashset, filter, map, predicates
7_oop: class, constructors, access modifiers, instance, open, interface, inheritance, getter, setter, property, hold, init block, companion objects, objects
8_coroutines: threads, scope, runBlocking, deferred, job, suspend functions, launch
9_misc:

*/

// REST: Representational State Transfer


// `lateinit`:

// Sometimes you need to declare a non-null property but initialize it later. The lateinit keyword can help with this:

class MyViewModel() {
    private lateinit var dataRepository: DataRepository

    fun initialize(repo: DataRepository) {
        this.dataRepository = repo
    }

    fun loadData() {
        // Using dataRepository safely after initialization
        if (::dataRepository.isInitialized) {
            dataRepository.fetchData()
        } else {
            throw IllegalStateException("DataRepository not initialized")
        }
    }
}

// you can also use let chains to invert the order of composition, to reduce nesting.
// so instead of three(two(one)) you can write one.let(two).let(three)
// let turns parameter functions into receiver functions :)

// let returns block result, and objects are referenced with it

val maybePerson? = TODO()

maybePerson?.let {
    sendMail(it.email)
}

// ---

some.deep.nested.attributes.let { }

// or

some.deep.nested.attributes.let { myVar -> }


// also returns the original object, and objects are referenced with it

// useful for intermediate side-effects.

myList.filter { TODO() }
    .map { TODO() }
    .also { log.info { "this is being modified" } }
    .sum()

// from
val res = myFunc()
log.info { "res: $res" }
return res

// to
return myFunc().also { log.info { "res: $it" } }

// apply returns the original object, and objects are shadowed by this.

// for initializing when constructor does satisfy your needs or when you only
// have access to setter methods.

rect = Rectangle()
rect.x = 5
rect.y = 3
rect.w = 10
rect.h = 20

// more concise
rect = Rectangle().apply {
    x = 5
    y = 3
    w = 10
    h = 20
}


con = DbConnection().apply {
    setHost(hostname)
    setPort(port)
    setUser(user)
    setPassword(password)
}

// run returns the function result, and references the object by this

// helpful when you want to configure and return the result

builtConfig = Config().run {
    setUser(user)
    setPassword(password)
    build() // gets returned
}

// you can also use with for the same effect but object as the receiver argument

with(Config) {
    TODO
}

// you can also use `run {}` to open a new scope.

run {

}

// However, the scope functions can also be a anti-pattern. You can make statements
// directly rather than using also. you can also generally simplify your code
// much better by just making objects not-null, using builder pattern when available,

---

// Delegation:

class DatabaseConnection {
    val connection: String by lazy {
        println("Connecting to database...")

        "Connected to PostgreSQL"
    }
}

val db = DatabaseConnection()
println("Object created")

println(db.connection) // triggers lazy init
println(db.connection) // uses cached value
// output:
    // Object created
    // Connecting... <==== only once.
    // Connected...
    // Connected...

// `by lazy` is thread safe. It uses synchronization so that only one thread computes it even tho multiple access is provided.

// If you know asynchronous access will not happen, you can set the following parameter.

// thread safe
val connection: String by lazy {
    "connected"
}

// single thread: fast but no synchronization
val fastLazy: String by lazy(LazyThreadSafetyMode.NONE) {
    "Computed value"
}

// use case:
// - database connections or expensive repeated computations
// - properties that might never be accessed
// - singleton-style initialization

// Observable:
// fires a callback when properties change.

// perfect for logging.

import kotlin.properties.Delegates

class UserSettings {
    var theme: String by Delegates.observable("Light") { _, old, new ->
        println("Theme changed: $old -> $new")
    }

    var fontSize: Int by Delegates.observable(14) { _, old, new ->
        println("Font size changed: $old -> $new")
    }
}

val settings = UserSettings()
settings.theme = "Dark" // theme changed is printed
settings.fontSize = 18 // font size changed is printed

// Vetoable:

// fires a callback right before value is updated. return true to accept, false
// to reject. Perfect for validation on properties. The false rejects new
// assignment and keep old value but its silent. The application will not crash.


class Player {
    var health: Int by Delegates.vetoable(100) { _, _, new ->
        new >= 0 // reject negative
    }
    var score: Int by Delegates.vetoable(0) { _, old, new ->
        new >= old // score can only increase
    }
}

val pla = Player()
pla.health = 80 // accepted
pla.health = -10 // rejected. value remains 80
pla.score = 100 // accepted -> 100
pla.score = 50 // rejected still 100
