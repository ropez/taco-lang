# Taco — language tour and reference

Taco is a deliberately small, statically typed scripting language designed for clarity, predictability, and safety. It targets single-file scripts and small embedded scenarios where the minimal, well-typed runtime surface is an advantage. Arguments are always passed by value (copy-on-write), variables are effectively immutable (assignment shadows previous bindings), and mutation is only available using explicit mutable primitives. The language is intended for scripting tasks such as file manipulation, API calls, and automation while avoiding common pitfalls like shared mutable state, use-after-free, and aliasing bugs.

This document is a concise language tour and reference. It explains the most important language concepts, illustrates them with realistic examples, and documents the validation rules the interpreter enforces.

## Quick start

A Taco script is a single file you run with the interpreter. The most basic "Hello world" program is:

```taco
println("Hello, world")
```

Execution is top-to-bottom: you declare functions, records, and unions, and then call them. The interpreter validates types statically before running the script and reports type errors such as passing wrong argument types, attempts to use optional values where plain values are required, and other problems.

## Lexical basics and blocks

Taco's syntax is intentionally small and consistent. Code blocks are always enclosed in curly braces (`{ ... }`), no exceptions. Strings use double quotes and always support interpolation with `$name` or `${expr}`. Lines are the primary separation between statements and tokens such as `fun`, `rec`, `union`, `if`, `for`, `while`, and `return` are recognized.

Comments are single-line and start with `#`. The language also supports multi-line strings. Trailing punctuation like semicolon after statement or comma after multi-line list items is not allowed.

Example:

```taco
# This is a comment
message = "This is a message"

println("${message}") # trailing comment

list = [
    "foo"
    "bar"
]
```

## Primitive types

Taco exposes a small set of primitives that can be combined through composition:

- `int` - integer numbers
- `str` - unicode strings (literals use double quotes)
- `bool` - booleans (`true` / `false`)
- `char` - single unicode character (literals use single quotes)

Composite types include:

- Lists: `[T]` - homogeneous lists of `T`
- Tuples: `(T1, T2, ...)` - fixed-size aggregates
- Optionals/Fallible: `T?` - optional or fallible variants (a `T` or nothing / error)
- Records and unions: custom structured and tagged types

Type annotations are required in function signatures and record/union definitions. Local variable types are normally inferred from their value.

## Variables, immutability and shadowing

Taco bindings are immutable in the sense that a new assignment creates a new binding that shadows previous ones within the current scope. This shadowing model prevents aliasing and subtle shared-state bugs because values are passed by value (copy-on-write semantics).

Example showing shadowing and scope:

```taco
a = "outer"
if true {
    println(a)   # prints "outer"
    a = "inner"  # shadows 'a' in the inner scope
    println(a)   # prints "inner"
}
println(a)       # prints "outer" - the outer binding is unchanged
```

Because rebinding does not mutate the previous binding outside the active scope, the model is simple to reason about and avoids many classes of bugs.

For cases where truly mutable state is needed, Taco provides a `State(...)` primitive instead of general-purpose mutation (see the "State" section).

## Functions and return semantics

Functions are declared with `fun`. Parameter names and types are required; a return type may be provided. If a function declares a return type, every code path must produce a compatible value; otherwise the validator will raise an error.

Functions may use explicit `return` statements or the return value can be implied if the function body contains exactly one expression.

```taco
fun greet(name: str) {
    println("Hello, $name")
}

fun square(n: int): int {
    return n * n
}

fun identity(n: int) {
    n  # implied return value
}
```

Unlike other languages (e.g. Rust), the implicit return feature is restricted to functions having only one statement, which must be an expression. In all other cases, the `return` keyword is required.

When calling a function, arguments can be passed positionally or named:

```taco
greet("Taco")
greet(name: "Taco")
```

Using named arguments is useful when passing many arguments, or when using optional arguments, because they can be given in any order.

```taco
fun register(name: str, email: str, age: int?, occupation: str?) { ... }

register("Fred Flintstone", "fred@flintstone.net")

register(
  name: "Fred Flintstone"
  email: "fred@flintstone.net"
  occupation: "Caveman"
  age: 10000
)
```

**Note**: When arguments are placed on separate lines, there are no trailing commas. The line break is the separator.

## Tuples

Tuples are a core feature of the Taco language. They are the basic building block that is used for many things such as `rec`, `union` and function arguments. All syntax related to tuples use parentheses, and parentheses are *always* somehow related to tuples (because function arguments are also tuples).

In it's simple form, literal tuples can be written with parentheses and contain a sequence of values.

```taco
pair = (4, 5)
response = (404, "Not found")
```

Of course values can be any expression, including other tuples:

```taco
tuple = (a, add(3, 4), ("foo", "bar"))
```

Tuple items can optionally be named:

```taco
point = (x: 100, y: 200)

assert point.x == 100
assert point.y == 200
```

Tuples can be "destructed" into their inner values:

```taco
point = (100, 200)

(x, y) = point

assert x == 100
assert y == 200
```

In this simple case, the arguments are destructed positionally in the order they are given, and assigned to the local variables `x` and `y` respectively. There are specific rules for what happens when destructing tuples with named values. In fact, this is *exactly* equivalent to passing arguments to a function (the left side of the destructing assignment acts as the formal function arguments, and the right side as the given call arguments). This is documented in the next section.

**Note**: To keep the syntax clear and consistent, parentheses are *not* used in arithmetic expressions. Curly braces are used instead, e.g. `{ a + b } * c`.

## Tuples and functions

Function arguments are tuples, so everything that is true for function arguments is also true for tuples, and vice versa. Arguments can be named or unnamed, they can be optional, and they can be nested tuples. There are specific rules that come into play when evaluating tuples as arguments. These rules are the same for all function calls, because the arguments to any function is just a tuple.

Consider these examples to illustrate how function arguments and tuples are the same thing:

```taco
fun foo(x: int, y: int) { ... }
foo(3, 4)

fun bar(a: (x: int, y: int)) { ... }
bar((3, 4))

a = (3, 4)
(x, y) = a
```

The same basic rules apply to all three examples.

There is a special keyword, `arguments` that can be used inside any function to access the arguments as a tuple:

```
fun foo(int, int) {
    (x, y) = arguments
    assert x == arguments.x
    assert y == arguments.y
}
```

The opposite of this is also possible. If you have a tuple, and you want to call a function using that as arguments, you can use the special "splat" syntax:

```
fun foo(x: int, y: int) { ... }

args = (100, 200)
foo(=args)
```

Finally, if you want to pass the special tuple `arguments` to another function, there is a shortcut for this:

```
fun foo(x: int, y: int) { ... }

fun bar(x: int, y: int) {
    foo(=)            # shortcut
    foo(=arguments)   # this is equivalent
}
```

In all cases, the rules for "applying" given arguments onto a formal argument list (whether this is the "bare" argument list of a function, a nested tuple argument, or other cases like `rec`, `union` and pattern matching which will be covered below) is as follows.

- Both formal and given arguments can contain any combination of named and unnamed arguments (although it's recommended to stick with simple uniform patterns)
- A *named* formal argument can be assigned from an *unnamed* or *named* given argument, but a *named* given argument is never assigned to an *unnamed* (or differently named) formal argument
- The formal argument list is processed left-to-right. For each *named* argument, if a given argument with the same name is found, that is used, otherwise the *next unnamed* (positional) argument is consumed. For unnamed formal arguments, the next positional argument is always consumed.

## Records (rec)

Records in Taco serve a similar purpose to "structs" in many other languages. They are the closest Taco has to "classes", but Taco is not an object oriented language. These is no inheritance or polymorphism.

Basically, records are simply pre-defined tuples. A named record can be thought of as an "alias" for a tuple with the given structure. Records are represented exactly like tuples, and can be used in all places where tuples are expected, such as function calls.

Records are defined with `rec`, then a name, then the structure of the record using the exact same syntax as a function argument list.

The name of the record definition becomes available in the scope, and can be used both as a formal type, and as a constructor function that creates a record. This constructor is *just a function*, and can be used anywhere a function is expected, including being passed as a callback to another function.

```taco
rec Person(name: str, age: int)

# constructor call
p = Person("Alice", 30)

# as function argument
fun register(person: Person) { ... }

# pass constructor as a callback
[("Fred", 40), ("Barney", 30)].map_to(Person)
```

Of course, records can also contain other records:

```
rec Name(first: str, last: str)
rec Person(
    name: Name
    # ...
)
```

Records are, like all other values in Taco, immutable. However, they have a build-in method named `with` that can be used to update values and create a new record of the same type. All arguments to this function are optional, and default to the original value.

```taco
# create a new record with an updated age
older = p.with(age: 31)
```

**PLANNED** Taco will have a way to define custom methods for records:

```taco
fun Person::older(self): Person {
    self.with(age: self.age + 1)
}
```

## Unions and pattern matching

Unions provide sum types (tagged unions). Each variant contains a **tuple** of arbitrary values. Their syntax and semantics are similar to Rust's enums. Use `match` to examine union values. Or variations must be handled, or the validator will generate an error.

```taco
rec City(name: str, country: str)

union VacationPlan {
    DoNothing
    TravelTo(City)
    PlayGame(name: str)
}

fun describe(plan: VacationPlan): str {
    match plan {
        VacationPlan::DoNothing => "Do nothing"
        VacationPlan::TravelTo(city) => "Travel to ${city.name} in ${city.country}"
        VacationPlan::PlayGame(name) => "Play a game: $name"
    }
}
```

## Lists and iteration

Taco has a built-in list type for sequential data of arbitrary length.

List literals use square brackets. And iteration over lists is done with the `for ... in` syntax.

```taco
fruits = [
    "apple"
    "orange"
    "banana"
]

for f in fruits {
    println("Fruit: $f")
}
```

Lists are like all other types in Taco immutable. Methods that change the content of a list, such as `push`, doesn't change the original list, but returns a new list. The interpreter is smart enough to avoid copying in situations when this is possible (i.e. if the original list is never used again).

There are many built-in methods defined for lists:

- `len()` - Returns the number of elements
- `push(T)` - Add an item to the end (creating a new list)
- `at(int)` - Return the item at the given position
- `take(int)` - Return the first N items from the list as a new list
- `skip(int)` - Return the items from position N as a new list
- `find(T)` - Find the element equal to T (or `None` if not found)
- `find_index(T)` - Find the index of the first element equal to T (or `None` if not found)
- `contains(T)` - Return true of the list contains an element equal to T
- `count(T)` - Returns how many elements are equal to T
- `sum()` - Returns the sum of a list of numbers (invalid for other list types)
- `max()` - Returns the highest number in the list
- `join(str)` - Makes one string from a list of strings
- `sort()` - Create a sorted list
- `map(fun)` - Produce a new list by call a function for every item in the list
- `map_to(fun)` - Similar to `map`, but *applies* list elements (which must be tuples) as arguments to the function
- `scan(fun)` - Similar to `map`, but maintains a "state" value that is passed between calls
- `any(fun)` - Return true if the given function returns true for any element
- `filter(fun)` - Produce a list containing all elements for which the function returns true
- `enumerate()` - Produce a list with each element together with it's index (useful for iteration)
- `flip()` - Flip the axes of a 2-dimensional list (list of lists)


## Optional type

There are no references or pointers in Taco, and no such thing as "null" in many other languages.

However, there is the concept of *optional* values that can be either *present* (some) or *absent* (none). This is very similar to the `Option` type in Rust, and similar concepts in other safe languages. Optional types are denoted with a question mark after the type (e.g. `str?`).

Optional values must always be checked before use. It's a validation error to try to use a value of type `str?` where a value of type `str` is expected. There is a special construct `if .. in ..` to check if an optional value is present. Example using the list method `.find()`, which returns an optional value:

```taco
fruits = [
    "apple"
    "orange"
    "banana"
]

if fruit in fruits.find("banana") {
    println("Found: $fruit")
} else {
    println("no banana")
}
```

It's also possible to use a match expression:

```taco
fruits = [
    "apple"
    "orange"
    "banana"
]

msg = match fruits.find("banana") {
    Some(fruit) { "Found: $fruit" }
    None { "no banana" }
}

println(msg)
```

Note that `match` is an *expression*, and each arm must be a single expression. Think of it as a generalization of the ternary operator (`.. ? .. : ..`) found in many other languages. It's not possible to have multiple statements inside an arm. That would require something like implicitly evaluating to the last statement, which not a language feature by design.

## Control flow

Taco supports the usual control flow constructs (`if`, `else`, `for`, `while`, `break`, `continue`), with semantics similar to what you find in most other languages.

There is no parentheses around the condition or loop parameters (parentheses are only used for tuples). Blocks are always delimited by `{}` and can contain arbitrary statements, including nested control flow.

Because of how variable binding works in Taco, variable assignments inside these blocks are not visible outside the block. This means that all control flow statements must either contain `return` statements, or have some kind of external side-effect, otherwise they are meaningless. The side-effect can be something as simple as making a call to `print`, or updating a state primitive, or things like network or file I/O.

We already saw the `for` loop in section about lists. This is the most basic loop construct. In addition to lists, the `for` loop also support numerical ranges:

```taco
for i in 1..10 {
    println("I have $i bananas")
}
```

If statements are similar to most other languages. The condition *must* have a boolean type (there is no such thing as the number 0 being false etc). The `else` block is optional. The syntax is identical to Rust, but unlike Rust `if` statements are *not* expressions, because they can have arbitrary statements:

```taco
if foo > 0 {
    println("do something")
} else {
    println("something else")
}
```

The while loop evaluates a boolean expression repeatedly (e.g. a function call), and executes a code block every time the expression evaluates to true. Note that since the block itself doesn't have any side-effects on the surrounding scope, there's no way for the result of evaluating the condition to change, and the loop to terminate, without changing some external state. In this case, updating a state primitive:

```taco
rest = State(10)
while rest.get() > 0 {
    rest.update(fun(r) { r - 1 })
    println("Remaining: ${rest.get()}")
}
```

Taco supports a special `if..in` construct that is used with the optional type. It evaluates an expression with an option type, and if the expression yields a value, binds it to a variable, and executes the block. Otherwise it executes an optional `else` block:

```taco
if value in list.find("foo") {
    println("Found $value")
} else {
    println("Not found")
}
```

There is also a `while..in` construct. Just like `while` is a repeating version of `if`, `while..in` is a repeating version of `if..in`.

```taco
while message in connection.read() {
    println("Received: $message")
}
```

**Note**: `while..in` might look very similar to `for..in`. They difference is that `for..in` evaluates the expression *once* (and expects some kind of iterable value), but `while..in` evaluates the expression every time the loop repeats (and expects an option value). The `while..in` statement also allows an expression with a non-optional type, which is equivalent to an eternal loop (`while true`). This will repeat until it hits a `break` or `return` statement, or until the script is terminated. There are many use-cases for this.

## Strings

Strings in Taco support the full Unicode character set. How they are represented is an implementation detail, but the implementation shall guarantee that the length (`len()`) of a string is exactly the number of Unicode characters, and indexes into a string, such as when calling `char_at()`, `substring()`, or `split()`, are Unicode character boundaries.

Literal strings support interpolation using `$name` for identifiers or `${expression}` for arbitrary expressions. There is no special syntax for enabling interpolation like in other languages. It's always enabled.

```taco
name = "Taco"
message = "$name has ${name.len()} letters"
```

A literal `$` can be inserted using double `"$$"`. Escape sequences are supported for things like newline: `"\r\n"`.

There is also a verbatim string format, that allows double quotes without having to use escape sequences, but enclosing the string with three double quotes:

```taco
json = """{"name": "Taco"}"""
```

## Question operator and coalescing

The `?` operator can be used in expressions to propagate absence or errors to the caller. When a value-producing expression has type `T?`, appending `?` attempts to unwrap the `T` and will cause the current function to return early with `None` (or propagate the fallible state) if the value is absent. This pattern avoids repetitive branching and keeps code concise.

```taco
available_fruits = [
    "apple"
    "orange"
    "banana"
]

fun try_buy(fruit: str, amount: int): str? {
    f = available_fruits.find(fruit)?   # returns early if not found
    return "$amount ${f}s"              # returns a string inside the option
}

if message in try_buy("banana", 3) {
    println(message)           # prints "3 bananas"
} else {
    println("no banana")
}
```

## State (typed, controlled mutation)

Taco avoids unstructured mutation in favor of a controlled `State(...)` primitive that provides typed mutable containers. A `State` value has `get()` and `set(...)` methods used to access the inner value, as well as `update(...)` for atomic updates.

```taco
counter = State(0)

fun increment() {
    v = counter.get()
    counter.set(v + 1)
}

assert counter.get() == 0
increment()
assert counter.get() == 1
```

Use `State` sparingly: it is the explicit escape hatch for mutation when your script must maintain shared or evolving state.

## Standard library overview

The goal of Taco is to come with "batteries included" for common use cases.

The "standard library" provides many methods for the built-in types, as well as modules for different I/O purposes, that come with their own types such as `File`, `Socket` and `Process`.

The stdlib bridges native host functionality into the language while keeping the language core small and consistent. Fallible APIs return typed results so error handling remains explicit.

### I/O concepts

I/O in Taco is entirely provided by native modules. The core language doesn't support any kind of I/O. All I/O modules are optional feature flags that can be enabled/disabled when compiling Taco itself. This is useful for instance for embedding Taco into special environments such as web assembly, where certain I/O modules are not available.

We've already seen several examples of I/O in this documentation, namely the `print` and `println` functions. These are in fact provided by the "print" module.

Other I/O modules are:

- `file` - for accessing the local file system
- `process` - for running programs from Taco
- `http` - for making HTTP requests
- `net` - for low-level TCP/UDP sockets
- `json` - for JSON decoding and parsing (strictly speaking not I/O)

Taco supports a few concepts for *concurrent* programming. Generally speaking, the I/O modules use efficient *asynchronous* native APIs, but are exposed to Taco as a simple blocking operations. There is no "await" keyword in Taco, and there never will be. The concurrent programming model in Taco is somewhat similar to Go.

The `spawn` keyword provides a way to run tasks in parallel. An example will be provided in the "Sockets" section.

### I/O pipelines

A unique feature of Taco, is the ability to create "pipes" or "pipelines", that can run several related I/O tasks in parallel. This feature is similar to pipes in Linux shells such as Bash.

Consider this shell command:

```bash
find -name '*.rs' | grep test | sort
```

Taco can run external programs using the `exec` function. So this exact command can be written like this:

```taco
exec("find -name '*.rs' | grep test | sort")
```

Nothing special so far. However, Taco also has a similar syntax built into the language:

```taco
exec("find -name '*.rs'") | exec("grep test") | exec("sort")
```

This is essentially the same as before, except that the pipes are implemented by Taco. Each process is started by Taco, and the whole pipeline runs concurrently.

In Taco, this is not limited to processes. For instance, a network socket can be "plugged" into the pipeline:

In this example, Taco reads from one socket ("client"), filters the data through an process, then forwards the result to another socket ("server"):

```taco
client | exec("nl") | server
```

Pipeline steps can also run Taco code. The `apply` function takes a regular Taco function, and "applies" it to every message in a stream, sending the return value to the next. This plugs into the type system, so that arbitrary Taco values can be passed down the pipe.

We can imagine an example where we listen to incoming messages from a client, parse each message into a record, pass that record to a function that adds it to a database, then returns a response message that is logged by an external program, and then passed back to the client:

```taco
rec User( ... )
fun import_user(user: User): str { ... }

client | apply(User::parse) | apply(import_user) | exec("logger") | client
```


