# Sea-lang

The simplest way to program

## Simplicity

Type inference
No memory management
No garbage collection
Value semantics (CoW)
No reference types
Rust-like enum/struct
Immutable

Single file (maybe "modules" later)

## Features

- "native" subcommands
- "native" HTTP requests
- Pipes?

## Inspiration

Rust:
    - Structs & Enums
    - Result & Option
Swift:
    - Value semantics
Bash:
    - Simplicity
    - Program structure
    - Resource management

## Mutability

Mutable variables don't exist.
See how much we can do without.

Eventually, add something like this:

```
getFruits, setFruits = useState(list<str>())

setFruits(getFruits().add("Apple"))
setFruits(getFruits().add("Banana"))
```

## Good for

API "integrations":

```
record Project {
    id: str
    name: str
    number: str
}

fun get_projects() -> list<Project> {
    res = http.get(cfg.api_url)
    return res.json()
}

for p in get_projects() {
    println("Found project: ", project.name)
}
```

# Samples

```
println("Hello, world")
```

```
fun welcome(msg: str) {
    println(msg)
}

welcome("Hello, world")
```

```
fun print_names(names: list<str>) {
    for name in names {
        println("Name: ", name)
    }
}
```


```
exec('ls').pipe('less')
```

```
rec Point {
    x: int
    y: int
}

# Equivalent
rec Point(x: int, y: int)

enum Opt<T> {
    None
    Some(T)
}

p: Opt<Point> = Some(Point(0, 0))
p: Point? = None

fn parse(json: str): Point? {
}

enum Res<T, E> {
    Ok(T)
    Err(E)
}

r: Res<Point, Error>
r: Point ~ Error

fun parse(json: str): Res<Opt<Point>, Error> {
}

fun parse(json: str): Res<Point?, Error> {
}

fun parse(json: str): Opt<Point> ~ Error {
}

fun parse(json: str): Point? ~ Error {
}
```

## Assignment always shadows

"Variables" aren't _variable_, they are immutable.
Need a different name than "variable".
Assigning a new value doesn't change a variable, it replaces it with a new binding.

Changes type:

```
a = http.get(url)
a = a.json<Data>()
a = a.items
```

Scoped:

```
a = 10
if a == 10 {
    println(a) # 10
    a: str = "hello"
    println(a) # hello
}
println(a) # 10
```

To change outer state:

```
get_a, set_a = state(10)

if get_a() == 10 {
    set_a(20)
}
println(get_a()) # 20
```

XXX Avoid = / == confusion?

But what happens here??

```
a = 10

fun foo() {
    println(a)
}

a = 20

foo()
```

XXX Probably this should not be allowed?
XXX In Rust, this is legal, and prints "10"
