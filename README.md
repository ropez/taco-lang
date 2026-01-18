# Taco programming language

This is an experimental programming language, under development as a
recreational hobby project.

The main goal of the language: Be the simplest possible way to program.

It is a statically typed _interpreted_ language, meant to be used for
relatively simple _scripts_. Typical use cases could be something like
automating chores, making requests to a remote HTTP API, or accessing a
database.

## Simplicity

The language design aims to achieve programs to be as _simple_ as possible, and
prevent many types of programming mistakes, by eliminating many complex
concepts found in many other programming languages.

This comes with some performance penalties and limited control over low-level
processes. The language is not expected to score high on any micro-benchmark.
However, it can still be effective for the specific tasks it's meant to solve.

I'm not sure if it should be possible to split a program into multiple files,
or create any kind of "project" or "package" structure beyond single script files.
(AFAIK, Bash scripts don't support anything like that). However, I am imagining
that the language could be _embeddable_, and support native extensions specific
to the embedded context. For instance, a text editor could inject a global
`document` object available inside an embedded script.

### Simple syntax

I've tried to eliminate many of the syntactical inconsistencies and
ambiguity found in many other languages:

- All code blocks have curly braces, and curly braces are only used for code
  blocks (not record instantiation etc)
- All strings support interpolation, there is no special quotation or prefix
  needed to "enable" interpolation
- Record "constructors" are just functions, with all the same rules
- Type annotations for lists and tuples use the same symbols and look similar
  to literal instances
- Type annotations are required in function and record definitions, and not
  present in variable assignments
- Type annotations always use a color followed by the type (never an arrow like
  in some languages)

### No memory management

The language by design doesn't offer any ways to manually allocate or manage
memory. Nor does it use a garbage collector. There are no references or
pointers, and no way to directly share state between parts of a program.

All data is _value based_. Passing an argument to a function always creates a
copy of the data (copy on write). There is no way for two "variables" to refer
to the same data, such that doing something with one of them affects the other.

This eliminates errors common in many other languages, such as:

- Memory leaks
- Access to unavailable memory
- Use after free

### Mutations don't exist

Any change to a data structure, such as appending an item to the end of a list,
always creates a new list. There is no way to _mutate_ an existing data
structure, such that the change is visible outside the current scope.

Assigning a new value to an existing "variable", _replaces_ that variable in the
current scope. There's no way to _change_ the value of an existing variable, so
that the change is visible outside the current scope. Technically, it doesn't
really make sense to use the name "variable", because they aren't allowed to "vary".
Taco variables are more like parameters in a math expression. You give 'x' a value,
and that's it. However, I'm using the name "variable" because it's so
established, there's no real alternative.

I believe that in many of the cases that the language is meant for, mutable
data is simply not needed. The "state" of a script simply progresses as the
script executes from top to bottom, from function to function and so on.
However, for advanced cases where state is needed, the language will provide a
construct specific for that. It will be possible to create a kind of stateful
primitive, that can be read/written by value similar to reading/writing a file.

### No shared state between concurrent processes

The language by design doesn't offer any means of creating "threads" or "async"
tasks with access to shared state.

At some point in the future, some way to run tasks concurrently might be
implemented, by allowing one script to spawn some kind of sub-processes or
"isolates". However, it will not be allowed to directly share state between
them. Synchronization must use some kind of messaging passing or signalling.
Probably closely related to state primitives.

### Statically typed

Untypical for scripting languages, Taco scripts will be statically typed, and
the interpreter will provide informational error messages for programming
mistakes such as trying to call a function with wrong arguments.

### Expressiveness

String interpolation, native structures like list, set and dict. Record types,
ranges, unions (like enums in Rust) and pattern matching.

### Batteries included

The plan is that the language will _natively_, or through some kind of _native
extensions_, support commonly needed functionality, such as:

- Read command line arguments
- Writing to standard output
- Reading from stardard input
- Reading/writing to files
- Running sub-processes
- Making HTTP requests, and serialize/deserialize JSON data

## Inspiration

Although at the time of writing many of the language features are not yet
implemented, the conceptual ideas and prototypes draw inspiration from a
variety of existing programming languages and frameworks:

- The spirit and main purpose of the language is similar to that of scripting
  languages such as *Bash*.
- There are syntactical elements with similarities to several languages such as
  *Rust*, *Typescript* and *Python*
- String interpolation syntax similar to *Dart*
- Passing standard data structures by value, and copy on write is something I
  have seen in the *Swift* language and the *Qt* framework in C++
- State primitives are similar to "reactivity" found in *React*, *Vue* and *Svelte*
- Isolates and message passing in *Dart*
- Restrictions on mutation is central in functional programming languages
- Union types including Option and Result (error handling) similar to *Rust*

## Documentation

See [documentation](DOCUMENTATION.md).

