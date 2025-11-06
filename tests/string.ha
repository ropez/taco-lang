prefix = "--"
fruit = "banana"
println("$prefix $fruit is a fruit")

fun foo() {
    return "foo: $fruit"
}

println("$prefix$fruit$prefix")

println("expression: ${fruit == prefix}")

println("call: '${foo()}'")

