fun indent(arg) {
    return "- $arg"
}

fun args(first, second, third) {
    println("first: $first, second: $second, third: $third")
}

args(1, 2, 3)
args("a", third = "c", second = "b")

fun bye(arg) {
    msg = "Good bye"
    println(msg, indent(arg))
}

bye("Have a nice day")
