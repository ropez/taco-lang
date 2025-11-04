fun indent(arg) {
    return "- $arg"
}

fun bye(arg) {
    msg = "Good bye"
    println(msg, indent(arg))
}

bye("Have a nice day")
