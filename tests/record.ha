record Point(x, y)

p = Point(100, 200)

println(p.x, p.y)

println("Point(${p.x}, ${p.y})")

if p.x == 100 {
    println("x is 100")
}

if 200 == p.y {
    println("y is 200")
}

if p.x == p.y {
    println("equal")
} else {
    println("not equal")
}
