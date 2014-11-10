package main

import (
    "fmt"
    "math"
)

/*
 * ITC: Horrible implementation of FizzBuzz in Go
 * If this is anywhere near an optimal solution, go is already dead
 */

func main() {
    for i := 1; i <= 100; i++ {
        if math.Mod(float64(i), 3) == 0 && math.Mod(float64(i), 5) == 0 {
            fmt.Println("FizzBuzz")
            continue
        }
        if math.Mod(float64(i), 3) == 0 {
            fmt.Println("Fizz")
            continue
        }
        if math.Mod(float64(i), 5) == 0 {
            fmt.Println("Buzz")
            continue
        }
        fmt.Println(i)
    }
}

