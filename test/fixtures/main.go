package main

func Add(a, b int) int {
	return a + b
}

func Multiply(a, b int) int {
	result := 0
	for i := 0; i < b; i++ {
		result += a
	}
	return result
}

func main() {
	x := Add(3, 4)
	y := Multiply(x, 2)
	_ = y
}
