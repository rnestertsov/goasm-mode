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

type Calc struct{}

func (c Calc) Add(a, b int) int {
	return a + b
}

type Store struct{}

func (s *Store) Get(key string) string {
	return key
}

type Alpha struct{}

func (a Alpha) Scan() int {
	return 1
}

type Beta struct{}

func (b *Beta) Scan() int {
	return 2
}
