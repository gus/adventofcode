package maths

import "github.com/gus/adventofcode/2022/internal/types"

func Between[T types.OrderedNumeric](x, a, b T) bool {
	if b < a {
		a, b = b, a
	}
	return x >= a && x <= b
}

func Cmp[T types.OrderedNumeric](a, b T) int {
	if a < b {
		return -1
	} else if a > b {
		return 1
	}
	return 0
}
