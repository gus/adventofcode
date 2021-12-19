package maths

import "github.com/gus/adventofcode/2021/internal/types"

func Between[T types.OrderedNumeric](x, a, b T) bool {
	if b < a {
		a, b = b, a
	}
	return x >= a && x <= b
}
