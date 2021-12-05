package maths

import "github.com/gus/adventofcode/2021/internal/types"

func Min[T types.OrderedNumeric](a, b T) T {
	if b < a {
		return b
	}
	return a
}

func Max[T types.OrderedNumeric](a, b T) T {
	if b > a {
		return b
	}
	return a
}
