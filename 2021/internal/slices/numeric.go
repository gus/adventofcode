package slices

import "github.com/gus/adventofcode/2021/internal/types"

type ReduceFunc[T any, A types.OrderedNumeric] func(acc A, val T) A

func Reduce[T any, A types.OrderedNumeric](s []T, acc A, fn ReduceFunc[T, A]) A {
	for _, i := range s {
		acc = fn(acc, i)
	}
	return acc
}

func Sum[T types.OrderedNumeric](s []T) (sum T) {
	return Reduce(s, 0, func(acc, i T) T { return acc + i })
}
