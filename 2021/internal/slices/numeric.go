package slices

import "github.com/gus/adventofcode/2021/internal/types"

type ReduceFunc[T types.OrderedNumeric] func(acc T, val T) T

func Reduce[T types.OrderedNumeric](s []T, acc T, fn ReduceFunc[T]) T {
	for _, i := range s {
		acc = fn(acc, i)
	}
	return acc
}

func Sum[T types.OrderedNumeric](s []T) (sum T) {
	return Reduce(s, 0, func(acc, i T) T { return acc + i })
}
