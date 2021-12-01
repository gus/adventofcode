package slices

import "github.com/gus/adventofcode/2021/internal/types"

func Sum[T types.OrderedNumeric](s []T) (sum T) {
	for _, i := range s {
		sum += i
	}
	return sum
}
