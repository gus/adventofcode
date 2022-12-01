package slices

import (
	"github.com/gus/adventofcode/2022/internal/collections"
	"github.com/gus/adventofcode/2022/internal/types"
)

func Sequence[T types.AnyInt](a, b T, inclusive bool) []T {
	seq := []T{}
	collections.IterateRange(a, b, inclusive, func(i T) {
		seq = append(seq, i)
	})
	return seq
}
