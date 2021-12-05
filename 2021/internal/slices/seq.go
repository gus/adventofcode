package slices

import "github.com/gus/adventofcode/2021/internal/types"

func Sequence[T types.AnyInt](a, b T, inclusive bool) []T {
	step := T(1)
	if a > b {
		step = -step
	}

	seq := []T{}
	for i := a; i != b; i += step {
		seq = append(seq, i)
	}
	if inclusive {
		seq = append(seq, b)
	}
	return seq
}
