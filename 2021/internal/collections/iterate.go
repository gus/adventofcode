package collections

import "github.com/gus/adventofcode/2021/internal/types"

type IterateRangeFn[T types.AnyInt] func(i T)

func IterateRange[T types.AnyInt](a, b T, inclusive bool, fn IterateRangeFn[T]) {
	step := T(1)
	if a > b {
		step = -step
	}

	for i := a; i != b; i += step {
		fn(i)
	}
	if inclusive {
		fn(b)
	}
}
