package maths

import (
	"fmt"

	"github.com/gus/adventofcode/2022/internal/types"
)

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

func Abs[T types.OrderedNumeric](a T) T {
	if a < 0 {
		return -a
	}
	return a
}

type MinMax[T types.OrderedNumeric] struct {
	Min     T
	Max     T
	applied bool
}

func (mm *MinMax[T]) Copy() *MinMax[T] {
	return &MinMax[T]{Min: mm.Min, Max: mm.Max, applied: mm.applied}
}

func (mm *MinMax[T]) Apply(n T) *MinMax[T] {
	if !mm.applied || n < mm.Min {
		mm.Min = n
	}
	if !mm.applied || n > mm.Max {
		mm.Max = n
	}
	mm.applied = true
	return mm
}

func (mm MinMax[T]) String() string {
	return fmt.Sprintf("MinMax(%v %v)", mm.Min, mm.Max)
}
