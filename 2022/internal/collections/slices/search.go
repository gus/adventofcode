package slices

import (
	"github.com/gus/adventofcode/2022/internal/collections"
	"github.com/gus/adventofcode/2022/internal/types"
)

type SearchFn[T any] func(elem T) bool

func First[T any](s []T, fn SearchFn[T]) (int, T) {
	for i, elem := range s {
		if fn(elem) {
			return i, elem
		}
	}
	return -1, types.Zero[T]()
}

func RemoveFirst[T any](s []T, fn SearchFn[T]) T {
	i, elem := First(s, fn)
	if i > -1 {
		sp := &s
		*sp = append((*sp)[:i], (*sp)[i+1:]...)
	}
	return elem
}

func Any[T any](s []T, fn SearchFn[T]) bool {
	i, _ := First(s, fn)
	return i > -1
}

func Filter[T any](s []T, fn SearchFn[T]) []T {
	ts := []T{}
	for _, elem := range s {
		if fn(elem) {
			ts = append(ts, elem)
		}
	}
	return ts
}

// IntersectUniq will return a [[]K] containing one [K] for each element that is common
// between the two given slices. Meaning, if each has multiple values for the same
// [K], only one will be returned. IntersectUniq uses [collections.Set] to accomplish
// this.
func IntersectUniq[T comparable](a, b []T) []T {
	return collections.NewSet(a...).Intersect(collections.NewSet(b...)).Slice()
}
