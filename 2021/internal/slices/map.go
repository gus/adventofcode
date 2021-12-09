package slices

type MapFn[S any, T any] func(s S) T

func Map[S any, T any](ss []S, fn MapFn[S, T]) []T {
	st := make([]T, len(ss))
	for i, s := range ss {
		st[i] = fn(s)
	}
	return st
}
