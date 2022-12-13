package slices

import (
	"github.com/gus/adventofcode/2022/internal/types"
)

// maps

type MapErrEnumFn[S any, T any] func(idx int, s S) (T, error)

func MapErrEnum[S any, T any](ss []S, fn MapErrEnumFn[S, T]) ([]T, error) {
	var err error
	st := make([]T, len(ss))
	for i, s := range ss {
		if st[i], err = fn(i, s); err != nil {
			return st, err
		}
	}
	return st, nil
}

type MapEnumFn[S any, T any] func(idx int, s S) T

func MapEnum[S any, T any](ss []S, fn MapEnumFn[S, T]) []T {
	st, _ := MapErrEnum(ss, func(idx int, s S) (T, error) { return fn(idx, s), nil })
	return st
}

type MapErrFn[S any, T any] func(s S) (T, error)

func MapErr[S any, T any](ss []S, fn MapErrFn[S, T]) ([]T, error) {
	return MapErrEnum(ss, func(idx int, s S) (T, error) { return fn(s) })
}

type MapFn[S any, T any] func(s S) T

func Map[S any, T any](ss []S, fn MapFn[S, T]) []T {
	return MapEnum(ss, func(idx int, s S) T { return fn(s) })
}

// reducers

type ReduceEnumErrFn[T any, A any] func(acc A, idx int, val T) (A, error)

func ReduceEnumErr[T any, A any](st []T, acc A, fn ReduceEnumErrFn[T, A]) (A, error) {
	var err error
	for i, v := range st {
		if acc, err = fn(acc, i, v); err != nil {
			return acc, err
		}
	}
	return acc, nil
}

type ReduceEnumFn[T any, A any] func(acc A, idx int, val T) A

func ReduceEnum[T any, A any](st []T, acc A, fn ReduceEnumFn[T, A]) A {
	acc, _ = ReduceEnumErr(st, acc, func(a A, i int, t T) (A, error) { return fn(a, i, t), nil })
	return acc
}

type ReduceErrFn[T any, A any] func(acc A, val T) (A, error)

func ReduceErr[T any, A any](st []T, acc A, fn ReduceErrFn[T, A]) (A, error) {
	return ReduceEnumErr(st, acc, func(a A, i int, t T) (A, error) { return fn(a, t) })
}

type ReduceFn[T any, A any] func(acc A, val T) A

func Reduce[T any, A any](st []T, acc A, fn ReduceFn[T, A]) A {
	acc, _ = ReduceErr(st, acc, func(a A, t T) (A, error) { return fn(a, t), nil })
	return acc
}

// helpers

func Sum[T types.OrderedNumeric](s []T) T {
	return Reduce(s, T(0), func(acc, i T) T { return acc + i })
}

type IterFn[T any] func(idx int, elem T)

func Each[T any](ss []T, fn IterFn[T]) {
	for i, s := range ss {
		fn(i, s)
	}
}
