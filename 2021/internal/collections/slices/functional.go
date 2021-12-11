package slices

import (
	"github.com/gus/adventofcode/2021/internal/types"
)

type MapErrFn[S any, T any] func(s S) (T, error)

func MapErr[S any, T any](ss []S, fn MapErrFn[S, T]) ([]T, error) {
	var err error
	st := make([]T, len(ss))
	for i, s := range ss {
		if st[i], err = fn(s); err != nil {
			return st, err
		}
	}
	return st, nil
}

type MapFn[S any, T any] func(s S) T

func Map[S any, T any](ss []S, fn MapFn[S, T]) []T {
	st, _ := MapErr(ss, func(s S) (T, error) { return fn(s), nil })
	return st
}

type ReduceErrFn[T any, A any] func(acc A, val T) (A, error)

func ReduceErr[T any, A any](st []T, acc A, fn ReduceErrFn[T, A]) (A, error) {
	var err error
	for _, v := range st {
		if acc, err = fn(acc, v); err != nil {
			return acc, err
		}
	}
	return acc, nil
}

type ReduceFn[T any, A any] func(acc A, val T) A

func Reduce[T any, A any](st []T, acc A, fn ReduceFn[T, A]) A {
	acc, _ = ReduceErr(st, acc, func(a A, t T) (A, error) { return fn(a, t), nil })
	return acc
}

func Sum[T types.OrderedNumeric](s []T) T {
	return Reduce(s, T(0), func(acc, i T) T { return acc + i })
}

type IterFn[T any] func(idx int, elem T)

func Each[T any](ss []T, fn IterFn[T]) {
	for i, s := range ss {
		fn(i, s)
	}
}
