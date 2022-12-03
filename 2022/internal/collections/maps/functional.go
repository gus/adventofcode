package maps

// ReduceErrFn defines the type of function provided to ReduceErr. It passes the accumulator
// and a k/v pair from a map and returns the accumulator and any error.
type ReduceErrFn[A any, K comparable, V any] func(acc A, key K, val V) (A, error)

// ReduceErr provides folding functionality over a map using a ReduceErrFn. It will halt
// folding if the provided function returns an error and return the accumulator value at that
// point along with the error. Use this reducer if errors are meaningful, otherwise use Reduce.
func ReduceErr[A any, K comparable, V any](m map[K]V, acc A, fn ReduceErrFn[A, K, V]) (A, error) {
	var err error
	for k, v := range m {
		if acc, err = fn(acc, k, v); err != nil {
			return acc, err
		}
	}
	return acc, nil
}

// ReduceFn defines the type of function provided to Reduce. It passes the accumulator
// and a k/v pair from a map and returns the accumulator.
type ReduceFn[A any, K comparable, V any] func(acc A, key K, val V) A

// Reduce provides folding functionality over a map using a ReduceFn. Use this reducer if
// errors are unexpected or safe to ignore, otherwise use ReduceErr.
func Reduce[A any, K comparable, V any](m map[K]V, acc A, fn ReduceFn[A, K, V]) A {
	acc, _ = ReduceErr(m, acc, func(a A, k K, v V) (A, error) { return fn(a, k, v), nil })
	return acc
}

func Intersect[K comparable, V any](a, b map[K]V) map[K]V {
	m := map[K]V{}

	for ak, av := range a {
		if _, ok := b[ak]; ok {
			m[ak] = av
		}
	}

	return m
}

func IntersectAll[K comparable, V any](maps ...map[K]V) map[K]V {
	if len(maps) == 0 {
		return map[K]V{}
	}
	m := maps[0]

	for _, cm := range maps[1:] {
		m = Intersect(m, cm)
	}

	return m
}

func Keys[K comparable, V any](m map[K]V) []K {
	ks := []K{}

	for k := range m {
		ks = append(ks, k)
	}

	return ks
}
