package slices

type SearchFn[T any] func(elem T) bool

func Find[T any](s []T, fn SearchFn[T]) T {
	for _, elem := range s {
		if fn(elem) {
			return elem
		}
	}
	var zero T
	return zero
}

func FindPop[T any](s []T, fn SearchFn[T]) (T, []T) {
	for i := 0; i < len(s); i++ {
		elem := s[i]
		if fn(elem) {
			s = append(s[:i], s[i+1:]...)
			return elem, s
		}
	}
	var zero T
	return zero, s
}

func Any[T any](s []T, fn SearchFn[T]) bool {
	for _, elem := range s {
		if fn(elem) {
			return true
		}
	}
	return false
}
