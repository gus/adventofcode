package maps

type SearchFn[K comparable, V any] func(K, V) bool

func Any[K comparable, V any](m map[K]V, fn SearchFn[K, V]) bool {
	for k, v := range m {
		if fn(k, v) {
			return true
		}
	}
	return false
}
