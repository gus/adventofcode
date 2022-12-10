package maps

import "github.com/gus/adventofcode/2022/internal/types"

// SearchFn defines a function which can be provided to any of the search-related
// map functions. For each k/v pair in a map, the search function will receive
// the key and value as arguments; the function should return true or false if the
// pair applies to the search.
type SearchFn[K comparable, V any] func(K, V) bool

// Any returns true if at least one k/v pair succeeds for the provided
// search function; else false.
func Any[K comparable, V any](m map[K]V, fn SearchFn[K, V]) bool {
	for k, v := range m {
		if fn(k, v) {
			return true
		}
	}
	return false
}

// First returns the first key which succeeds for the given SearchFn (order is not
// guaranteed). The second return argument will be true if successful; else false.
func First[K comparable, V any](m map[K]V, fn SearchFn[K, V]) (K, bool) {
	for k, v := range m {
		if fn(k, v) {
			return k, true
		}
	}
	return types.Zero[K](), false
}

// Has returns true if k is a key with a value explicitly stored in m.
func Has[K comparable, V any](m map[K]V, k K) bool {
	_, ok := m[k]
	return ok
}
