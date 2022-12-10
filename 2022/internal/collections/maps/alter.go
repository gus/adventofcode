package maps

// Update adds the k/v pairs from src to dst and returns dst.
func Update[K comparable, V any](dst map[K]V, src map[K]V) map[K]V {
	for k, v := range src {
		dst[k] = v
	}
	return dst
}
