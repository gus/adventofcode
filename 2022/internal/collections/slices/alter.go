package slices

func Pop[T any](s *[]T) (T, bool) {
	if len(*s) == 0 {
		var zero T
		return zero, false
	}
	elem := (*s)[0]
	*s = (*s)[1:]
	return elem, true
}
