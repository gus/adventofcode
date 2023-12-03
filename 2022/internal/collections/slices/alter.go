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

// Remove one [e] from given slice and return a new slice.
func Remove[T comparable](s []T, e T) []T {
	ns := make([]T, 0, len(s))
	for i, v := range s {
		if v == e {
			return append(ns, s[i+1:]...)
		}
		ns = append(ns, v)
	}
	return ns
}
