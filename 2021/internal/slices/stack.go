package slices

type Stack[T any] []T

func NewStack[T any](elems ...T) Stack[T] {
	return Stack[T](elems)
}

// Slice returns the stack as a slice, with elements in reverse order for easier
// consumption.
func (s Stack[T]) Slice() []T {
	rs := make([]T, len(s))
	for i := 0; i < len(s); i++ {
		rs[len(s)-i-1] = s[i]
	}
	return rs
}

func (s *Stack[T]) Push(v T) {
	*s = append(*s, v)
}

func (s *Stack[T]) Pop() (T, bool) {
	if len(*s) == 0 {
		var zero T
		return zero, false
	}
	elem := (*s)[len(*s)-1]
	*s = (*s)[:len(*s)-1]
	return elem, true
}
