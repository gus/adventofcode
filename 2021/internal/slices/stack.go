package slices

type Stack[T any] struct {
	s []T
}

func NewStack[T any](elems ...T) *Stack[T] {
	return &Stack[T]{elems}
}

// Slice returns the stack as a slice, with elements in reverse order for easier
// consumption.
func (s *Stack[T]) Slice() []T {
	rs := make([]T, len(s.s))
	for i := 0; i < len(s.s); i++ {
		rs[len(s.s)-i-1] = s.s[i]
	}
	return rs
}

func (s *Stack[T]) Push(v T) {
	s.s = append(s.s, v)
}

func (s *Stack[T]) Pop() (T, bool) {
	if len(s.s) == 0 {
		var zero T
		return zero, false
	}
	elem := s.s[len(s.s)-1]
	s.s = s.s[:len(s.s)-1]
	return elem, true
}
