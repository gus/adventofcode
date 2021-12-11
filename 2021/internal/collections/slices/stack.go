package slices

type Stack[T any] []T

func NewStack[T any](elems ...T) Stack[T] {
	return Stack[T](elems)
}

func (s *Stack[T]) Push(v T) {
	if len(*s) == 0 {
		*s = append(*s, v)
		return
	}
	*s = append((*s)[:1], (*s)[0:]...)
	(*s)[0] = v
}

func (s *Stack[T]) Pop() (T, bool) {
	if len(*s) == 0 {
		var zero T
		return zero, false
	}
	elem := (*s)[0]
	*s = (*s)[1:]
	return elem, true
}
