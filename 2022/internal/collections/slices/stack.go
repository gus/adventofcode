package slices

// Stack implements a stack via a slice. Because elements pushed on this Stack
// are added to the head of the slice, if you were to loop/range over a Stack,
// elements will be retrieved in the same order as if you were popping them.
type Stack[T any] []T

// NewStack creates a new Stack. If any elements are provided, they are treated
// as if they are already a stack; thus they will be popped in the order they are
// provided.
func NewStack[T any](elems ...T) Stack[T] {
	s := make(Stack[T], 0)
	s = append(s, elems...)
	return s
}

// Push adds one or more new elements to the head of the stack. If more than one
// element is provided, each is pushed to the stack individually. Example:
//
//	s := NewStack[int]()
//	s.Push(0)
//	# s == Stack{0}
//	s.Push(1, 2, 3, 4)
//	# s == Stack{4, 3, 2, 1, 0}
func (s *Stack[T]) Push(vs ...T) {
	Each(vs, func(idx int, v T) { s.push(v) })
}

func (s *Stack[T]) push(v T) {
	if len(*s) == 0 {
		*s = append(*s, v)
		return
	}
	*s = append((*s)[:1], (*s)[0:]...)
	(*s)[0] = v
}

func (s *Stack[T]) PushN(vs ...T) {
	if len(*s) == 0 {
		*s = append(*s, vs...)
		return
	}
	*s = append(vs, *s...)
}

// Pop removes the element at the head of the stack and returns that value.
// If no more elements are in the stack, a zero-value is returned along with
// a false value as the second argument.
func (s *Stack[T]) Pop() (T, bool) {
	if len(*s) == 0 {
		var zero T
		return zero, false
	}
	elem := (*s)[0]
	*s = (*s)[1:]
	return elem, true
}

func (s *Stack[T]) PopN(n int) Stack[T] {
	news := []T{}
	for i := 0; i < n; i++ {
		t, ok := (*s).Pop()
		if !ok {
			break
		}
		news = append(news, t)
	}
	return news
}

// Peek looks at the head of the stack and returns the first value. If no
// elements are in the stack, a zero-value is returned along with a false
// value as the second argument.
func (s *Stack[T]) Peek() (T, bool) {
	if len(*s) == 0 {
		var zero T
		return zero, false
	}
	return (*s)[0], true
}
