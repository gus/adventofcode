package collections

type Set[T comparable] map[T]struct{}

func NewSet[T comparable](elems ...T) Set[T] {
	s := Set[T]{}
	s.Add(elems...)
	return s
}

func (s Set[T]) Add(elems ...T) {
	for _, e := range elems {
		s[e] = struct{}{}
	}
}

func (s Set[T]) Contains(elem T) bool {
	_, ok := s[elem]
	return ok
}
