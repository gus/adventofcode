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

func (s Set[T]) Intersect(o Set[T]) Set[T] {
	snew := Set[T]{}
	for e := range o {
		if s.Contains(e) {
			snew.Add(e)
		}
	}
	return snew
}

func (s Set[T]) Merge(o Set[T]) {
	for e := range o {
		s.Add(e)
	}
}
