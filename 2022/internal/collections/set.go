package collections

type setv struct{}
type Set[K comparable] map[K]setv

func NewSet[K comparable](ks ...K) Set[K] {
	s := Set[K]{}
	s.Add(ks...)
	return s
}

func (s Set[K]) Add(ks ...K) Set[K] {
	for _, k := range ks {
		s[k] = setv{}
	}
	return s
}

func (s Set[K]) Remove(ks ...K) Set[K] {
	for _, k := range ks {
		delete(s, k)
	}
	return s
}

func (s Set[K]) Contains(k K) bool {
	_, ok := s[k]
	return ok
}

func (s Set[K]) Intersect(o Set[K]) Set[K] {
	snew := Set[K]{}
	for k := range o {
		if s.Contains(k) {
			snew.Add(k)
		}
	}
	return snew
}

func (s Set[K]) Merge(o Set[K]) Set[K] {
	for k := range o {
		s.Add(k)
	}
	return s
}

func (s Set[K]) Slice() []K {
	ks := make([]K, len(s))
	idx := 0
	for k := range s {
		ks[idx] = k
		idx++
	}
	return ks
}

func (s Set[K]) Clone() Set[K] {
	return NewSet(s.Slice()...)
}
