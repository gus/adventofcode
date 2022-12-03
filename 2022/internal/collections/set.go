package collections

type setv struct{}
type Set[K comparable] map[K]setv

func NewSet[K comparable](ks ...K) Set[K] {
	s := Set[K]{}
	s.Add(ks...)
	return s
}

func (s Set[K]) Add(ks ...K) {
	for _, k := range ks {
		s[k] = setv{}
	}
}

func (s Set[K]) Remove(ks ...K) {
	for _, k := range ks {
		delete(s, k)
	}
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

func (s Set[K]) Merge(o Set[K]) {
	for k := range o {
		s.Add(k)
	}
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
