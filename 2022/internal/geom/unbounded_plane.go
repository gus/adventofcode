package geom

import (
	"github.com/gus/adventofcode/2022/internal/collections/maps"
	"github.com/gus/adventofcode/2022/internal/maths"
)

type UnboundedPlane[T any] struct {
	Cells    map[P2]T
	mmx, mmy *maths.MinMax[int]
}

func NewUnboundedPlane[T any]() *UnboundedPlane[T] {
	return &UnboundedPlane[T]{
		Cells: map[P2]T{},
		mmx:   &maths.MinMax[int]{},
		mmy:   &maths.MinMax[int]{},
	}
}

func (p *UnboundedPlane[T]) Copy() Plane[T] {
	return &UnboundedPlane[T]{
		Cells: maps.Update(map[P2]T{}, p.Cells),
		mmx:   p.mmx.Copy(),
		mmy:   p.mmx.Copy(),
	}
}

func (p *UnboundedPlane[T]) Get(pt P2) T {
	return p.Cells[pt]
}

func (p *UnboundedPlane[T]) GetOK(pt P2) (T, bool) {
	t, ok := p.Cells[pt]
	return t, ok
}

func (p *UnboundedPlane[T]) Set(new T, pt P2) T {
	// old := p.Get(pt)
	p.Cells[pt] = new
	p.mmx.Apply(pt.X)
	p.mmy.Apply(pt.Y)
	// slices.Each(p.onChangeFns, func(idx int, fn PlaneChangeFn[T]) { fn(old, new, pt) })
	return new
}

func (p UnboundedPlane[T]) Min() P2 {
	return P2{X: p.mmx.Min, Y: p.mmy.Min}
}

func (p UnboundedPlane[T]) Max() P2 {
	return P2{X: p.mmx.Max, Y: p.mmy.Max}
}

func (p UnboundedPlane[T]) OnEdge(pt P2) bool {
	return false
}
