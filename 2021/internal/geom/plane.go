package geom

import (
	"fmt"
	"math"

	"github.com/gus/adventofcode/2021/internal/collections/slices"
	"github.com/gus/adventofcode/2021/internal/types"
)

// P2 is a 2-dimensional Point.
type P2 struct {
	X, Y int
}

func (p2 P2) XY() (int, int) {
	return p2.X, p2.Y
}

func (pt P2) String() string {
	return fmt.Sprintf("Point{%d, %d}", pt.X, pt.Y)
}

type PlaneChangeFn[T any] func(old, new T, pt P2)

type Plane[T any] struct {
	grid        [][]T
	onChangeFns []PlaneChangeFn[T]
}

func NewEmptyPlane[T any]() *Plane[T] {
	return &Plane[T]{}
}

func NewPlane[T any](bounds P2) *Plane[T] {
	p := &Plane[T]{}
	for y := 0; y < bounds.Y; y++ {
		p.Append(make([]T, bounds.X))
	}
	return p
}

func (p *Plane[T]) Append(r []T) *Plane[T] {
	p.grid = append(p.grid, r)
	return p
}

func (p *Plane[T]) OnChange(fn PlaneChangeFn[T]) int {
	p.onChangeFns = append(p.onChangeFns, fn)
	return len(p.onChangeFns) - 1
}

func (p *Plane[T]) ClearChange(id int) {
	p.onChangeFns = append(p.onChangeFns[:id], p.onChangeFns[id+1:]...)
}

func (p *Plane[T]) Contains(pt P2) bool {
	b := p.Bounds()
	return pt.X > -1 && pt.X < b.X && pt.Y > -1 && pt.Y < b.Y
}

func (p *Plane[T]) Get(pt P2) T {
	return p.grid[pt.Y][pt.X]
}

func (p *Plane[T]) GetOK(pt P2) (T, bool) {
	if !p.Contains(pt) {
		return types.Zero[T](), false
	}
	return p.grid[pt.Y][pt.X], true
}

func (p *Plane[T]) Set(new T, pt P2) T {
	old := p.Get(pt)
	p.grid[pt.Y][pt.X] = new
	slices.Each(p.onChangeFns, func(idx int, fn PlaneChangeFn[T]) { fn(old, new, pt) })
	return new
}

func (p Plane[T]) Bounds() P2 {
	if len(p.grid) == 0 {
		return P2{0, 0}
	}
	return P2{len(p.grid[0]), len(p.grid)}
}

func (p Plane[T]) Size() int {
	b := p.Bounds()
	return b.X * b.Y
}

var (
	// up, down, left, right neighbors
	localNeighborOffsets = []P2{{0, -1}, {0, 1}, {-1, 0}, {1, 0}}
	// local neighbors plus corners
	neighborOffsets = append(localNeighborOffsets, []P2{{-1, -1}, {-1, 1}, {1, -1}, {1, 1}}...)
)

func (p Plane[T]) neighbors(pt P2, offsets []P2) []P2 {
	b, x, y := p.Bounds(), pt.X, pt.Y
	return slices.Reduce(offsets, []P2{}, func(acc []P2, o P2) []P2 {
		nx, ny := x+o.X, y+o.Y
		if !(nx < 0 || nx >= b.X || ny < 0 || ny >= b.Y) {
			acc = append(acc, P2{nx, ny})
		}
		return acc
	})
}

func (p Plane[T]) Neighbors(pt P2) []P2 {
	return p.neighbors(pt, neighborOffsets)
}

func (p Plane[T]) LocalNeighbors(pt P2) []P2 {
	return p.neighbors(pt, localNeighborOffsets)
}

type WalkFn[T any] func(T, P2) bool

func (p Plane[T]) walk(pt, step P2, fn WalkFn[T]) {
	b := p.Bounds()
	w, h, x, y, xstep, ystep := b.X, b.Y, pt.X, pt.Y, step.X, step.Y
	for y := y; y < h && y >= 0; y += ystep {
		for x := x; x < w && x >= 0; x += xstep {
			if !fn(p.grid[y][x], P2{x, y}) {
				return
			}
		}
	}
}

func (p Plane[T]) WalkAll(fn WalkFn[T]) {
	p.walk(P2{0, 0}, P2{1, 1}, fn)
}

func (p Plane[T]) WalkLeft(pt P2, fn WalkFn[T]) {
	p.walk(pt, P2{-1, math.MaxInt}, fn)
}

func (p Plane[T]) WalkRight(pt P2, fn WalkFn[T]) {
	p.walk(pt, P2{1, math.MaxInt}, fn)
}

func (p Plane[T]) WalkUp(pt P2, fn WalkFn[T]) {
	p.walk(pt, P2{math.MaxInt, -1}, fn)
}

func (p Plane[T]) WalkDown(pt P2, fn WalkFn[T]) {
	p.walk(pt, P2{math.MaxInt, 1}, fn)
}
