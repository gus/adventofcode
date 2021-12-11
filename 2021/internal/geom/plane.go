package geom

import (
	"fmt"
	"math"

	"github.com/gus/adventofcode/2021/internal/slices"
)

type Point struct {
	X, Y int
}

func (pt Point) String() string {
	return fmt.Sprintf("Point{%d, %d}", pt.X, pt.Y)
}

type PlaneChangeFn[T any] func(old, new T, pt Point)

type Plane[T any] struct {
	grid        [][]T
	onChangeFns []PlaneChangeFn[T]
}

func NewPlane[T any]() *Plane[T] {
	return &Plane[T]{}
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
	// if changeId == 0 {
	// 	p.onChangeFns = p.onChangeFns[1:]
	// }
	p.onChangeFns = append(p.onChangeFns[:id], p.onChangeFns[id+1:]...)
}

func (p *Plane[T]) Get(pt Point) T {
	return p.grid[pt.Y][pt.X]
}

func (p *Plane[T]) Set(new T, pt Point) T {
	old := p.Get(pt)
	p.grid[pt.Y][pt.X] = new
	slices.Each(p.onChangeFns, func(idx int, fn PlaneChangeFn[T]) { fn(old, new, pt) })
	return new
}

func (p Plane[T]) Bounds() Point {
	if len(p.grid) == 0 {
		return Point{0, 0}
	}
	return Point{len(p.grid[0]), len(p.grid)}
}

func (p Plane[T]) Size() int {
	b := p.Bounds()
	return b.X * b.Y
}

func (p Plane[T]) Neighbors(pt Point) []Point {
	// up, down, left, right neighbors
	b, x, y := p.Bounds(), pt.X, pt.Y
	ns := p.LocalNeighbors(pt)
	// pick up corners
	if x > 0 {
		if y > 0 { // top left
			ns = append(ns, Point{x - 1, y - 1})
		}
		if y < b.Y-1 { // bottom left
			ns = append(ns, Point{x - 1, y + 1})
		}
	}
	if x < b.X-1 {
		if y > 0 { // top right
			ns = append(ns, Point{x + 1, y - 1})
		}
		if y < b.Y-1 { // bottom right
			ns = append(ns, Point{x + 1, y + 1})
		}
	}
	return ns
}

func (p Plane[T]) LocalNeighbors(pt Point) []Point {
	// up, down, left, right neighbors
	b, x, y := p.Bounds(), pt.X, pt.Y
	ns := []Point{}
	if x > 0 {
		ns = append(ns, Point{x - 1, y})
	}
	if x < b.X-1 {
		ns = append(ns, Point{x + 1, y})
	}
	if y > 0 {
		ns = append(ns, Point{x, y - 1})
	}
	if y < b.Y-1 {
		ns = append(ns, Point{x, y + 1})
	}
	return ns
}

type WalkFn[T any] func(T, Point) bool

func (p Plane[T]) walk(pt, step Point, fn WalkFn[T]) {
	b := p.Bounds()
	w, h, x, y, xstep, ystep := b.X, b.Y, pt.X, pt.Y, step.X, step.Y
	for y := y; y < h && y >= 0; y += ystep {
		for x := x; x < w && x >= 0; x += xstep {
			if !fn(p.grid[y][x], Point{x, y}) {
				return
			}
		}
	}
}

func (p Plane[T]) WalkAll(fn WalkFn[T]) {
	p.walk(Point{0, 0}, Point{1, 1}, fn)
}

func (p Plane[T]) WalkLeft(pt Point, fn WalkFn[T]) {
	p.walk(pt, Point{-1, math.MaxInt}, fn)
}

func (p Plane[T]) WalkRight(pt Point, fn WalkFn[T]) {
	p.walk(pt, Point{1, math.MaxInt}, fn)
}

func (p Plane[T]) WalkUp(pt Point, fn WalkFn[T]) {
	p.walk(pt, Point{math.MaxInt, -1}, fn)
}

func (p Plane[T]) WalkDown(pt Point, fn WalkFn[T]) {
	p.walk(pt, Point{math.MaxInt, 1}, fn)
}
