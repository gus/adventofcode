package geom

import "math"

type Point struct {
	X, Y int
}

type Plane[T any] [][]T

func (p Plane[T]) Bounds() Point {
	if len(p) == 0 {
		return Point{0, 0}
	}
	return Point{len(p[0]), len(p)}
}

func (p Plane[T]) LocalNeighbors(pt Point) []T {
	b, x, y := p.Bounds(), pt.X, pt.Y
	ns := []T{}
	if x > 0 {
		ns = append(ns, p[y][x-1])
	}
	if x < b.X-1 {
		ns = append(ns, p[y][x+1])
	}
	if y > 0 {
		ns = append(ns, p[y-1][x])
	}
	if y < b.Y-1 {
		ns = append(ns, p[y+1][x])
	}
	return ns
}

type WalkFn[T any] func(T, Point) bool

func (p Plane[T]) walk(pt, step Point, fn WalkFn[T]) {
	b := p.Bounds()
	w, h, x, y, xstep, ystep := b.X, b.Y, pt.X, pt.Y, step.X, step.Y
	for y := y; y < h && y >= 0; y += ystep {
		for x := x; x < w && x >= 0; x += xstep {
			if !fn(p[y][x], Point{x, y}) {
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
