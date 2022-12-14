package geom

import (
	"fmt"
	"math"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/types"
)

// P2 is a 2-dimensional Point.
type P2 struct {
	X, Y int
}

func (p2 P2) XY() (int, int) {
	return p2.X, p2.Y
}

func (pt P2) String() string {
	return fmt.Sprintf("P2(%d, %d)", pt.X, pt.Y)
}

type PlaneChangeFn[T any] func(old, new T, pt P2)

type Plane[T any] struct {
	Cells       [][]T
	onChangeFns []PlaneChangeFn[T]
}

func NewEmptyPlane[T any]() *Plane[T] {
	return &Plane[T]{}
}

func NewPlane[T any](grid [][]T) *Plane[T] {
	return &Plane[T]{Cells: grid}
}

func NewBoundedPlane[T any](bounds P2) *Plane[T] {
	p := &Plane[T]{}
	for y := 0; y < bounds.Y; y++ {
		p.Append(make([]T, bounds.X))
	}
	return p
}

func (p *Plane[T]) Append(r []T) *Plane[T] {
	p.Cells = append(p.Cells, r)
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
	return p.Cells[pt.Y][pt.X]
}

func (p *Plane[T]) GetOK(pt P2) (T, bool) {
	if !p.Contains(pt) {
		return types.Zero[T](), false
	}
	return p.Cells[pt.Y][pt.X], true
}

func (p *Plane[T]) Set(new T, pt P2) T {
	old := p.Get(pt)
	p.Cells[pt.Y][pt.X] = new
	slices.Each(p.onChangeFns, func(idx int, fn PlaneChangeFn[T]) { fn(old, new, pt) })
	return new
}

type LocateFn[T any] func(t T) bool

// LocateFirst will find the first P2 whose value matches [t], searching horizontal, then vertical.
func (p *Plane[T]) LocateFirst(fn LocateFn[T]) (P2, bool) {
	for y, line := range p.Cells {
		for x, t := range line {
			if fn(t) {
				return P2{x, y}, true
			}
		}
	}
	return P2{-1, -1}, false
}

func (p Plane[T]) Bounds() P2 {
	if len(p.Cells) == 0 {
		return P2{0, 0}
	}
	return P2{len(p.Cells[0]), len(p.Cells)}
}

func (p Plane[T]) Size() int {
	b := p.Bounds()
	return b.X * b.Y
}

func (p Plane[T]) Width() int {
	return p.Bounds().X
}

func (p Plane[T]) Height() int {
	return p.Bounds().Y
}

// neighbors

var (
	StepUp, StepDown, StepLeft, StepRight = P2{0, -1}, P2{0, 1}, P2{-1, 0}, P2{1, 0}
)

func (p Plane[T]) OnEdge(pt P2) bool {
	b := p.Bounds()
	return pt.X == 0 || pt.Y == 0 || pt.X == b.X-1 || pt.Y == b.Y-1
}

var (
	// up, down, left, right neighbors
	localNeighborOffsets = []P2{StepUp, StepLeft, StepRight, StepDown}
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

// Step returns a new [P2] applying [step] to [pt]. If new [P2] is outside the bounds of the
// plane, returns new [P2] and false; otherwise, returns new [P2] and true. See [StepUp],
// [StepDown], [StepLeft], and [StepRight] for examples of to use a step P2.
func (p Plane[T]) Step(pt P2, step P2) (P2, bool) {
	b := p.Bounds()
	npt := P2{X: pt.X + step.X, Y: pt.Y + step.Y}
	return npt, npt.X >= 0 && npt.Y >= 0 && npt.X < b.X && npt.Y < b.Y
}

// Up returns a [P2] above [pt]. If [P2] above is outside the bounds of the plane, returns
// [P2] above and false; otherwise, returns [P2] above and true.
func (p Plane[T]) Up(pt P2) (P2, bool) {
	return p.Step(pt, StepUp)
}

// Down returns a [P2] below [pt]. If [P2] below is outside the bounds of the plane, returns
// [P2] below and false; otherwise, returns [P2] below and true.
func (p Plane[T]) Down(pt P2) (P2, bool) {
	return p.Step(pt, StepDown)
}

// Left returns a [P2] left of [pt]. If [P2] left is outside the bounds of the plane, returns
// [P2] left and false; otherwise, returns [P2] left and true.
func (p Plane[T]) Left(pt P2) (P2, bool) {
	return p.Step(pt, StepLeft)
}

// Right returns a [P2] right of [pt]. If [P2] right is outside the bounds of the plane, returns
// [P2] right and false; otherwise, returns [P2] right and true.
func (p Plane[T]) Right(pt P2) (P2, bool) {
	return p.Step(pt, StepRight)
}

// traversals

type WalkFn[T any] func(T, P2) bool

func (p Plane[T]) walk(pt, step P2, fn WalkFn[T]) {
	b := p.Bounds()
	w, h, x, y, xstep, ystep := b.X, b.Y, pt.X, pt.Y, step.X, step.Y
	for y := y; y < h && y >= 0; y += ystep {
		for x := x; x < w && x >= 0; x += xstep {
			if !fn(p.Cells[y][x], P2{x, y}) {
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

type PathIncludeFn[T any] func(cur T, nxt T) bool

// ShortestLocalPath finds the shortest path from [S] to [E] using a breadth-first search.
// Each P2 on the plane (i.e. a node) is checked to see whether it has been a) visited and
// b) valid for inclusion in the path using the [PathIncludeFn]. Returns a [[]P2] of the
// points in the path from [S] to [E]; if not path found only [E] will be in the slice.
func (p Plane[T]) ShortestLocalPath(S, E P2, includeFn PathIncludeFn[T]) []P2 {
	parents := map[P2]*P2{S: nil} // track parents and node visits

	q := []P2{S}
	for len(q) > 0 {
		cpt := q[0] // pop
		q = q[1:]
		if cpt == E { // done
			break
		}
		cur := p.Get(cpt)
		for _, npt := range p.LocalNeighbors(cpt) {
			nxt := p.Get(npt)
			if _, v := parents[npt]; !v && includeFn(cur, nxt) {
				parents[npt] = &cpt // mark visited as well as next P2 parent
				q = append(q, npt)
			}
		}
	}
	steps := slices.NewStack(E)
	for wn := parents[E]; wn != nil; wn = parents[*wn] {
		steps.Push(*wn)
	}
	return steps
}
