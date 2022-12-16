package geom

import "fmt"

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

type PathIncludeFn[T any] func(cur T, nxt T) bool

type LocateFn[T any] func(t T) bool

var (
	P2StepUp    = P2{0, -1}
	P2StepDown  = P2{0, 1}
	P2StepLeft  = P2{-1, 0}
	P2StepRight = P2{1, 0}
)

var (
	// up, down, left, right neighbors
	localNeighborOffsets = []P2{P2StepUp, P2StepLeft, P2StepRight, P2StepDown}
	// local neighbors plus corners
	neighborOffsets = append(localNeighborOffsets, []P2{{-1, -1}, {-1, 1}, {1, -1}, {1, 1}}...)
)

type Plane[T any] interface {
	Copy() Plane[T]
	Get(pt P2) T
	GetOK(pt P2) (T, bool)
	Set(t T, pt P2) T
	Min() P2
	Max() P2
	OnEdge(pt P2) bool
	// WalkAll(fn WalkFn[T])
}
