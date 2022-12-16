package geom

type WalkFn[T any] func(T, P2) bool

func walk[T any](p Plane[T], pt, step P2, fn WalkFn[T]) {
	mn, mx := p.Min(), p.Max()
	px, py, xstep, ystep := pt.X, pt.Y, step.X, step.Y
	for y := py; y <= mx.Y && y >= mn.Y; y += ystep {
		for x := px; x <= mx.X && x >= mn.X; x += xstep {
			p2 := P2{x, y}
			if !fn(p.Get(p2), p2) {
				return
			}
		}
	}
}

func WalkPlane[T any](p Plane[T], fn WalkFn[T]) {
	walk(p, p.Min(), P2{1, 1}, fn)
}
