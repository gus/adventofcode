package main

import (
	"bytes"
	"fmt"
	"io"
	"os"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/geom"
	"github.com/gus/adventofcode/2022/internal/maths"
)

type state int

const (
	Air state = iota
	Rock
	Sand
)

func parsePath(line []byte) []geom.P2 {
	return slices.Map(bytes.Split(line, []byte(" -> ")), func(pt []byte) geom.P2 {
		xy := slices.Atoi(slices.Map(bytes.Split(pt, []byte{','}), func(b []byte) string {
			return string(b)
		}))
		return geom.P2{X: xy[0], Y: xy[1]}
	})
}

func placeRocks(p *geom.Plane[state], path []geom.P2) *geom.Plane[state] {
	lp2 := path[0]
	for _, p2 := range path[1:] {
		if p2.X == lp2.X { // draw vertical
			step := maths.Cmp(lp2.Y, p2.Y) * -1
			for y := lp2.Y; y != p2.Y+step; y += step {
				p.Set(Rock, geom.P2{X: lp2.X, Y: y})
			}
		} else { // draw horizontal
			step := maths.Cmp(lp2.X, p2.X) * -1
			for x := lp2.X; x != p2.X+step; x += step {
				p.Set(Rock, geom.P2{X: x, Y: lp2.Y})
			}
		}
		lp2 = p2
	}

	return p
}

func poundRocks(p *geom.Plane[state], entryP2 geom.P2, expander expandfn) int {
	var resting int
	inside := true
	sand := entryP2
	for {
		var d, dl, dr geom.P2
		// look down
		if d, inside = p.Down(sand); !inside {
			break
		} else if p.Get(d) == Air {
			sand = d
			continue
		}
		// look down+left
		if dl, inside = p.Left(d); !inside && expander == nil {
			break
		} else if !inside {
			p = expander(p)
			// fmt.Printf("    L-EXPAND from -> %s\n", sand)
			inside, sand = true, geom.P2{X: sand.X + 1, Y: sand.Y}
			entryP2 = geom.P2{X: entryP2.X + 1, Y: entryP2.Y}
			continue // try again
		} else if p.Get(dl) == Air {
			sand = dl
			continue
		}
		// look down+right
		if dr, inside = p.Right(d); !inside && expander == nil {
			break
		} else if !inside {
			p = expander(p)
			// fmt.Printf("    R-EXPAND from -> %s\n", sand)
			inside, sand = true, geom.P2{X: sand.X + 1, Y: sand.Y}
			entryP2 = geom.P2{X: entryP2.X + 1, Y: entryP2.Y}
			continue // try again
		} else if p.Get(dr) == Air {
			sand = dr
			continue
		}
		p.Set(Sand, sand)
		resting++
		if !inside || sand.Y == 0 {
			break
		}
		// made it here, we're now resting
		sand = entryP2 // new sand
	}
	printPlane(p)
	return resting
}

type expandfn func(*geom.Plane[state]) *geom.Plane[state]

func part2Expander(p *geom.Plane[state]) *geom.Plane[state] {
	// increases x by 1 in both directions, ensure rocks on the bottom row
	np := geom.NewEmptyPlane[state]()
	for i, row := range p.Cells {
		if i == p.Height()-1 { // last row is rocks
			np.Append(append(append([]state{Rock}, row...), Rock))
		} else {
			np.Append(append(append([]state{Air}, row...), Air))
		}
	}
	return np
}

func main() {
	bs, _ := io.ReadAll(os.Stdin)
	paths := [][]geom.P2{}
	mmx, mmy := &maths.MinMax[int]{}, &maths.MinMax[int]{}
	for _, line := range bytes.Split(bs, []byte{'\n'}) {
		p2s := parsePath(line)
		slices.Each(p2s, func(idx int, p2 geom.P2) {
			mmx.Apply(p2.X)
			mmy.Apply(p2.Y)
		})
		paths = append(paths, p2s)
	}

	entryPt := geom.P2{X: 500 - mmx.Min, Y: 0}
	fmt.Printf("ENTRY %s\n", entryPt)
	bound := geom.P2{X: mmx.Max - mmx.Min + 1, Y: mmy.Max + 1}

	// recalibrate x relative to min(x)
	for i := 0; i < len(paths); i++ {
		paths[i] = slices.Map(paths[i], func(pt geom.P2) geom.P2 {
			return geom.P2{X: pt.X - mmx.Min, Y: pt.Y}
		})
	}

	// apply paths to plane
	p1 := geom.NewBoundedPlane[state](bound)
	for i := 0; i < len(paths); i++ {
		p1 = placeRocks(p1, paths[i])
	}

	bound = geom.P2{X: bound.X, Y: bound.Y + 2}
	p2 := geom.NewBoundedPlane[state](bound)
	paths = append(paths, []geom.P2{{X: 0, Y: bound.Y - 1}, {X: bound.X - 1, Y: bound.Y - 1}})
	for i := 0; i < len(paths); i++ {
		p2 = placeRocks(p2, paths[i])
	}

	// start pounding rock (dropping sand)
	part1 := poundRocks(p1, entryPt, nil)
	part2 := poundRocks(p2, entryPt, part2Expander)

	// TEST: part 1.a[1] (24)	part 2.a (93)
	// REAL: part 1 (799)	part 2 ()
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", part1, part2)
}

func printPlane(p *geom.Plane[state]) {
	bnd := p.Bounds()
	fmt.Printf("BOUND %s\n", bnd)
	p.WalkAll(func(s state, p2 geom.P2) bool {
		switch s {
		case Air:
			fmt.Print("  ")
		case Rock:
			fmt.Print("# ")
		case Sand:
			fmt.Print(". ")
		}
		if p2.X == bnd.X-1 {
			fmt.Println()
		}
		return true
	})
	fmt.Println()
}
