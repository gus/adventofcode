package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"regexp"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/geom"
	"github.com/gus/adventofcode/2022/internal/maths"
	"github.com/gus/adventofcode/2022/internal/utils"
)

type state int

const (
	Out state = iota
	In
	Signal
	Beacon
)

type cell struct {
	state
	md int
}

func (c cell) String() string {
	return fmt.Sprintf("cell{%d, %d}", c.state, c.md)
}

// ex. "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
var linePtn = regexp.MustCompile("-?[0-9]+")

func fillDiamond(p geom.Plane[cell], pt geom.P2, md int) geom.Plane[cell] {
	// apply a diamond pattern for the signal range with
	// a radius (including center)
	drawIn := func(_ int, npt geom.P2) {
		if p.Get(npt).state == Out {
			p.Set(cell{state: In}, npt)
		}
	}
	xr, yr := md+1, md+1
	for y := 0; y < yr; y++ {
		for x := 0; x < xr; x++ {
			lu := geom.P2{X: pt.X - x, Y: pt.Y - y}
			ru := geom.P2{X: pt.X - x, Y: pt.Y + y}
			ld := geom.P2{X: pt.X + x, Y: pt.Y - y}
			rd := geom.P2{X: pt.X + x, Y: pt.Y + y}
			slices.Each([]geom.P2{lu, ru, ld, rd}, drawIn)
		}
		xr--
	}
	return p
}

// func updateSignalRange(p geom.Plane[cell]) geom.Plane[cell] {
func updateSignalRange(p geom.Plane[cell]) geom.Plane[cell] {
	newp := p.Copy()

	// fmt.Printf("# PRE CHECK %s -> %s\n", newp.Min(), newp.Max())
	geom.WalkPlane(newp, func(c cell, pt geom.P2) bool {
		if c.state == Signal {
			fillDiamond(newp, pt, c.md)
		}
		return true
	})
	return newp
}

func countStateInRow(p geom.Plane[cell], s state, y int) int {
	fmt.Printf("BOUNDS %s -> %s\n", p.Min(), p.Max())
	printPlane(p)
	cnt := 0
	mnx, mxx := p.Min().X, p.Max().X
	for x := mnx; x <= mxx; x++ {
		if c := p.Get(geom.P2{X: x, Y: y}); c.state == s {
			// fmt.Printf("%s -> %s\n", geom.P2{X: x, Y: y}, c)
			cnt++
		}
	}
	return cnt
}

func main() {
	p := geom.NewUnboundedPlane[cell]()
	bs, _ := io.ReadAll(os.Stdin)
	for _, line := range bytes.Split(bs, []byte{'\n'}) {
		nums := slices.Atoi(linePtn.FindAllString(string(line), -1))
		sigpt := geom.P2{X: nums[0], Y: nums[1]}
		bcnpt := geom.P2{X: nums[2], Y: nums[3]}
		// manhattan distance
		md := maths.Abs(sigpt.X-bcnpt.X) + maths.Abs(sigpt.Y-bcnpt.Y)
		p.Set(cell{state: Signal, md: md}, sigpt)
		p.Set(cell{state: Beacon}, bcnpt)
	}

	part1 := countStateInRow(updateSignalRange(p), In, utils.Atoi(os.Args[1]))

	// TEST: part 1.a (26)	part 2.a ()
	// this doesn't scale for real data, but it was fun
	// REAL: part 1 ()	part 2 ()
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", part1, 0)
}

func printPlane(p geom.Plane[cell]) {
	minb, maxb := p.Min(), p.Max()
	fmt.Printf("BOUND %s -> %s\n", minb, maxb)
	for x := minb.X; x <= maxb.X; x++ {
		if x >= 0 && x%10 == 0 {
			fmt.Print("0")
		} else {
			fmt.Print(" ")
		}
	}
	fmt.Println("")
	geom.WalkPlane(p, func(c cell, pt geom.P2) bool {
		switch c.state {
		case Out:
			fmt.Print(".")
		case In:
			fmt.Print("#")
		case Signal:
			fmt.Print("S")
		case Beacon:
			fmt.Print("B")
		}
		if pt.X == maxb.X {
			fmt.Printf(" %d\n", pt.Y)
		}
		return true
	})
	fmt.Println()
}
