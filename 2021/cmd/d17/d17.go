package main

import (
	"flag"
	"fmt"
	"math"
	"os"

	"github.com/gus/adventofcode/2021/internal/collections/slices"
	"github.com/gus/adventofcode/2021/internal/geom"
	"github.com/gus/adventofcode/2021/internal/maths"
)

type TargetArea struct {
	x0, y0 int
	x1, y1 int
}

var (
	taidx = flag.Int("ta", 0, "indicates which target area to use (0={20..30, -10..-5}, 1={57..116, -198..-148}")
)

var targetAreas = []TargetArea{
	{20, -10, 30, -5},
	{57, -198, 116, -148},
}

func hasValidPath(vel geom.P2, ta TargetArea) bool {
	x, y := vel.XY()
	diffx, diffy := vel.XY()
	for x <= ta.x1 && y >= ta.y0 { // yes, assumes x > 0 and y < 0
		if maths.Between(x, ta.x0, ta.x1) && maths.Between(y, ta.y0, ta.y1) {
			return true
		}
		diffx, diffy = maths.Max(0, diffx-1), diffy-1
		x, y = x+diffx, y+diffy
	}
	return false
}

func validVelocities(ta TargetArea) []geom.P2 {
	vels := []geom.P2{}
	for x := 1; x <= ta.x1; x++ {
		for y := ta.y0; y <= maths.Abs(ta.y0); y++ {
			vel := geom.P2{X: x, Y: y}
			if hasValidPath(vel, ta) {
				vels = append(vels, vel)
			}
		}
	}
	return vels
}

func main() {
	flag.Parse()
	if *taidx >= len(targetAreas) {
		fmt.Fprintf(os.Stderr, "! ERR you must use a --ta between [0] and (%d)\n", len(targetAreas))
		os.Exit(1)
	}
	ta := targetAreas[*taidx]
	fmt.Printf("# using target area %+v\n", ta)
	vels := validVelocities(ta)
	maxy := slices.Reduce(vels, math.MinInt, func(max int, pt geom.P2) int { return maths.Max(max, pt.Y) })
	fmt.Printf("Part 1: %d\n", maxy*(maxy+1)/2)
	fmt.Printf("Part 2: %d\n", len(vels))
}

// Example
//   Part 1: 45
//   Part 2: 112
// Solution
//   Part 1: 19503
//   Part 2: 5200
