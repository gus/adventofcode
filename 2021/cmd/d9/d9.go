package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections"
	"github.com/gus/adventofcode/2021/internal/collections/slices"
	"github.com/gus/adventofcode/2021/internal/geom"
)

func scanBasin(plane *geom.Plane[int], pt geom.P2) collections.Set[geom.P2] {
	set := collections.NewSet[geom.P2]()
	pcheck := func(h int, pt geom.P2) bool {
		if h < 9 {
			set.Add(pt)
		}
		return h < 9
	}
	vwalker := func(h int, pt geom.P2) bool {
		if h < 9 {
			plane.WalkLeft(pt, pcheck)
			plane.WalkRight(geom.P2{X: pt.X + 1, Y: pt.Y}, pcheck)
		}
		return h < 9
	}
	hwalker := func(h int, pt geom.P2) bool {
		if h < 9 {
			plane.WalkUp(pt, vwalker)
			plane.WalkDown(geom.P2{X: pt.X, Y: pt.Y + 1}, vwalker)
		}
		return h < 9
	}
	plane.WalkLeft(pt, hwalker)
	plane.WalkRight(geom.P2{X: pt.X + 1, Y: pt.Y}, hwalker)
	return set
}

func solve(plane *geom.Plane[int]) (p1 int, p2 int) {
	basins := []int{}
	plane.WalkAll(func(h int, pt geom.P2) bool {
		if !slices.Any(plane.LocalNeighbors(pt), func(npt geom.P2) bool { return plane.Get(npt) <= h }) {
			p1 += h + 1
			basins = append(basins, len(scanBasin(plane, pt)))
		}
		return true
	})
	sort.Ints(basins)
	p2 = slices.Reduce(basins[len(basins)-3:], 1, func(acc, x int) int { return acc * x })
	return p1, p2
}

func main() {
	plane := geom.NewEmptyPlane[int]()

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		row, _ := slices.Atoi(strings.Split(scanner.Text(), ""))
		plane.Append(row)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	p1, p2 := solve(plane)
	fmt.Printf("part 1: %d\npart 2: %d\n", p1, p2)
}

// part 1: 486
// part 2: 1059300
