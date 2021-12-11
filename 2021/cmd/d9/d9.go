package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections"
	"github.com/gus/adventofcode/2021/internal/geom"
	"github.com/gus/adventofcode/2021/internal/slices"
)

func scanBasin(grid geom.Plane[int], pt geom.Point) collections.Set[geom.Point] {
	set := collections.NewSet[geom.Point]()
	pcheck := func(h int, pt geom.Point) bool {
		if h < 9 {
			set.Add(pt)
		}
		return h < 9
	}
	vwalker := func(h int, pt geom.Point) bool {
		if h < 9 {
			grid.WalkLeft(pt, pcheck)
			grid.WalkRight(geom.Point{X: pt.X + 1, Y: pt.Y}, pcheck)
		}
		return h < 9
	}
	hwalker := func(h int, pt geom.Point) bool {
		if h < 9 {
			grid.WalkUp(pt, vwalker)
			grid.WalkDown(geom.Point{X: pt.X, Y: pt.Y + 1}, vwalker)
		}
		return h < 9
	}
	grid.WalkLeft(pt, hwalker)
	grid.WalkRight(geom.Point{X: pt.X + 1, Y: pt.Y}, hwalker)
	return set
}

func solve(grid geom.Plane[int]) (p1 int, p2 int) {
	basins := []int{}
	grid.WalkAll(func(h int, pt geom.Point) bool {
		if !slices.Any(grid.LocalNeighbors(pt), func(n int) bool { return n <= h }) {
			p1 += h + 1
			basins = append(basins, len(scanBasin(grid, pt)))
		}
		return true
	})
	sort.Ints(basins)
	p2 = slices.Reduce(basins[len(basins)-3:], 1, func(acc, x int) int { return acc * x })
	return p1, p2
}

func main() {
	// plane := Grid{}
	plane := geom.Plane[int]{}

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		row, _ := slices.Atoi(strings.Split(scanner.Text(), ""))
		plane = append(plane, row)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	p1, p2 := solve(plane)
	fmt.Printf("part 1: %d\npart 2: %d\n", p1, p2)
}

// part 1: 486
// part 2: 1059300
