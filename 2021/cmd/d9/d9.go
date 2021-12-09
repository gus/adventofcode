package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"math"
	"os"
	"sort"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections"
	"github.com/gus/adventofcode/2021/internal/slices"
)

type WalkFn func(h, x, y int) bool

type Grid [][]int

func (g Grid) Size() (int, int) {
	if len(g) == 0 {
		return 0, 0
	}
	return len(g[0]), len(g)
}

func (g Grid) Neighbors(x, y int) []int {
	w, h := g.Size()
	ns := []int{}
	if x > 0 {
		ns = append(ns, g[y][x-1])
	}
	if x < w-1 {
		ns = append(ns, g[y][x+1])
	}
	if y > 0 {
		ns = append(ns, g[y-1][x])
	}
	if y < h-1 {
		ns = append(ns, g[y+1][x])
	}
	return ns
}

func (g Grid) walk(x, y int, xstep, ystep int, fn WalkFn) {
	w, h := g.Size()
	for y := y; y < h && y >= 0; y += ystep {
		for x := x; x < w && x >= 0; x += xstep {
			if !fn(g[y][x], x, y) {
				return
			}
		}
	}
}

func (g Grid) WalkAll(fn WalkFn) {
	g.walk(0, 0, 1, 1, fn)
}

func (g Grid) WalkLeft(x, y int, fn WalkFn) {
	g.walk(x, y, -1, math.MaxInt, fn)
}

func (g Grid) WalkRight(x, y int, fn WalkFn) {
	g.walk(x, y, 1, math.MaxInt, fn)
}

func (g Grid) WalkUp(x, y int, fn WalkFn) {
	g.walk(x, y, math.MaxInt, -1, fn)
}

func (g Grid) WalkDown(x, y int, fn WalkFn) {
	g.walk(x, y, math.MaxInt, 1, fn)
}

type P struct {
	x int
	y int
}

func scanBasin(grid Grid, x, y int) collections.Set[P] {
	set := collections.NewSet[P]()
	pcheck := func(h, x, y int) bool {
		if h < 9 {
			set.Add(P{x, y})
		}
		return h < 9
	}
	vwalker := func(h, x, y int) bool {
		if h < 9 {
			grid.WalkLeft(x, y, pcheck)
			grid.WalkRight(x+1, y, pcheck)
		}
		return h < 9
	}
	hwalker := func(h, x, y int) bool {
		if h < 9 {
			grid.WalkUp(x, y, vwalker)
			grid.WalkDown(x, y+1, vwalker)
		}
		return h < 9
	}
	grid.WalkLeft(x, y, hwalker)
	grid.WalkRight(x+1, y, hwalker)
	return set
}

func solve(grid Grid) (p1 int, p2 int) {
	basins := []int{}
	grid.WalkAll(func(h, x, y int) bool {
		if !slices.Any(grid.Neighbors(x, y), func(n int) bool { return n <= h }) {
			p1 += h + 1
			basins = append(basins, len(scanBasin(grid, x, y)))
		}
		return true
	})
	sort.Ints(basins)
	p2 = slices.Reduce(basins[len(basins)-3:], 1, func(acc, x int) int { return acc * x })
	return p1, p2
}

func main() {
	flag.Parse()

	grid := Grid{}

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		row, _ := slices.Atoi(strings.Split(scanner.Text(), ""))
		grid = append(grid, row)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	p1, p2 := solve(grid)
	fmt.Printf("part 1: %d\npart 2: %d\n", p1, p2)
}

// part 1: 486
// part 2: 1059300
