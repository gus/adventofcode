package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"math"
	"os"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections"
	"github.com/gus/adventofcode/2021/internal/collections/slices"
	"github.com/gus/adventofcode/2021/internal/geom"
)

func solve(p *geom.Plane[int], steps int) (int, int) {
	flashes := 0
	var flashed collections.Set[geom.P2]

	changeId := p.OnChange(func(old, new int, pt geom.P2) {
		if new == 0 {
			flashed.Add(pt)
			slices.Each(p.Neighbors(pt), func(idx int, npt geom.P2) {
				if !flashed.Contains(npt) {
					p.Set((p.Get(npt)+1)%10, npt)
				}
			})
		}
	})
	defer p.ClearChange(changeId)

	for step := 0; step < steps; step++ {
		// reset flash check
		flashed = collections.NewSet[geom.P2]()
		p.WalkAll(func(lvl int, pt geom.P2) bool {
			if !flashed.Contains(pt) {
				p.Set((lvl+1)%10, pt)
			}
			return true
		})
		flashes += len(flashed)
		if len(flashed) == p.Size() { // flashes now synchronized
			return flashes, step + 1
		}
	}
	return flashes, 0
}

var (
	steps = flag.Int("steps", math.MaxInt, "number of steps to calculate")
)

func main() {
	flag.Parse()
	plane := geom.NewPlane[int]()

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		row, _ := slices.Atoi(strings.Split(scanner.Text(), ""))
		plane.Append(row)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	p1, p2 := solve(plane, *steps)
	fmt.Printf("part 1: %d\npart 2: %d\n", p1, p2)
}

// Example: part 1 (100 steps) = 1656, part 2 (unlimited steps) = 195
// Problem: part 1 (100 steps) = 1757, part 2 (unlimited steps) = 422
