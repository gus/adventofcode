package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections/slices"
	"github.com/gus/adventofcode/2021/internal/geom"
)

// thanks Dijkstra
func safestPath(g *geom.Plane[int], src geom.P2, dst geom.P2) int {
	q := PriorityQueue{}
	heap.Push(&q, &RiskState{risk: 0, pt: src, path: []geom.P2{}})
	curRisks := map[geom.P2]int{src: 0}

	for q.Len() > 0 {
		rs := heap.Pop(&q).(*RiskState)
		if rs.pt == dst { // short circuit, just for fun
			return rs.risk
		}

		for _, n := range g.LocalNeighbors(rs.pt) {
			risk := rs.risk + g.Get(n) // LocalNeighbors only returns P2s which exist
			if curRisk, ok := curRisks[n]; !ok || risk < curRisk {
				curRisks[n] = risk
				heap.Push(&q, &RiskState{risk: risk, pt: n, path: append(rs.path, rs.pt)})
			}
		}
	}

	return curRisks[dst] // shouldn't really ever get here, but okay
}

func growPlane(op *geom.Plane[int], times int) *geom.Plane[int] {
	ob := op.Bounds()
	b := geom.P2{X: ob.X * times, Y: ob.Y * times}
	p := geom.NewPlane[int](b)
	p.WalkAll(func(v int, pt geom.P2) bool {
		ov := op.Get(geom.P2{X: pt.X % ob.X, Y: pt.Y % ob.Y})
		p.Set((ov+pt.X/ob.X+pt.Y/ob.Y-1)%9+1, pt)
		return true
	})
	return p
}

func main() {
	p1 := geom.NewEmptyPlane[int]()
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		row, _ := slices.Atoi(strings.Split(scanner.Text(), ""))
		p1.Append(row)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	p1b := p1.Bounds()
	p1sol := safestPath(p1, geom.P2{X: 0, Y: 0}, geom.P2{X: p1b.X - 1, Y: p1b.Y - 1})
	fmt.Printf("\nPart 1: %d\n", p1sol)

	p2 := growPlane(p1, 5)
	// printPlane(p2, 10)
	p2b := p2.Bounds()
	p2sol := safestPath(p2, geom.P2{X: 0, Y: 0}, geom.P2{X: p2b.X - 1, Y: p2b.Y - 1})

	fmt.Printf("\nPart 2: %d\n", p2sol)
}

// Example
//   Part 1: 40
//   Part 2: 315
// Solution
//   Part 1: 619
//   Part 2: 2922
