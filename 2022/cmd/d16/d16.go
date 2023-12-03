package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"regexp"
	"sort"

	"github.com/gus/adventofcode/2022/internal/collections/maps"
	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/maths"
	"github.com/gus/adventofcode/2022/internal/utils"
)

// ex. "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
var valvePtn = regexp.MustCompile("[A-Z]{2}")
var ratePtn = regexp.MustCompile("[0-9]+")

type valve struct {
	id    string
	rate  int
	conns []string
}

type vmap map[string]*valve

func shortestPath(valves vmap, from, to string) int {
	parents := map[string]*string{from: nil} // track parents and visits

	q := []string{from}
	for len(q) > 0 {
		cur := q[0] // pop
		q = q[1:]
		if cur == to { // done
			break
		}
		for _, nxt := range valves[cur].conns {
			if _, v := parents[nxt]; !v {
				parents[nxt] = &cur // mark visited as well as next P2 parent
				q = append(q, nxt)
			}
		}
	}
	steps := slices.NewStack(to)
	for wn := parents[to]; wn != nil; wn = parents[*wn] {
		steps.Push(*wn)
	}
	return len(steps) - 1
}

func maxPath(valves vmap, from string, unopen []string, clock int) int {
	// slow and brutish
	if len(unopen) == 0 || clock < 0 {
		return 0
	}
	mm := &maths.MinMax[int]{}
	for _, to := range unopen {
		t := clock - shortestPath(valves, from, to) - 1 // costs N ticks to walk and 1 tick to open
		if t > 0 {
			mm.Apply(valves[to].rate*t + maxPath(valves, to, slices.Remove(unopen, to), t))
		}
	}
	return mm.Max
}

func part1(valves vmap) int {
	return maxPath(valves, "AA", slices.Filter(maps.Keys(valves), func(id string) bool {
		return valves[id].rate > 0
	}), 30)
}

func main() {
	valves := vmap{}
	bs, _ := io.ReadAll(os.Stdin)
	for _, line := range bytes.Split(bs, []byte{'\n'}) {
		names := valvePtn.FindAllString(string(line), -1)
		valves[names[0]] = &valve{names[0], utils.Atoi(ratePtn.FindString(string(line))), names[1:]}
	}
	fmt.Printf("VALVE MAP %v\n", valves)

	// TEST: part 1 (1651)	part 2 (1707)
	// REAL: part 1 (2320)	part 2 ()
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", part1(valves), 0)
}

func part1a(valves vmap, id string, clock int) int {
	unopen := maps.Keys(valves)
	sum := 0
	unopen = slices.Filter(unopen, func(s string) bool { return valves[s].rate > 0 })
	for clock > 0 && len(unopen) != 0 {
		fmt.Printf("# T-%d S-%d / @%s UNOPEN %v\n", clock, sum, id, unopen)
		v := valves[id]

		// find closest, unopened valve with highest rate from here (weight=pressure-distance)
		mm := &maths.MinMax[int]{}
		weights := map[string]int{}
		steps := map[string]int{}
		for _, to := range unopen {
			if valves[to].rate > 0 {
				steps[to] = shortestPath(valves, v.id, to)
				weights[to] = valves[to].rate * (clock - steps[to] - 1)
				// mm.Apply(steps[to])
				mm.Apply(weights[to])
				fmt.Printf("\tfrom %s to %s in %d steps for %d weight\n", v.id, to, steps[to], weights[to])
			}
		}
		// // find valve ids with shortest distance
		// choices := slices.Filter(unopen, func(s string) bool { return steps[s] == mm.Min })
		// // sort by weight
		// sort.Slice(choices, func(i, j int) bool {
		// 	return weights[choices[i]] > weights[choices[j]]
		// })
		// find valve ids with most weight
		choices := slices.Filter(maps.Keys(weights), func(s string) bool { return weights[s] == mm.Max })
		// sort by number of steps (prefer fewer)
		sort.Slice(choices, func(i, j int) bool {
			return steps[choices[i]] < steps[choices[j]]
		})

		if len(choices) == 0 {
			break
		}
		id = choices[0]
		clock -= steps[id] + 1
		fmt.Printf("\tOPEN %v, T-%d\n", id, clock)
		sum += weights[id]
		// pop valve
		unopen = slices.Filter(unopen, func(s string) bool { return s != id })
	}
	return sum
}
