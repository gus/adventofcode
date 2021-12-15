package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"math"
	"os"
	"strings"

	"github.com/gus/adventofcode/2021/internal/maths"
)

type Pair struct {
	A byte
	B byte
}

func (p Pair) String() string {
	return fmt.Sprintf("{%c%c}", p.A, p.B)
}

type PairMap map[Pair]byte

type PairDist map[Pair]int

func (d PairDist) String() string {
	str := ""
	for k, v := range d {
		str += fmt.Sprintf("%s -> %d\n", k, v)
	}
	return str
}

type ElemDist map[byte]int

func (d ElemDist) String() string {
	str := ""
	for k, v := range d {
		str += fmt.Sprintf("%c -> %d\n", k, v)
	}
	return str
}

func solve(pdist PairDist, edist ElemDist, pmap PairMap, steps int) {
	for step := 0; step < steps; step++ {
		npdist := PairDist{}
		for pair := range pdist {
			elem, times := pmap[pair], pdist[pair]
			npdist[Pair{pair.A, elem}] += times
			npdist[Pair{elem, pair.B}] += times
			edist[elem] += times
		}
		pdist = npdist
	}
}

func initPolymer(seq string) (PairDist, ElemDist) {
	pdist := PairDist{}
	edist := ElemDist{}
	for i := 0; i < len(seq)-1; i++ {
		pdist[Pair{seq[i], seq[i+1]}] += 1
		edist[seq[i]]++
	}
	edist[seq[len(seq)-1]]++
	return pdist, edist
}

func maxmin(d ElemDist) int {
	min, max := math.MaxInt, math.MinInt
	for _, c := range d {
		min = maths.Min(min, c)
		max = maths.Max(max, c)
	}
	return max - min
}

var (
	steps = flag.Int("steps", 1, "number of steps to run")
)

func main() {
	flag.Parse()

	scanner := bufio.NewScanner(os.Stdin)
	if !scanner.Scan() {
		panic("unable to read input")
	}
	pdist, edist := initPolymer(scanner.Text())
	cmap := PairMap{}
	scanner.Scan() // skip blank line
	for scanner.Scan() {
		cpair := strings.Split(scanner.Text(), " -> ")
		cmap[Pair{cpair[0][0], cpair[0][1]}] = cpair[1][0]
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	solve(pdist, edist, cmap, *steps)
	fmt.Printf("Solution for %d steps: %d\n", *steps, maxmin(edist))
}

// Example
//   Part 1: 1588 (10 steps)
//   Part 2: 2188189693529 (40 steps)
// Solution
//   Part 1: 2587 (10 steps)
//   Part 2: 3318837563123 (40 steps)
