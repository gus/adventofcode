package main

import (
	"fmt"
	"log"
	"os"
	"sort"

	"github.com/gus/adventofcode/2020/internal/io"
)

func diffHist(a []int) map[int]int {
	last := int(0)
	hist := map[int]int{}
	for _, j := range a {
		d := j - last
		if _, ok := hist[d]; !ok {
			hist[d] = 0
		}
		hist[d]++
		last = j
	}
	return hist
}

func pathCount(a []int) int64 {
	edgeIdx := map[int][]int{}

	// graph adapter-edges for each adapter
	for i, j := range a {
		edgeIdx[j] = []int{}
		for ei := i + 1; ei < len(a) && a[ei]-j <= 3; ei++ {
			edgeIdx[j] = append(edgeIdx[j], a[ei])
		}
	}

	// memoize path count per adapter, reverse sum
	pathCtrIdx := map[int]int64{a[len(a)-1]: 1}
	for i := len(a) - 2; i >= 0; i-- {
		pathCtrIdx[a[i]] = int64(0)
		for _, e := range edgeIdx[a[i]] {
			pathCtrIdx[a[i]] += pathCtrIdx[e]
		}
	}
	return pathCtrIdx[0]
}

func main() {
	adapters := []int{0}
	scanner := io.NewIntScanner(os.Stdin)
	for scanner.Scan() {
		adapters = append(adapters, scanner.Int())
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	sort.Ints(adapters)
	adapters = append(adapters, adapters[len(adapters)-1]+3)

	part1 := diffHist(adapters[1:])
	fmt.Printf("part 1: %d\n", part1[1]*part1[3])
	fmt.Printf("part 2: %d\n", pathCount(adapters))
}
