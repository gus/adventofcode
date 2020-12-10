package main

import (
	"fmt"
	"log"
	"os"

	"github.com/gus/adventofcode/2020/internal/io"
	"github.com/gus/adventofcode/2020/internal/maps"
	"github.com/gus/adventofcode/2020/internal/slices"
)

func diffHist(a slices.Int64) maps.Int64 {
	last := int64(a[0])
	hist := maps.Int64{}
	for _, j := range a[1:] {
		hist.Incr(j-last, 1)
		last = j
	}
	return hist
}

func pathCount(a slices.Int64) int64 {
	edgeIdx := map[int64]slices.Int64{}

	// graph adapter-edges for each adapter
	for i, j := range a {
		edgeIdx[j] = slices.Int64{}
		for ei := i + 1; ei < len(a) && a[ei]-j <= 3; ei++ {
			edgeIdx[j] = edgeIdx[j].Append(a[ei])
		}
	}

	// memoize path count per adapter, reverse sum
	pathCtrIdx := maps.Int64{a.Get(-1): 1}
	for i := a.LastIndex() - 1; i >= 0; i-- {
		for _, e := range edgeIdx[a[i]] {
			pathCtrIdx.Incr(a[i], pathCtrIdx[e])
		}
	}
	return pathCtrIdx[0]
}

func main() {
	adapters := slices.Int64{0}
	scanner := io.NewIntScanner(os.Stdin)
	for scanner.Scan() {
		adapters = adapters.Append(scanner.Int64())
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	adapters = adapters.Sort().Append(adapters.Get(-1) + 3)

	part1 := diffHist(adapters)
	fmt.Printf("%v\n", part1)
	fmt.Printf("part 1: %d\n", part1[1]*part1[3])
	fmt.Printf("part 2: %d\n", pathCount(adapters))
}
