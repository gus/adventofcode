package main

import (
	"fmt"
	"log"
	"os"

	"github.com/gus/adventofcode/2020/internal/io"
	"github.com/gus/adventofcode/2020/internal/maps"
	"github.com/gus/adventofcode/2020/internal/slices"
)

func diffHistProduct(a slices.Int64) int64 {
	last := a[0]
	hist := maps.Int64{}
	for _, j := range a[1:] {
		hist.Incr(j-last, 1)
		last = j
	}
	return hist.Get(1, 0) * hist.Get(3, 0)
}

func pathCount(a slices.Int64) int64 {
	pathCtrIdx := maps.Int64{0: 1}
	for _, j := range a[1:] {
		pathCtrIdx.Incr(j, pathCtrIdx.Get(j-1, 0)+pathCtrIdx.Get(j-2, 0)+pathCtrIdx.Get(j-3, 0))
	}
	return pathCtrIdx.Get(a.Get(-1), 0)
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

	fmt.Printf("part 1: %d\n", diffHistProduct(adapters))
	fmt.Printf("part 2: %d\n", pathCount(adapters))
}
