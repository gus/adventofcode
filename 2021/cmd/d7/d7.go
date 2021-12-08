package main

import (
	"bufio"
	"flag"
	"fmt"
	"math"
	"os"
	"sort"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections"
	"github.com/gus/adventofcode/2021/internal/maths"
	"github.com/gus/adventofcode/2021/internal/slices"
)

func optimalPosition(crabs []int, scalefn func(x int) int) int {
	min := math.MaxInt
	collections.IterateRange(crabs[0], crabs[len(crabs)-1], true, func(pos int) {
		min = maths.Min(min, slices.Reduce(crabs, 0, func(acc, c int) int {
			return acc + scalefn(maths.Abs(c-pos))
		}))
	})

	return min
}

func main() {
	flag.Parse()

	scanner := bufio.NewScanner(os.Stdin)
	if !scanner.Scan() {
		panic("nothing to scan")
	}

	crabs, _ := slices.Atoi(strings.Split(scanner.Text(), ","))
	sort.Ints(crabs)

	fmt.Printf("part 1: %d\n", optimalPosition(crabs, func(x int) int { return x }))
	fmt.Printf("part 2: %d\n", optimalPosition(crabs, func(x int) int { return (x * (x + 1)) / 2 }))
}

// part 1: 351901
// part 2: 101079875
