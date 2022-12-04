package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
)

type Range []int

func (r Range) Contains(o Range) bool {
	return o[0] >= r[0] && o[1] <= r[1]
}

func (r Range) Overlaps(o Range) bool {
	return (r[0] >= o[0] && r[0] <= o[1]) || (r[1] >= o[0] && r[1] <= o[1])
}

func main() {
	score1, score2 := 0, 0
	scn := bufio.NewScanner(os.Stdin)
	for scn.Scan() {
		rp := slices.Map(strings.Split(scn.Text(), ","), func(rngStr string) Range {
			return Range(slices.Atoi(strings.Split(rngStr, "-")))
		})
		if rp[0].Contains(rp[1]) || rp[1].Contains(rp[0]) {
			score1++
		}
		if rp[0].Overlaps(rp[1]) || rp[1].Overlaps(rp[0]) {
			score2++
		}
	}

	// TEST: part 1 (2)	part 2 (4)
	// REAL: part 1 (576)	part 2 (905)
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", score1, score2)
}
