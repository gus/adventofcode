package main

import (
	"bufio"
	"log"
	"os"
	"sort"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/utils"
)

func main() {
	cals := sort.IntSlice{}
	curCals := 0

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		s := scanner.Text()
		if s == "" {
			cals = append(cals, curCals)
			curCals = 0
		} else {
			curCals += utils.Atoi(s)
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}
	cals = append(cals, curCals)

	sort.Sort(sort.Reverse(cals))
	log.Printf("part 1 solution: %d\n", cals[0])
	log.Printf("part 2 solution: %d\n", slices.Sum(cals[0:3]))
}
