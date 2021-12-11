package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections/slices"
)

var (
	days = flag.Int("days", 80, "number of days to calc")
)

// every 7 days, each genx spawns a gen1
// every 9 days, each gen1 becomes a genx and spawns a gen1
func spawnCalc(days int) int {
	fishes := 1
	schedule := map[int]int{days: 1}

	for day := days; day >= 0; day-- {
		if f := schedule[day]; f > 0 {
			fishes += f
			schedule[day-7] += f
			schedule[day-9] += f
		}
	}
	return fishes
}

func main() {
	flag.Parse()

	scanner := bufio.NewScanner(os.Stdin)
	if !scanner.Scan() {
		panic("nothing to scan")
	}

	ages, _ := slices.Atoi(strings.Split(scanner.Text(), ","))
	sum := 0
	for _, age := range ages {
		sum += spawnCalc(*days - (age + 1))
	}
	fmt.Printf("fishes: %d\n", sum)
}
