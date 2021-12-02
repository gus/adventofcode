package main

import (
	"log"
	"math"
	"os"

	"github.com/gus/adventofcode/2021/internal/io"
	"github.com/gus/adventofcode/2021/internal/slices"
)

func part1solution(inputs []int16) int {
	ctr := 0
	prev := int16(math.MaxInt16)

	for _, i := range inputs {
		if i > prev {
			ctr++
		}
		prev = i
	}
	return ctr
}

func part2solution(inputs []int16) int {
	ctr := 0
	prev := int16(math.MaxInt16)

	for i := 0; i < len(inputs)-2; i++ {
		g := slices.Sum(inputs[i : i+3])
		if g > prev {
			ctr++
		}
		prev = g
	}

	return ctr
}

func main() {
	inputs := []int16{}

	scanner := io.NewIntScanner(os.Stdin)
	for scanner.Scan() {
		inputs = append(inputs, scanner.Int16())
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	log.Printf("part 1 solution: %d increases\n", part1solution(inputs))
	log.Printf("part 2 solution: %d increases\n", part2solution(inputs))
}
