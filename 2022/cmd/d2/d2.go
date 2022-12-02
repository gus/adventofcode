package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

// 0=A (Rock), 1=B (Paper), 2=C (Scissors)

const decoder = "ABCXYZ"

var beats = map[int]int{
	0: 2, 1: 0, 2: 1,
}

func part1choices(them, me byte) (int, int) {
	return strings.IndexByte(decoder, them), strings.IndexByte(decoder, me) % 3
}

func part2choices(them, me byte) (int, int) {
	tc, mc := part1choices(them, me)
	// probably too clever
	return tc, (tc + (mc - 1)) % 3
}

func score(them, me int) int {
	s := me + 1
	if them == me {
		s += 3 // Draw
	} else if beats[me] == them {
		s += 6 // Win
	}
	return s
}

func main() {
	score1, score2 := 0, 0
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		txt := scanner.Text()
		score1 += score(part1choices(txt[0], txt[2]))
		score2 += score(part2choices(txt[0], txt[2]))
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", score1, score2)
}
