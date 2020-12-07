package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type group struct {
	respondents int
	answers     map[byte]int
}

func newGroup() *group {
	return &group{0, map[byte]int{}}
}

func (g *group) addSet(answerSet string) {
	g.respondents++
	for _, c := range []byte(answerSet) {
		if _, ok := g.answers[c]; !ok {
			g.answers[c] = 0
		}
		g.answers[c]++
	}
}

func (g *group) allAnswers() int {
	return len(g.answers)
}

func (g *group) commonAnswers() int {
	yctr := 0
	for _, y := range g.answers {
		if y == g.respondents {
			yctr++
		}
	}
	return yctr
}

// main

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	curGroup := newGroup()
	anyYesCtr := 0
	commonYesCtr := 0
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			anyYesCtr += curGroup.allAnswers()
			commonYesCtr += curGroup.commonAnswers()
			curGroup = newGroup()
			continue
		}
		curGroup.addSet(line)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	fmt.Printf("part 1: %d\n", anyYesCtr+curGroup.allAnswers())
	fmt.Printf("part 2: %d\n", commonYesCtr+curGroup.commonAnswers())
}
