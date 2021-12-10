package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"

	"github.com/gus/adventofcode/2021/internal/slices"
)

var corruptScores = map[byte]int{
	')': 3,
	']': 57,
	'}': 1197,
	'>': 25137,
}

var completionScores = map[byte]int{
	')': 1,
	']': 2,
	'}': 3,
	'>': 4,
}

var closers = map[byte]byte{
	'(': ')',
	'[': ']',
	'{': '}',
	'<': '>',
}

func p1scan(line string) (int, int) {
	expects := slices.NewStack[byte]()
	for i := 0; i < len(line); i++ {
		b := line[i]
		if closer, isOpener := closers[b]; isOpener {
			expects.Push(closer)
		} else if closer, ok := expects.Pop(); ok && closer != b {
			return corruptScores[b], 0
		} else if !ok {
			panic("whoops! too many closers in input")
		}
	}
	return 0, int(slices.Reduce(expects.Slice(), 0, func(acc int, elem byte) int {
		return acc*5 + completionScores[elem]
	}))
}

func main() {
	p1, p2scores := 0, []int{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		p1score, p2score := p1scan(scanner.Text())
		p1 += p1score
		if p2score > 0 {
			p2scores = append(p2scores, p2score)
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	sort.Ints(p2scores)
	fmt.Printf("part 1: %d\npart 2: %d\n", p1, p2scores[len(p2scores)/2])
}

// part 1: 462693
// part 2: 3094671161
