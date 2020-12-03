package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

const tree = '#'

func treesInPath(llen, xinc, yinc int, slope []string) int {
	xpos := 0
	ypos := 0
	ctr := 0
	for _, line := range slope {
		ypos++
		if ypos == yinc {
			xpos = (xpos + xinc) % llen
			if line[xpos] == tree {
				ctr++
			}
			ypos = 0
		}
	}
	return ctr
}

func main() {
	llen := -1
	lines := make([]string, 0)
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		if llen == -1 {
			llen = len(line)
			continue
		}
		lines = append(lines, line)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	// part 1
	fmt.Printf("part 1: trees encountered: %d", treesInPath(llen, 3, 1, lines))

	// part 2
	p1 := treesInPath(llen, 1, 1, lines)
	p2 := treesInPath(llen, 3, 1, lines)
	p3 := treesInPath(llen, 5, 1, lines)
	p4 := treesInPath(llen, 7, 1, lines)
	p5 := treesInPath(llen, 1, 2, lines)

	fmt.Printf("part 2: [%d %d %d %d %d]: %d", p1, p2, p3, p4, p5, p1*p2*p3*p4*p5)
}
