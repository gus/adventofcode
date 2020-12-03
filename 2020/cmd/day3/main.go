package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

const tree = '#'

func treesInPath(xinc, yinc int, slope []string) int {
	llen := len(slope[0])
	xpos := 0
	ctr := 0
	for ypos := yinc; ypos < len(slope); ypos += yinc {
		xpos = (xpos + xinc) % llen
		if slope[ypos][xpos] == tree {
			ctr++
		}
	}
	return ctr
}

func main() {
	lines := make([]string, 0)
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	// part 1
	fmt.Printf("part 1: %d\n", treesInPath(3, 1, lines))

	// part 2
	p1 := treesInPath(1, 1, lines)
	p2 := treesInPath(3, 1, lines)
	p3 := treesInPath(5, 1, lines)
	p4 := treesInPath(7, 1, lines)
	p5 := treesInPath(1, 2, lines)

	fmt.Printf("part 2: [%d %d %d %d %d]: %d\n", p1, p2, p3, p4, p5, p1*p2*p3*p4*p5)
}
