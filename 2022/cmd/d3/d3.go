package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
)

var priority = []byte("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

type sack []byte

func p(b byte) int {
	return bytes.IndexByte(priority, b) + 1
}

func findDupe(sck sack) byte {
	return slices.IntersectUniq(sck[:len(sck)/2], sck[len(sck)/2:])[0]
}

func findBadge(scks ...sack) byte {
	cmnsck := scks[0]
	for _, sck := range scks[1:] {
		cmnsck = slices.IntersectUniq(cmnsck, sck)
	}
	return cmnsck[0]
}

func main() {
	var scks = []sack{}
	dupeScore := 0

	var sck sack
	var err error
	rdr := bufio.NewReader(os.Stdin)
	for err == nil {
		sck, err = rdr.ReadBytes('\n')
		if sck[len(sck)-1] == '\n' {
			sck = sck[:len(sck)-1]
		}
		scks = append(scks, sck)
		dupeScore += p(findDupe(sck))
	}

	badgeScore := 0
	for i := 0; i < len(scks); i += 3 {
		badgeScore += p(findBadge(scks[i : i+3]...))
	}

	// TEST: part 1 (157)	part 2 (70)
	// REAL: part 1 (7727)	part 2 (2609)
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", dupeScore, badgeScore)
}
