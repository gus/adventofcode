package main

import (
	"bytes"
	"fmt"
	"io"
	"os"

	"github.com/gus/adventofcode/2022/internal/geom"
	"github.com/gus/adventofcode/2022/internal/maths"
)

// H maps a byte to a height (makes the S and E nicer for debugging and comparisons)
var H = map[byte]int{'S': int('a'), 'E': int('z')}

func init() {
	for c := byte('a'); c <= 'z'; c++ {
		H[c] = int(c)
	}
}

func hasValidHeight(ch, nh byte) bool {
	return H[nh] <= H[ch]+1
}

func part1(p *geom.Plane[byte]) int {
	S, _ := p.LocateFirst(func(h byte) bool { return h == 'S' })
	E, _ := p.LocateFirst(func(h byte) bool { return h == 'E' })
	return len(p.ShortestLocalPath(S, E, hasValidHeight)) - 1
}

func part2(p *geom.Plane[byte]) int {
	starters := []geom.P2{}
	p.WalkAll(func(b byte, pt geom.P2) bool { // find the starting P2s
		if H[b] == H['a'] {
			starters = append(starters, pt)
		}
		return true
	})

	mm := &maths.MinMax[int]{}
	E, _ := p.LocateFirst(func(t byte) bool { return t == 'E' })
	for _, S := range starters {
		if path := p.ShortestLocalPath(S, E, hasValidHeight); len(path) > 1 {
			mm.Apply(len(path) - 1)
		}
	}
	return mm.Min
}

func main() {
	bs, _ := io.ReadAll(os.Stdin)
	p := geom.NewPlane(bytes.Split(bs, []byte{'\n'}))
	// TEST: part 1.a (31)	part 2.a (29)
	// REAL: part 1 (462)	part 2 (451)
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", part1(p), part2(p))
}
