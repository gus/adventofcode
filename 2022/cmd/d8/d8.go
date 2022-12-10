package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/geom"
	"github.com/gus/adventofcode/2022/internal/maths"
)

type dirWalkFn func(geom.P2, geom.WalkFn[int])

func isVisible(srcpt geom.P2, srch int, fn dirWalkFn) bool {
	v := true
	fn(srcpt, func(pth int, pt geom.P2) bool {
		if pt != srcpt && pth >= srch {
			v = false
		}
		return v
	})
	return v
}

func scenicScore(srcpt geom.P2, srch int, fn dirWalkFn) int {
	s := 0
	fn(srcpt, func(pth int, pt geom.P2) bool {
		if pt != srcpt {
			s++
		}
		return pt == srcpt || pth < srch
	})
	return s
}

func main() {
	p := geom.NewEmptyPlane[int]()
	buf := bufio.NewScanner(os.Stdin)
	for buf.Scan() {
		line := buf.Text()
		p.Append(slices.Atoi(strings.Split(line, "")))
	}
	if err := buf.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	part1 := 0                    // visible trees
	part2 := &maths.MinMax[int]{} // scenic scores
	dirWalkers := []dirWalkFn{p.WalkLeft, p.WalkRight, p.WalkUp, p.WalkDown}
	p.WalkAll(func(srch int, srcpt geom.P2) bool {
		if slices.Any(dirWalkers, func(fn dirWalkFn) bool { return isVisible(srcpt, srch, fn) }) {
			part1++
		}
		part2.Apply(slices.Reduce(dirWalkers, 1, func(acc int, fn dirWalkFn) int {
			return acc * scenicScore(srcpt, srch, fn)
		}))
		return true
	})

	// TEST: part 1 (21)	part 2 (8)
	// REAL: part 1 (1814)	part 2 (330786)
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", part1, part2.Max)
}
