package main

import (
	"bufio"
	"log"
	"os"
	"strconv"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections/slices"
)

type Point struct {
	x, y int
}

func splitPoint(pt string) (int, int) {
	xy := strings.Split(pt, ",")
	x, _ := strconv.Atoi(xy[0])
	y, _ := strconv.Atoi(xy[1])
	return x, y
}

func ParsePointSeq(p1, p2 string) ([]Point, bool) {
	x1, y1 := splitPoint(p1)
	x2, y2 := splitPoint(p2)
	pts := []Point{}
	isDiag := false
	if x1 == x2 { // vertical line
		for _, y := range slices.Sequence(y1, y2, true) {
			pts = append(pts, Point{x1, y})
		}
	} else if y1 == y2 { // horizontal line
		for _, x := range slices.Sequence(x1, x2, true) {
			pts = append(pts, Point{x, y1})
		}
	} else { // diagonal line
		isDiag = true
		xs := slices.Sequence(x1, x2, true)
		ys := slices.Sequence(y1, y2, true)
		for i := 0; i < len(xs); i++ {
			pts = append(pts, Point{xs[i], ys[i]})
		}
	}

	return pts, isDiag
}

type Plane struct {
	overlapCounts map[Point]int
}

func (p *Plane) AddPoints(pts []Point) {
	for _, pt := range pts {
		if _, ok := p.overlapCounts[pt]; !ok {
			p.overlapCounts[pt] = 1
		} else {
			p.overlapCounts[pt]++
		}
	}
}

func (p *Plane) OverlapCount(gte int) int {
	sum := 0
	for _, cnt := range p.overlapCounts {
		if cnt >= gte {
			sum++
		}
	}
	return sum
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	p1plane := &Plane{map[Point]int{}}
	p2plane := &Plane{map[Point]int{}}
	for scanner.Scan() {
		pts := strings.Split(scanner.Text(), " -> ")
		ptseq, isDiag := ParsePointSeq(pts[0], pts[1])
		if !isDiag {
			p1plane.AddPoints(ptseq)
		}
		p2plane.AddPoints(ptseq)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	log.Printf("part 1: %v\n", p1plane.OverlapCount(2))
	log.Printf("part 2: %v\n", p2plane.OverlapCount(2))
	// Example solutions: 5, 12
	// Data solutions: 4728, 17717
}
