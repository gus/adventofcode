package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections/slices"
	"github.com/gus/adventofcode/2021/internal/geom"
	"github.com/gus/adventofcode/2021/internal/maths"
	"github.com/gus/adventofcode/2021/internal/utils"
)

type fold struct {
	axis byte
	at   int
}

func foldy(p *geom.Plane[int8], at int) *geom.Plane[int8] {
	b := p.Bounds()
	p2 := geom.NewPlane[int8](geom.P2{X: b.X, Y: at})
	p.WalkAll(func(v int8, pt geom.P2) bool {
		if pt.Y == at {
			return false // ignore other rows
		}
		fv := p.Get(geom.P2{X: pt.X, Y: (b.Y - pt.Y - 1)})
		p2.Set(maths.Max(v, fv), pt)
		return true
	})
	return p2
}

func foldx(p *geom.Plane[int8], at int) *geom.Plane[int8] {
	b := p.Bounds()
	p2 := geom.NewPlane[int8](geom.P2{X: at, Y: b.Y})
	p.WalkAll(func(v int8, pt geom.P2) bool {
		if pt.X >= at {
			// ignore rest of row
			return true
		}
		fv := p.Get(geom.P2{X: (b.X - pt.X - 1), Y: pt.Y})
		p2.Set(maths.Max(v, fv), pt)
		return true
	})
	return p2
}

func print(p *geom.Plane[int8]) {
	p.WalkAll(func(v int8, pt geom.P2) bool {
		if pt.X == 0 {
			fmt.Println()
		}
		if v == 1 {
			fmt.Print("#")
		} else {
			fmt.Print(" ")
		}
		return true
	})
	fmt.Println()
}

func process(p *geom.Plane[int8], folds ...fold) *geom.Plane[int8] {
	p2 := p
	for _, f := range folds {
		if f.axis == 'y' {
			p2 = foldy(p2, f.at)
		} else {
			p2 = foldx(p2, f.at)
		}
	}
	return p2
}

func sumPlane(p *geom.Plane[int8]) int {
	sum := 0
	p.WalkAll(func(v int8, pt geom.P2) bool {
		sum += int(v)
		return true
	})
	return sum
}

func main() {
	points := []geom.P2{}
	folds := []fold{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		str := scanner.Text()
		if str == "" {
			break
		}
		pts, _ := slices.Atoi(strings.Split(str, ","))
		points = append(points, geom.P2{X: pts[0], Y: pts[1]})
	}
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), "=")
		axis := parts[0][len(parts[0])-1]
		folds = append(folds, fold{axis, utils.Atoi(parts[1])})
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	maxP2 := slices.Reduce(points, geom.P2{X: 0, Y: 0}, func(acc geom.P2, p geom.P2) geom.P2 {
		return geom.P2{X: maths.Max(acc.X, p.X), Y: maths.Max(acc.Y, p.Y)}
	})

	plane := geom.NewPlane[int8](geom.P2{X: maxP2.X + 1, Y: maxP2.Y + 1})
	slices.Each(points, func(i int, p geom.P2) { plane.Set(1, p) })
	p1, p2 := process(plane, folds[0]), process(plane, folds...)
	fmt.Printf("Part 1: %d\nPart 2:\n", sumPlane(p1))
	print(p2)
}

// Solution
// Part 1: 807
// Part 2:
//
// #     ##  #  # ####  ##  #  # ####   ##
// #    #  # #  # #    #  # #  # #       #
// #    #    #### ###  #    #  # ###     #
// #    # ## #  # #    # ## #  # #       #
// #    #  # #  # #    #  # #  # #    #  #
// ####  ### #  # ####  ###  ##  ####  ##
