package main

import (
	"fmt"

	"github.com/gus/adventofcode/2021/internal/geom"
)

func printPlane(p *geom.Plane[int], spaceEvery int) {
	b := p.Bounds()
	for y := 0; y < b.Y; y++ {
		if y > 0 && y%spaceEvery == 0 {
			fmt.Println()
		}
		for x := 0; x < b.X; x++ {
			if x > 0 && x%spaceEvery == 0 {
				fmt.Print(" ")
			}
			v := p.Get(geom.P2{X: x, Y: y})
			fmt.Printf("%d", v)
		}
		fmt.Println()
	}
}
