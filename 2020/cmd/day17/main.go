package main

import (
	"bufio"
	"fmt"
	"log"
	"os"

	"github.com/gus/adventofcode/2020/internal/num"
)

// row

type row []byte

func newRow(size int) row {
	r := make(row, size, size)
	for i := 0; i < size; i++ {
		r[i] = '.'
	}
	return r
}

func (r row) copy() row {
	r2 := make(row, len(r), len(r))
	copy(r2, r)
	return r2
}

func (r row) expand() row {
	return []byte("." + string(r) + ".")
}

func (r row) String() string {
	s := ""
	for _, b := range r {
		s += " " + string(b)
	}
	return s
}

// plane

type plane []row

func newPlane(size int) plane {
	p := make(plane, size, size)
	for i := 0; i < size; i++ {
		p[i] = newRow(size)
	}
	return p
}

func (p plane) activeNeighbors(x, y int, excludeSelf bool) int {
	activeCnt := 0
	max := len(p) - 1
	for y2 := num.MaxInt(y-1, 0); y2 <= num.MinInt(y+1, max); y2++ {
		for x2 := num.MaxInt(x-1, 0); x2 <= num.MinInt(x+1, max); x2++ {
			if excludeSelf && x2 == x && y2 == y {
				continue
			} else if p[y2][x2] == '#' {
				activeCnt++
			}
		}
	}
	return activeCnt
}

func (p plane) copy() plane {
	p2 := make(plane, len(p), len(p))
	for i, r := range p {
		p2[i] = r.copy()
	}
	return p2
}

func (p plane) expand() plane {
	p2 := make(plane, len(p)+2, len(p)+2)
	p2[0] = newRow(len(p) + 2)
	p2[len(p)+1] = newRow(len(p) + 2)
	for i, r := range p {
		p2[i+1] = r.copy().expand()
	}
	return p2
}

func (p plane) String() string {
	s := ""
	for _, r := range p {
		s += fmt.Sprintf("%s\n", r.String())
	}
	return s
}

// cube

type cube []plane

func newCube(size, deep int) cube {
	c := make(cube, deep, deep)
	for i := 0; i < deep; i++ {
		c[i] = newPlane(size)
	}
	return c
}

func (c cube) activeNeighbors(x, y, z int, excludeSelf bool) int {
	activeNCnt := 0
	if z > 0 {
		activeNCnt += c[z-1].activeNeighbors(x, y, false) // previous plane
	}
	activeNCnt += c[z].activeNeighbors(x, y, excludeSelf) // this plane
	if z < len(c)-1 {                                     // next plane
		activeNCnt += c[z+1].activeNeighbors(x, y, false)
	}
	return activeNCnt
}

func (c cube) copy() cube {
	c2 := make(cube, len(c), len(c))
	for i, p := range c {
		c2[i] = p.copy()
	}
	return c2
}

func (c cube) expand() cube {
	c2 := make(cube, len(c)+2, len(c)+2)
	c2[0] = newPlane(len(c[0]) + 2)
	c2[len(c)+1] = newPlane(len(c[0]) + 2)
	for i, p := range c {
		c2[i+1] = p.copy().expand()
	}
	return c2
}

func (c cube) String() string {
	s := ""
	for _, p := range c {
		s += fmt.Sprintf("\n%s", p.String())
	}
	return s
}

// hyper cube

type hcube []cube

func (hc hcube) copy() hcube {
	hc2 := make(hcube, len(hc), len(hc))
	for i, c := range hc {
		hc2[i] = c.copy()
	}
	return hc2
}

func (hc hcube) expand() hcube {
	hc2 := make(hcube, len(hc)+2, len(hc)+2)
	hc2[0] = newCube(len(hc[0][0])+2, len(hc[0])+2)
	hc2[len(hc)+1] = newCube(len(hc[0][0])+2, len(hc[0])+2)
	for i, c := range hc {
		hc2[i+1] = c.copy().expand()
	}
	return hc2
}

func (hc hcube) String() string {
	s := ""
	for i, p := range hc {
		s += fmt.Sprintf("\nHCUBE %d\n%s", i, p.String())
	}
	return s
}

// main

func cubeCycle(c cube) (cube, int) {
	dst := c.copy()
	newActivePoints := 0
	for z := 0; z < len(c); z++ {
		p := c[z]
		for y := 0; y < len(p); y++ {
			r := p[y]
			for x := 0; x < len(r); x++ {
				activeNCnt := c.activeNeighbors(x, y, z, true)

				if c[z][y][x] == '#' && (activeNCnt < 2 || activeNCnt > 3) {
					dst[z][y][x] = '.'
				} else if c[z][y][x] == '.' && activeNCnt == 3 {
					dst[z][y][x] = '#'
				}

				if dst[z][y][x] == '#' {
					newActivePoints++
				}
			}
		}
	}
	return dst, newActivePoints
}

func hcubeCycle(hc hcube) (hcube, int) {
	dst := hc.copy()
	newActivePoints := 0
	for w := 0; w < len(hc); w++ {
		c := hc[w]
		for z := 0; z < len(c); z++ {
			p := c[z]
			for y := 0; y < len(p); y++ {
				r := p[y]
				for x := 0; x < len(r); x++ {
					activeNCnt := 0
					if w > 0 {
						activeNCnt += hc[w-1].activeNeighbors(x, y, z, false) // previous plane
					}
					activeNCnt += hc[w].activeNeighbors(x, y, z, true) // this plane
					if w < len(hc)-1 {                                 // next plane
						activeNCnt += hc[w+1].activeNeighbors(x, y, z, false)
					}

					if hc[w][z][y][x] == '#' && (activeNCnt < 2 || activeNCnt > 3) {
						dst[w][z][y][x] = '.'
					} else if hc[w][z][y][x] == '.' && activeNCnt == 3 {
						dst[w][z][y][x] = '#'
					}

					if dst[w][z][y][x] == '#' {
						newActivePoints++
					}
				}
			}
		}
	}
	return dst, newActivePoints
}

func main() {
	p := make(plane, 0)
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		p = append(p, row(scanner.Bytes()))
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	c := cube{newPlane(len(p)), p, newPlane(len(p))}
	hc := hcube{newCube(len(p), len(c)), c.copy(), newCube(len(p), len(c))}

	activeCubes := 0
	for i := 0; i < 6; i++ {
		c, activeCubes = cubeCycle(c.expand())
	}

	fmt.Printf("part 1: %d\n", activeCubes)

	activeHCubes := 0
	for i := 0; i < 6; i++ {
		hc, activeHCubes = hcubeCycle(hc.expand())
	}
	// fmt.Printf("%v\n", hc)
	fmt.Printf("part 1: %d\n", activeHCubes)
}
