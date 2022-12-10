package main

import (
	"bufio"
	"fmt"
	"log"
	"os"

	"github.com/gus/adventofcode/2022/internal/collections"
	"github.com/gus/adventofcode/2022/internal/geom"
	"github.com/gus/adventofcode/2022/internal/maths"
	"github.com/gus/adventofcode/2022/internal/utils"
)

type cmd struct {
	dir   byte
	steps int
}

func parseCmd(str string) cmd {
	return cmd{dir: str[0], steps: utils.Atoi(str[2:])}
}

func mv1(diff int) int {
	if diff == 0 {
		return 0
	} else if diff < 0 {
		return -1
	}
	return 1
}

type stepfn func(geom.P2) geom.P2

// a stepper moves a point one step in a direction
var steppers = map[byte]stepfn{
	'R': func(pt geom.P2) geom.P2 { return geom.P2{X: pt.X + 1, Y: pt.Y} },
	'L': func(pt geom.P2) geom.P2 { return geom.P2{X: pt.X - 1, Y: pt.Y} },
	'U': func(pt geom.P2) geom.P2 { return geom.P2{X: pt.X, Y: pt.Y - 1} },
	'D': func(pt geom.P2) geom.P2 { return geom.P2{X: pt.X, Y: pt.Y + 1} },
}

func followTail(cmds []cmd, links int) collections.Set[geom.P2] {
	H, Ts := geom.P2{}, make([]geom.P2, links)
	pts := collections.NewSet(Ts[links-1])

	for _, cmd := range cmds {
		for i := 0; i < cmd.steps; i++ { // for each step ...
			H = steppers[cmd.dir](H)  // perform command on HEAD
			h := H                    // note temporary head for each link to follow
			for link, t := range Ts { // for each link, follow the link in front of it
				if maths.Abs(h.X-t.X) == 2 || maths.Abs(h.Y-t.Y) == 2 {
					t = geom.P2{X: t.X + mv1(h.X-t.X), Y: t.Y + mv1(h.Y-t.Y)}
				}
				h, Ts[link] = t, t
			}
			pts.Add(Ts[links-1]) // note the tail's new position
		}
	}
	return pts
}

func main() {
	cmds := []cmd{}
	buf := bufio.NewScanner(os.Stdin)
	for buf.Scan() {
		cmds = append(cmds, parseCmd(buf.Text()))
	}
	if err := buf.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	// TEST: part 1.a (12)	part 2.a (1)	part 2.b (36)
	// REAL: part 1 (5883)	part 2 (2367)
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", len(followTail(cmds, 1)), len(followTail(cmds, 9)))
}
