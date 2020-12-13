package main

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
	"strconv"

	"github.com/gus/adventofcode/2020/internal/num"
)

// Dir ...
type Dir int

var directions = []byte("NESWRLF")

func (d Dir) String() string {
	return string(directions[d])
}

func DirFromByte(v byte) Dir {
	return Dir(bytes.IndexByte(directions, v))
}

const (
	N Dir = iota
	E
	S
	W
	R
	L
	F
)

type instruction struct {
	dir   Dir
	amt   int
	angle bool
}

func (i instruction) String() string {
	return fmt.Sprintf("%v->%d", i.dir, i.amt)
}

func parseInstruction(bytes []byte) instruction {
	if amt, err := strconv.Atoi(string(bytes[1:])); err == nil {
		d := DirFromByte(bytes[0])
		if d == L {
			amt = -amt
		}
		return instruction{d, amt, d == R || d == L}
	}
	panic(string(bytes) + " not a valid instruction")
}

type wayPoint struct {
	x, y int
}

func (wp *wayPoint) move(d Dir, amt int) {
	switch d {
	case N:
		wp.y += amt
	case S:
		wp.y -= amt
	case E:
		wp.x += amt
	case W:
		wp.x -= amt
	}
}

func (wp *wayPoint) swing(angle int) {
	absAngle := (angle + 360) % 360
	newx, newy := -wp.x, -wp.y // handles 180
	if absAngle == 180 {
		wp.x, wp.y = newx, newy
		return
	} else if absAngle == 90 {
		newy = -newy // undo the negation
	} else if absAngle == 270 {
		newx = -newx // undo the negation
	}
	wp.x, wp.y = newy, newx
}

func (wp wayPoint) String() string {
	return fmt.Sprintf("[%d, %d]", wp.x, wp.y)
}

type dirPoint struct {
	*wayPoint
	dir Dir
}

func (dp dirPoint) manhattanNumber() int {
	return num.AbsInt(dp.x) + num.AbsInt(dp.y)
}

func (dp *dirPoint) turn(angle int) {
	dp.dir = Dir((int(dp.dir) + 4 + angle/90) % 4)
}

func (dp dirPoint) String() string {
	return fmt.Sprintf("%v->[%d, %d]", dp.dir, dp.x, dp.y)
}

func processInstruction(dp *dirPoint, ins instruction) {
	if ins.angle {
		dp.turn(ins.amt)
	} else {
		d := ins.dir
		if d == F {
			d = dp.dir
		}
		dp.move(d, ins.amt)
	}
}

func processInstructionWithWaypoint(dp *dirPoint, wp *wayPoint, ins instruction) {
	if ins.angle {
		wp.swing(ins.amt)
	} else if ins.dir == F {
		dp.x, dp.y = dp.x+wp.x*ins.amt, dp.y+wp.y*ins.amt
	} else { // N,E,S,W
		wp.move(ins.dir, ins.amt)
	}
}

func main() {
	instructions := []instruction{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		instructions = append(instructions, parseInstruction(scanner.Bytes()))
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	ship1 := &dirPoint{&wayPoint{0, 0}, E}
	for _, ins := range instructions {
		processInstruction(ship1, ins)
	}
	fmt.Printf("part 1: %d\n", ship1.manhattanNumber())

	ship2 := &dirPoint{&wayPoint{0, 0}, E}
	wp2 := &wayPoint{10, 1}

	for _, ins := range instructions {
		processInstructionWithWaypoint(ship2, wp2, ins)
	}

	fmt.Printf("part 2: %d\n", ship2.manhattanNumber())
}
