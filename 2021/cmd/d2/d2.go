package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func ParseInt(s string) int {
	x, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		panic(err)
	}
	return int(x)
}

// Part 1

type Part1 struct {
	h int
	d int
}

func (p *Part1) Forward(x int) {
	p.h += x
}

func (p *Part1) Sink(y int) {
	p.d += y
}

func (p *Part1) Position() int {
	return p.h * p.d
}

// Part 2

type Part2 struct {
	h   int
	d   int
	aim int
}

func (p *Part2) Forward(x int) {
	p.h += x
	p.d += p.aim * x
}

func (p *Part2) Aim(y int) {
	p.aim += y
}

func (p *Part2) Position() int {
	return p.h * p.d
}

// main

func main() {
	p1 := &Part1{0, 0}
	p2 := &Part2{0, 0, 0}

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		cmd := strings.Split(scanner.Text(), " ")
		n := ParseInt(cmd[1])
		switch cmd[0] {
		case "forward":
			p1.Forward(n)
			p2.Forward(n)
		case "down":
			p1.Sink(n)
			p2.Aim(n)
		case "up":
			p1.Sink(-n)
			p2.Aim(-n)
		default:
			panic(fmt.Errorf("unknown command: %s", cmd[0]))
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	log.Printf("part 1 solution: %d\n", p1.Position())
	log.Printf("part 2 solution: %d\n", p2.Position())
}
