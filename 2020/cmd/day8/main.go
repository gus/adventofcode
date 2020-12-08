package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// Op ...
type Op string

const (
	acc Op = "acc"
	jmp    = "jmp"
	nop    = "nop"
)

// Instruction ...
type Instruction struct {
	op  Op
	val int
}

func parseInstruction(line string) *Instruction {
	iparts := strings.Split(line, " ")
	val, err := strconv.Atoi(iparts[1])
	if err != nil {
		log.Fatalf("parsing value from instruction '%s': %v", line, err)
	}
	return &Instruction{Op(iparts[0]), val}
}

// main

func run(instructions []*Instruction) (int, bool) {
	reg := 0
	idx := 0

	visits := make([]bool, len(instructions))

	for idx < len(instructions) && !visits[idx] {
		in := instructions[idx]
		visits[idx] = true
		if in.op == acc {
			reg += in.val
			idx++
		} else if in.op == nop {
			idx++
		} else {
			idx += in.val
		}
	}
	return reg, idx == len(instructions)
}

func fix(instructions []*Instruction) int {
	// dumb, brute force approach. alternatively, could keep a trace of the indexes
	// touched while running the first run through and just alter those in reverse
	for i := 0; i < len(instructions); i++ {
		in := instructions[i]
		prevOp := in.op
		if in.op == acc {
			continue
		}

		if prevOp == jmp {
			in.op = nop
		} else {
			in.op = jmp
		}

		if reg, reachedLast := run(instructions); reachedLast {
			return reg
		}
		in.op = prevOp
	}
	panic("no solution found, actually")
}

func main() {
	instructions := make([]*Instruction, 0)

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		instructions = append(instructions, parseInstruction(scanner.Text()))
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	// part 1

	part1, _ := run(instructions)
	fmt.Printf("part 1: %d\n", part1)

	// part 2
	fmt.Printf("part 2: %d\n", fix(instructions))
}
