package main

// brute force implementation

import (
	"fmt"
	"log"
	"os"

	"github.com/gus/adventofcode/2020/internal/io"
)

const expectedSum int16 = 2020

func usage(errMsg string) {
	fmt.Printf("! %s\n\nusage: %s [--three]", errMsg, os.Args[0])
	os.Exit(1)
}

func product(inputs []int16) int64 {
	var p int64 = 1
	for _, i := range inputs {
		p = p * int64(i)
	}
	return p
}

type solverFunc func([]int16) []int16

// FindTwoSolver finds a solution for two inputs
func FindTwoSolver(inputs []int16) []int16 {
	for _, i := range inputs[:len(inputs)-1] {
		for _, j := range inputs[1:] {
			if i+j == expectedSum {
				return []int16{i, j}
			}
		}
	}
	return []int16{}
}

// FindThreeSolver finds a solution for three inputs
func FindThreeSolver(inputs []int16) []int16 {
	for _, i := range inputs[:len(inputs)-2] {
		for _, j := range inputs[1 : len(inputs)-1] {
			for _, k := range inputs[2:] {
				if i+j+k == expectedSum {
					return []int16{i, j, k}
				}
			}
		}
	}
	return []int16{}
}

func main() {
	solve := FindTwoSolver

	args := os.Args[1:]
	if len(args) > 1 {
		usage("too many arguments")
	} else if len(args) == 1 {
		if args[0] != "--three" {
			usage(fmt.Sprintf("unexpected argument: %s", args[0]))
		}
		solve = FindThreeSolver
	}

	inputs := make([]int16, 0)
	scanner := io.NewInt16Scanner(os.Stdin)
	for scanner.Scan() {
		inputs = append(inputs, scanner.Int16())
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	sol := solve(inputs)
	if len(sol) == 0 {
		log.Println("no solution found")
		os.Exit(1)
	} else {
		log.Printf("solution: %v = %d\n", sol, product(sol))
	}
}
