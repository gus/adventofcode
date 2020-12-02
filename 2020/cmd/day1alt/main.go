package main

// sacrificing some memory for performance

import (
	"fmt"
	"log"
	"os"

	"github.com/gus/adventofcode/2020/internal/io"
)

const maxInput int16 = 2020

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

// Ex is an empty struct meant to be used as a key indicating existence
type Ex struct{}

// InputIndex is a fixed size list of possible inputs. If an index entry is not nil
// then the input exists.
type InputIndex [maxInput + 1]*Ex

type solverFunc func(min, max int16, idx *InputIndex) []int16

// FindTwoSolver finds a solution for two inputs.
func FindTwoSolver(min, max int16, idx *InputIndex) []int16 {
	h, t := min, max
	for h < t {
		if idx[h] != nil && idx[maxInput-h] != nil {
			return []int16{h, maxInput - h}
		} else if idx[t] != nil && idx[maxInput-t] != nil {
			return []int16{maxInput - t, t}
		}
		h++
		t--
	}
	return []int16{}
}

func findAddends(sum int16, from int16, idx *InputIndex) {
}

// FindThreeSolver finds a solution for three inputs.
func FindThreeSolver(min, max int16, idx *InputIndex) []int16 {
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
		// solve = FindThreeSolver
	}

	inputs := &InputIndex{}
	scanner := io.NewInt16Scanner(os.Stdin)
	var min, max int16 = maxInput, 0
	for scanner.Scan() {
		num := scanner.Int16()
		if num < 0 || num > maxInput {
			continue
		}
		if num < min {
			min = num
		}
		if num > max {
			max = num
		}
		inputs[num] = &Ex{}
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	sol := solve(min, max, inputs)
	if len(sol) == 0 {
		log.Println("no solution found")
		os.Exit(1)
	} else {
		log.Printf("solution: %v = %d\n", sol, product(sol))
	}
}
