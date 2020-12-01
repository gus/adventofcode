package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

type solution []int16

type solverFunc func([]int16) solution

func findTwoSolver(inputs []int16) solution {
	for _, i := range inputs {
		for _, j := range inputs[1:] {
			if i+j == 2020 {
				return solution{i, j}
			}
		}
	}
	return solution{}
}

func findThreeSolver(inputs []int16) solution {
	for _, i := range inputs {
		for _, j := range inputs[1:] {
			for _, k := range inputs[2:] {
				if i+j+k == 2020 {
					return solution{i, j, k}
				}
			}
		}
	}
	return solution{}
}

func product(inputs solution) int64 {
	var p int64 = 1
	for _, i := range inputs {
		p = p * int64(i)
	}
	return p
}

func usage(errMsg string) {
	fmt.Printf("! %s\n\nusage: %s [--three]", errMsg, os.Args[0])
	os.Exit(1)
}

func main() {
	solve := findTwoSolver

	args := os.Args[1:]
	if len(args) > 1 {
		usage("too many arguments")
	} else if len(args) == 1 {
		if args[0] != "--three" {
			usage(fmt.Sprintf("unexpected argument: %s", args[0]))
		}
		solve = findThreeSolver
	}

	inputs := make([]int16, 0)
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		num, err := strconv.ParseInt(scanner.Text(), 10, 16)
		if err != nil {
			log.Fatalf("expected int16: %v", err)
		}
		inputs = append(inputs, int16(num))
	}

	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading lines: %v", err)
	}

	sol := solve(inputs)
	if len(sol) == 0 {
		log.Println("no solution found")
		os.Exit(1)
	} else {
		log.Printf("solution: %v = %d\n", sol, product(sol))
	}
}
