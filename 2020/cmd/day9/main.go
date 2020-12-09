package main

import (
	"fmt"
	"log"
	"os"
	"strconv"

	"github.com/gus/adventofcode/2020/internal/io"
)

func usage(errMsg string) {
	fmt.Printf("! %s\n\nusage: %s --window-size N", errMsg, os.Args[0])
	os.Exit(1)
}

type xmasdata []int64

func sumAnyTwo(x xmasdata, sum int64) bool {
	// w1 is exclusive
	for i := 0; i < len(x)-1; i++ {
		for j := i + 1; j < len(x); j++ {
			if x[i]+x[j] == sum {
				return true
			}
		}
	}
	return false
}

func findOutlierIndex(x xmasdata, win int) int {
	for i := win; i < len(x); i++ {
		if !sumAnyTwo(x[i-win:i], x[i]) {
			return i
		}
	}
	panic("no outlier found")
}

type winsum struct {
	acc int64
	min int64
	max int64
}

func newWinsum(acc int64) *winsum {
	return &winsum{acc, acc, acc}
}

func (w *winsum) weakness() int64 {
	return w.min + w.max
}

func (w *winsum) append(n int64) {
	w.acc += n
	if n < w.min {
		w.min = n
	}
	if n > w.max {
		w.max = n
	}
}

func calcWindowSum(x xmasdata, sum int64) *winsum {
	for i := 0; i < len(x)-1; i++ {
		ws := newWinsum(x[i])
		for j := i + 1; j < len(x); j++ {
			ws.append(x[j])
			if ws.acc == sum {
				return ws
			}
		}
	}
	return nil
}

func main() {
	var windowSize int = 0

	args := os.Args[1:]
	if len(args) != 2 {
		usage("invalid arguments")
	} else {
		if args[0] != "--window-size" {
			usage(fmt.Sprintf("unexpected argument: %s", args[0]))
		}
		windowSize, _ = strconv.Atoi(args[1])
	}

	data := make(xmasdata, 0)
	scanner := io.NewIntScanner(os.Stdin)
	for scanner.Scan() {
		data = append(data, scanner.Int64())
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	idx := findOutlierIndex(data, windowSize)
	fmt.Printf("part 1: %d\n", data[idx])

	if ws := calcWindowSum(data[:idx], data[idx]); ws != nil {
		fmt.Printf("part 2: %d\n", ws.weakness())
	} else if ws := calcWindowSum(data[idx+1:], data[idx]); ws != nil {
		fmt.Printf("part 2: %d\n", ws.weakness())
	} else {
		fmt.Printf("part 2: no weakness found :(\n")
	}
}
