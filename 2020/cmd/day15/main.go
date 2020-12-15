package main

import (
	"fmt"
	"log"
	"os"
	"strconv"

	"github.com/gus/adventofcode/2020/internal/io"
	"github.com/gus/adventofcode/2020/internal/slices"
)

func usage(errMsg string) {
	fmt.Printf("! %s\n\nusage: %s [N]", errMsg, os.Args[0])
	os.Exit(1)
}

func part1(nums slices.Int64, toNum int64) int64 {
	last := int64(0)
	// 					[2]int64{t-1,t}
	spoken := map[int64][2]int64{}
	for i, n := range nums {
		spoken[n] = [2]int64{0, int64(i) + 1}
		last = n
	}

	for i := int64(len(nums) + 1); i <= toNum; i++ {
		cur := spoken[last]
		if cur[0] == 0 {
			last = 0
		} else {
			last = cur[1] - cur[0]
		}
		spoken[last] = [2]int64{spoken[last][1], i}
	}
	return last
}

func main() {
	var toNum int64 = 0
	args := os.Args[1:]
	if len(args) > 1 {
		usage("too many arguments")
	} else if len(args) == 1 {
		if n, err := strconv.ParseInt(args[0], 0, 64); err != nil {
			usage(fmt.Sprintf("unexpected argument: %s", args[0]))
		} else {
			toNum = n
		}
	}

	nums := slices.Int64{}
	scanner := io.NewIntScanner(os.Stdin)
	for scanner.Scan() {
		nums = nums.Append(scanner.Int64())
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	fmt.Printf("part 1,2: %d @ %d\n", part1(nums, toNum), toNum)
}
