package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

type bus struct {
	id     int64
	offset int64
}

func waitForBus(departAt int64, buses []bus) int64 {
	bestBus := bus{}
	minWait := int64(math.MaxInt64)
	for _, bus := range buses {
		r := departAt / bus.id
		w := (r+1)*bus.id - departAt
		if w < minWait {
			bestBus = bus
			minWait = w
		}
	}
	return bestBus.id * minWait
}

func findOffsetTimestamp(f []bus) int64 {
	incr, base := f[0].id, f[0].id
	for _, bus := range f[1:] {
		for ts := base; ; ts += incr {
			if (ts+bus.offset)%bus.id == 0 {
				base, incr = ts, incr*bus.id
				break
			}
		}
	}
	return base
}

func main() {
	departAt := int64(-1)
	fleet := []bus{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		if departAt < 0 {
			departAt, _ = strconv.ParseInt(scanner.Text(), 10, 64)
			continue
		}
		txt := scanner.Text()
		for i, b := range strings.Split(txt, ",") {
			if b != "x" {
				busID, _ := strconv.ParseInt(b, 10, 64)
				fleet = append(fleet, bus{busID, int64(i)})
			}
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	fmt.Printf("part 1: %d\n", waitForBus(departAt, fleet))
	fmt.Printf("part 2: %d\n", findOffsetTimestamp(fleet))
}
