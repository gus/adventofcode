package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
)

// partitionable pair

type ppair struct{ a, b int }

func (p ppair) partition(lower bool) ppair {
	adjust := (p.b-p.a)/2 + p.a
	if lower {
		return ppair{p.a, adjust}
	}
	return ppair{adjust + 1, p.b}
}

// triangle number seat aggregator

type triseat struct {
	min int
	max int
	sum int
}

func newTriseat() *triseat {
	return &triseat{math.MaxInt32, math.MinInt32, 0}
}

func (t *triseat) add(v int) {
	if v < t.min {
		t.min = v
	} else if v > t.max {
		t.max = v
	}
	t.sum += v
}

func (t *triseat) findFirstMissing() int {
	// seats follow a triangular number pattern, so find the triangular number
	// for the max seat, subtract the min seat from that, then the diff of the
	// triangular number and the sum is the missing seat. assumes only missing
	// seat.
	tnumMax := (t.max * (t.max + 1)) / 2
	tnumMin := ((t.min - 1) * t.min) / 2
	return (tnumMax - tnumMin) - t.sum
}

// main

func main() {
	seats := newTriseat()

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		row := ppair{0, 127}
		col := ppair{0, 7}
		for _, c := range []byte(scanner.Text()) {
			switch c {
			case 'F', 'B':
				row = row.partition(c == 'F')
			case 'L', 'R':
				col = col.partition(c == 'L')
			}
		}
		seatID := row.a*8 + col.a
		seats.add(seatID)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	fmt.Printf("part 1: %d\n", seats.max)
	fmt.Printf("part 2: %d\n", seats.findFirstMissing())
}
