package main

import (
	"bufio"
	"log"
	"math"
	"os"
	"strconv"
)

type diag []int

func (d diag) bits() int {
	return len(d)
}

func (d diag) base10() int {
	s := ""
	for _, i := range d {
		s += strconv.Itoa(i)
	}
	i, _ := strconv.ParseInt(s, 2, 64)
	return int(i)
}

type diags []diag

func (d diags) bits() int {
	if len(d) == 0 {
		return 0
	}
	return d[0].bits()
}

func (d diags) mask() int {
	if d.bits() == 0 {
		return 0
	}
	return math.MaxInt64 >> (64 - 1 - d.bits())
}

type commonBitFn func(d diags, bpos int) int

func mcb(d diags, bpos int) int {
	// feels so dumb and brute force
	if len(d) == 0 {
		panic("no diagnostics to scan")
	}

	ctr := 0
	for _, s := range d {
		if s[bpos] == 1 {
			ctr++
		}
	}
	if ctr >= len(d)-ctr {
		return 1
	}
	return 0
}

func lcb(d diags, bpos int) int {
	return mcb(d, bpos) ^ 1
}

func solvep1(d diags) int {
	s := 0
	for i := 0; i < d.bits(); i++ {
		s = s<<1 | mcb(d, i)
	}
	return s * (s ^ d.mask())
}

func p2filter(d diags, bpos int, fn commonBitFn) int {
	if len(d) == 1 {
		return d[0].base10()
	} else if bpos >= d.bits() {
		panic("no more bits to check")
	}
	b := fn(d, bpos)
	newd := diags{}
	for _, diag := range d {
		if diag[bpos] == b {
			newd = append(newd, diag)
		}
	}
	return p2filter(newd, bpos+1, fn)
}

func solvep2(d diags) int {
	return p2filter(d, 0, mcb) * p2filter(d, 0, lcb)
}

func toInts(s string) []int {
	ints := make([]int, len(s))
	for i := 0; i < len(s); i++ {
		if s[i] == '1' {
			ints[i] = 1
		}
	}
	return ints
}

func main() {
	d := diags{}

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		d = append(d, toInts(scanner.Text()))
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	log.Printf("part 1: %d\n", solvep1(d))
	log.Printf("part 2: %d\n", solvep2(d))
}

// Example results: 198, 130
// Problem results: 2035764, 2817661
