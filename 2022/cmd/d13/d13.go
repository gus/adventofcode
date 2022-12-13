package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"sort"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/maths"
	"github.com/gus/adventofcode/2022/internal/types"
	"github.com/gus/adventofcode/2022/internal/utils"
)

type N struct {
	slice    bool
	v        int
	vs       []N
	injected bool
}

func parseInput(rdr []byte) (N, []byte) {
	n := N{slice: true}
	for len(rdr) > 0 {
		var v N
		switch {
		case rdr[0] == ']': // end of list
			return n, rdr[1:]
		case rdr[0] == '[': // new list
			v, rdr = parseInput(rdr[1:])
		case rdr[0] >= '0' && rdr[0] <= '9': // found int
			var ba []byte
			for ; len(rdr) > 0 && rdr[0] >= '0' && rdr[0] <= '9'; rdr = rdr[1:] {
				ba = append(ba, rdr[0])
			}
			v = N{v: utils.Atoi(string(ba))}
		default: // skip
			rdr = rdr[1:]
			continue
		}
		n.vs = append(n.vs, v)
	}
	return n, []byte{} // if we get here, probably something wrong with the input
}

func cmpN(nl, nr []N) int {
	pos, cmp := 0, 0
	for cmp == 0 && (pos < len(nl) || pos < len(nr)) {
		switch {
		case len(nl) > pos && len(nr) == pos: // left longer than right :(
			cmp = -1
		case len(nl) == pos && len(nr) > pos: // right longer than left :)
			cmp = 1
		case !nl[pos].slice && !nr[pos].slice: // both are ints, compare them
			cmp = maths.Cmp(nr[pos].v, nl[pos].v)
		case nl[pos].slice && nr[pos].slice: // both are slices, compare them
			cmp = cmpN(nl[pos].vs, nr[pos].vs)
		case nl[pos].slice: // left is a slice, right is int, make right a slice
			cmp = cmpN(nl[pos].vs, []N{nr[pos]})
		case nr[pos].slice: // right is a slice, left is int, make left a slice
			cmp = cmpN([]N{nl[pos]}, nr[pos].vs)
		default: // something impossible happened
			log.Fatalf("WTF? @%d %v <> %v", pos, nl[pos], nr[pos])
		}
		pos++
	}
	return cmp
}

func part1(ns []N) int {
	sum := 0 // find number of valid packet pairs
	for i := 0; i < len(ns); i += 2 {
		if cmpN(ns[i].vs, ns[i+1].vs) != -1 { // assume both roots are slices, because ... they will be
			sum += i/2 + 1 // add pair index
		}
	}
	return sum
}

func part2(ns []N) int {
	// inject two additional packets
	ns = append(ns, N{injected: true, slice: true, vs: []N{{slice: true, vs: []N{{v: 2}}}}})
	ns = append(ns, N{injected: true, slice: true, vs: []N{{slice: true, vs: []N{{v: 6}}}}})
	sort.Slice(ns, func(i, j int) bool { return cmpN(ns[i].vs, ns[j].vs) == 1 })

	// find multiply indices of injected packets together
	return slices.ReduceEnum(ns, 1, func(acc int, i int, n N) int {
		return acc * types.IfZero(n.injected, 1, i+1)
	})
}

func main() {
	bs, _ := io.ReadAll(os.Stdin)
	lines := slices.Filter(bytes.Split(bs, []byte{'\n'}), func(line []byte) bool { return len(line) > 0 })
	ns := slices.Map(lines, func(line []byte) N {
		in, _ := parseInput(line[1:])
		return in
	})

	// TEST: part 1.a (13)	part 2.a (140)
	// REAL: part 1 (5806)	part 2 (23600)
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", part1(ns), part2(ns))
}
