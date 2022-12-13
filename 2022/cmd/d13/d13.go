package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"sort"
	"strings"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/maths"
	"github.com/gus/adventofcode/2022/internal/utils"
)

type N struct {
	slice    bool
	v        int
	vs       []N
	injected bool
}

func (n N) String() string { // debugging
	if n.slice {
		return "[" + strings.Join(slices.Map(n.vs, func(v N) string { return v.String() }), ", ") + "]"
	}
	return fmt.Sprintf("%d", n.v)
}

func readInt(rdr []byte) (int, []byte) {
	var ba []byte
	for i := 0; i < len(rdr) && rdr[i] >= '0' && rdr[i] <= '9'; i++ {
		ba = append(ba, rdr[i])
	}
	return utils.Atoi(string(ba)), rdr[len(ba):]
}

func parseInput(rdr []byte) (N, []byte) {
	n := N{slice: true}
	for len(rdr) > 0 {
		b := rdr[0]
		switch {
		case b == ',': // skip
			rdr = rdr[1:]
		case b == ']': // end of list
			return n, rdr[1:]
		case b == '[': // new list
			v, r := parseInput(rdr[1:])
			rdr = r
			n.vs = append(n.vs, v)
		case b >= '0' && b <= '9': // found int
			v, r := readInt(rdr)
			rdr = r
			n.vs = append(n.vs, N{v: v})
		}
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
			log.Fatalf("WTF? @%d %s <> %s", pos, nl[pos], nr[pos])
		}
		pos++
	}
	return cmp
}

func part1(ns []N) int {
	valid := 0 // find number of valid packet pairs
	for i := 0; i < len(ns); i += 2 {
		if cmpN(ns[i].vs, ns[i+1].vs) != -1 { // assume both roots are slices, because ... they will be
			valid += i/2 + 1 // pair index
		}
	}
	return valid
}

func part2(ns []N) int {
	// inject two additional packets
	ns = append(ns, N{injected: true, slice: true, vs: []N{{slice: true, vs: []N{{v: 2}}}}})
	ns = append(ns, N{injected: true, slice: true, vs: []N{{slice: true, vs: []N{{v: 6}}}}})
	sort.Slice(ns, func(i, j int) bool { return cmpN(ns[i].vs, ns[j].vs) == 1 })

	// find indices of injected packets and multiply them together
	injProd := 1
	for i, n := range ns {
		if n.injected {
			injProd *= i + 1
		}
	}
	return injProd
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
