package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections/slices"
)

type N struct {
	v int
	p P
}

func (n N) Nested() bool {
	return len(n.p) > 0
}

func (n N) String() string {
	if len(n.p) > 0 {
		return fmt.Sprintf("%s", n.p)
	}
	return fmt.Sprintf("%d", n.v)
}

type P []*N

func (p P) Left() *N {
	if len(p) > 0 {
		return p[0]
	}
	return nil
}

func (p P) Right() *N {
	if len(p) > 1 {
		return p[1]
	}
	return nil
}

func (p P) String() string {
	s := "{"
	s += strings.Join(slices.Map(p, func(n *N) string { return n.String() }), ", ")
	return s + "}"
}

func parseP(str string) (*N, string) {
	var p = P{}
	for len(str) > 0 {
		c := str[0]
		var n *N
		switch c {
		case '[':
			n, str = parseP(str[1:])
			p = append(p, n)
		case ',':
			str = str[1:]
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			v, _ := strconv.Atoi(string(str[0]))
			p = append(p, &N{v: v})
			str = str[1:]
		case ']':
			return &N{p: p}, str[1:]
		default:
			panic(fmt.Sprintf("unknown char '%c' in \"%s\"", c, str))
		}
	}
	return &N{p: p}, str
}

func ParseP(str string) *N {
	n, _ := parseP(str[1:])
	return n
}

func resolveP(p P, lvl int) (int, int) {
	cl, cr := 0, 0

	ln := p.Left()
	if ln == nil {
		return cl, cr
	}

	if ln.Nested() {
		cl, cr = resolveP(ln.p, lvl+1)
	} else if lvl > 4 { // not nested
		cl = ln.v
	}

	rn := p.Right()
	if rn == nil {
		return cl, cr
	}

	if ln.Nested() {
		cl, cr = resolveP(rn.p, lvl+1)
	} else if lvl > 4 { // not nested
		cr = ln.v
	}

	return cl, cr
	// for _, n := range p {
	// 	if len(n.p) > 0 {
	// 		carryN = resolveP(n.p, lmN, lvl+1)
	// 	} else {
	// 		lmN = n
	// 	}
	// }
	// return &N{p: p}
}

func ResolveP(p P, lvl int) P {
	resolveP(p, nil, 0)
	return p
}

func main() {
	// pairs := []P{}
	var lastn *N
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		// pairs = append(pairs, ParseP(scanner.Text()))
		n := ParseP(scanner.Text())
		if lastn == nil {
			lastn = n
			continue
		}
		lastn = ResolveP(P{lastn, n})
		fmt.Printf("# NUM\n%v\n", lastn)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}
}
