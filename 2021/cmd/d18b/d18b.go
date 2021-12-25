package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"

	"github.com/gus/adventofcode/2021/internal/types"
)

type LR interface {
	End() bool
	Left() LR
	Right() LR
}

type V int

func (v V) End() bool { return true }
func (v V) Left() LR  { return nil }
func (v V) Right() LR { return nil }

type P [2]LR

func (p P) End() bool { return false } //p[0].End() && p[1].End() }
func (p P) Left() LR  { return p[0] }
func (p P) Right() LR { return p[1] }

func atoi(a string) int {
	i, _ := strconv.Atoi(a)
	return i
}

// parseP makes an assumption that we are starting from something nested
func parseP(str string) (P, string) {
	var p = P{}
	pidx := 0
	// fmt.Printf("\n# parsing %s\n", str)
	for len(str) > 0 {
		c := str[0]
		// fmt.Printf("\n=> %v '%s'\n", p, str)
		switch c {
		case '[':
			p[pidx], str = parseP(str[1:])
		case ',':
			str = str[1:]
			pidx++
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			p[pidx] = V(atoi(string(str[0])))
			str = str[1:]
		case ']':
			return p, str[1:]
		default:
			panic(fmt.Sprintf("unknown char '%c' in \"%s\"", c, str))
		}
	}
	return p, str
}

func ParseP(str string) P {
	p, _ := parseP(str[1:])
	return p
}

func sumP(p LR, lvl int) (newp LR) {

	return newp
}

func SumP(p LR) LR {
	newp := sumP(p, 0)
	return newp
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	var lastp LR
	for scanner.Scan() {
		// pairs = append(pairs, ParseP(scanner.Text()))
		p := SumP(ParseP(scanner.Text()))
		if lastp == types.Zero[P]() {
			lastp = SumP(p)
			continue
		}
		lastp = SumP(P{lastp, p})
		fmt.Printf("\n%v\n", lastp)
	}
	fmt.Printf("\n# FINAL\n%v\n", lastp)
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	// fmt.Printf("%v\n", p0)
}
