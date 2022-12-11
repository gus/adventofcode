package main

import (
	"fmt"
	"io"
	"os"
	"sort"
	"strings"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/utils"
)

type calcfn func(old int) int

type monkey struct {
	items       []int
	op          calcfn
	mod         int
	inspections int

	tMonkeyId int
	fMonkeyId int
}

var ops = map[string]func(int, int) int{
	"+": func(a, b int) int { return a + b },
	"*": func(a, b int) int { return a * b },
}

func parseOp(tokens []string) calcfn {
	op := ops[tokens[1]]
	if tokens[2] == "old" {
		return func(n int) int { return op(n, n) }
	}
	b := utils.Atoi(tokens[2])
	return func(n int) int { return op(n, b) }
}

func parseMonkey(lines []string) *monkey {
	itemsLine := strings.TrimSpace(lines[1])
	return &monkey{
		items: slices.Map(strings.Split(strings.Split(itemsLine, ": ")[1], ", "), func(n string) int {
			return int(utils.Atoi(n))
		}),
		op:        parseOp(strings.Split(strings.Split(strings.TrimSpace(lines[2]), "= ")[1], " ")),
		mod:       int(utils.Atoi(strings.Split(strings.TrimSpace(lines[3]), " ")[3])),
		tMonkeyId: utils.Atoi(strings.Split(strings.TrimSpace(lines[4]), " ")[5]),
		fMonkeyId: utils.Atoi(strings.Split(strings.TrimSpace(lines[5]), " ")[5]),
	}
}

func parseMonkeys(in string) []*monkey {
	monkeys := []*monkey{}
	lines := strings.Split(in, "\n")
	for i := 0; i < len(lines); i += 7 {
		monkeys = append(monkeys, parseMonkey(lines[i:i+6]))
	}
	return monkeys
}

func playGames(monkeys []*monkey, rounds int, relax calcfn) int {
	for rnd := 0; rnd < rounds; rnd++ {
		for _, m := range monkeys {
			items := m.items // pop all items from monkey first, then inspect them
			m.items = []int{}
			for _, item := range items {
				new := relax(m.op(item))
				if new%m.mod == 0 {
					monkeys[m.tMonkeyId].items = append(monkeys[m.tMonkeyId].items, new)
				} else {
					monkeys[m.fMonkeyId].items = append(monkeys[m.fMonkeyId].items, new)
				}
			}
			m.inspections += len(items)
		}
	}
	inspections := slices.Map(monkeys, func(m *monkey) int { return m.inspections })
	sort.Ints(inspections)
	return inspections[len(inspections)-2] * inspections[len(inspections)-1]
}

func main() {
	bs, _ := io.ReadAll(os.Stdin)
	monkeys1 := parseMonkeys(string(bs))
	monkeys2 := parseMonkeys(string(bs))

	p1relax := func(n int) int { return n / 3 }
	// mod by product of all divisors
	p2mod := slices.Reduce(monkeys2, 1, func(acc int, m *monkey) int { return acc * m.mod })
	p2relax := func(n int) int { return n % p2mod }

	// TEST: part 1.a (10605)	part 2.a (2713310158)
	// REAL: part 1 (54752)	part 2 (13606755504)
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", playGames(monkeys1, 20, p1relax), playGames(monkeys2, 10000, p2relax))
}
