package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections/slices"
)

var digmask = map[byte]int8{
	'a': 0b01000000,
	'b': 0b00100000,
	'c': 0b00010000,
	'd': 0b00001000,
	'e': 0b00000100,
	'f': 0b00000010,
	'g': 0b00000001,
}

type Digit struct {
	source string
	len    int
	code   int8
}

func NewDigit(source string) Digit {
	code := int8(0)
	for i := 0; i < len(source); i++ {
		code |= digmask[source[i]]
	}
	return Digit{source, len(source), code}
}

type Entry struct {
	signals []Digit
	outputs []Digit
}

func NewEntry(line string) Entry {
	sigsouts := strings.Split(line, " | ")
	return Entry{
		signals: slices.Map(strings.Split(sigsouts[0], " "), NewDigit),
		outputs: slices.Map(strings.Split(sigsouts[1], " "), NewDigit),
	}
}

func solvep1(entries []Entry) int {
	ctrs := map[int]int{}
	for _, e := range entries {
		for _, output := range e.outputs {
			ctrs[output.len]++
		}
	}
	return ctrs[2] + ctrs[4] + ctrs[3] + ctrs[7] // 1s, 4s, 7s, 8s
}

func decipherKey(entry Entry) map[int8]int {
	sigs := make([]Digit, len(entry.signals))
	copy(sigs, entry.signals)

	key := map[int8]int{}

	// 1, 4, 7, 8 are known
	one := slices.RemoveFirst(sigs, func(d Digit) bool { return d.len == 2 })
	key[one.code] = 1

	four := slices.RemoveFirst(sigs, func(d Digit) bool { return d.len == 4 })
	key[four.code] = 4

	seven := slices.RemoveFirst(sigs, func(d Digit) bool { return d.len == 3 })
	key[seven.code] = 7

	eight := slices.RemoveFirst(sigs, func(d Digit) bool { return d.len == 7 })
	key[eight.code] = 8

	// 3: 5 segments, contains a 1
	three := slices.RemoveFirst(sigs, func(d Digit) bool { return d.len == 5 && d.code&one.code == one.code })
	key[three.code] = 3

	// 9: 6 segments, contains a 4
	nine := slices.RemoveFirst(sigs, func(d Digit) bool { return d.len == 6 && d.code&four.code == four.code })
	key[nine.code] = 9

	// 0: 6 segments, contains a 1
	zero := slices.RemoveFirst(sigs, func(d Digit) bool { return d.len == 6 && d.code&one.code == one.code })
	key[zero.code] = 0

	// 6: 6 segments
	six := slices.RemoveFirst(sigs, func(d Digit) bool { return d.len == 6 })
	key[six.code] = 6

	// 5: 5 segments, can fit in 9
	five := slices.RemoveFirst(sigs, func(d Digit) bool { return d.len == 5 && d.code&nine.code == d.code })
	key[five.code] = 5

	// 2: last one remaining
	key[sigs[0].code] = 2

	return key
}

func solvep2(entries []Entry) int {
	sum := 0
	for _, entry := range entries {
		key := decipherKey(entry)
		outs := entry.outputs
		sum += key[outs[0].code]*1e3 + key[outs[1].code]*1e2 + key[outs[2].code]*1e1 + key[outs[3].code]
	}
	return sum
}

func main() {
	entries := []Entry{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		entries = append(entries, NewEntry(scanner.Text()))
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	fmt.Printf("part 1: %d\n", solvep1(entries))
	fmt.Printf("part 2: %d\n", solvep2(entries))
}

// part 1: 470
// part 2: 989396
