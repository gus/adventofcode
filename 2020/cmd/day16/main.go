package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type rule struct {
	id  string
	min int
	max int
}

func (r rule) allows(n int) bool {
	return n >= r.min && n <= r.max
}

func (r rule) String() string {
	return fmt.Sprintf("%s (%d-%d)\n", r.id, r.min, r.max)
}

type rules []*rule

func (rs rules) allows(n int) bool {
	for _, r := range rs {
		if r.allows(n) {
			return true
		}
	}
	return false
}

func (rs rules) addRange(id string, min, max int) rules {
	return append(rs, &rule{id, min, max})
}

var ruleRE = regexp.MustCompile("[a-z ]+[:]|[0-9]+-[0-9]+")

func scanRules(scanner *bufio.Scanner) rules {
	rs := rules{}
	for scanner.Scan() {
		if scanner.Text() == "" {
			break
		}
		parts := ruleRE.FindAllStringSubmatch(scanner.Text(), -1)
		id := parts[0][0]
		for _, match := range parts[1:] {
			nums := strings.Split(match[0], "-")
			min, _ := strconv.Atoi(nums[0])
			max, _ := strconv.Atoi(nums[1])
			rs = rs.addRange(id, min, max)
		}
	}
	return rs
}

type ticket []int

func (t ticket) sumInvalid(rs rules) int {
	acc := 0
	for _, n := range t {
		if !rs.allows(n) {
			acc += n
		}
	}
	return acc
}

func scanTickets(scanner *bufio.Scanner) []ticket {
	ts := []ticket{}
	for scanner.Scan() {
		if scanner.Text() == "" {
			break
		}
		t := []int{}
		for _, snum := range strings.Split(scanner.Text(), ",") {
			num, _ := strconv.Atoi(snum)
			t = append(t, num)
		}
		ts = append(ts, t)
	}
	return ts
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	rules := scanRules(scanner)

	scanner.Scan() // skip prologue
	myTicket := scanTickets(scanner)[0]

	scanner.Scan() // skip prologue
	tickets := scanTickets(scanner)

	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	invalidAcc := 0
	validTickets := []ticket{}
	for _, t := range tickets {
		invalidCnt := t.sumInvalid(rules)
		if invalidCnt == 0 {
			validTickets = append(validTickets, t)
		}
		invalidAcc += invalidCnt
	}

	fmt.Printf("part 1: %d\n", invalidAcc)

	fields := len(validTickets[0])
	ruleFieldMatches := map[string][]int{}

	for _, t := range validTickets {
		for fieldPos, n := range t {
			for _, r := range rules {
				if r.allows(n) {
					fieldMatches, ok := ruleFieldMatches[r.id]
					if !ok {
						fieldMatches = make([]int, fields, fields)
					}
					fieldMatches[fieldPos]++
					ruleFieldMatches[r.id] = fieldMatches
				}
			}
		}
	}

	fieldMatch := map[int]string{}
	for len(fieldMatch) < fields {
		for _, rule := range rules {
			rfm := rule.id
			fieldMatches := ruleFieldMatches[rfm]
			maxMatchFields := []int{}
			misMatchFields := []int{}
			for fieldPos, matches := range fieldMatches {
				if _, ok := fieldMatch[fieldPos]; !ok && matches == len(validTickets) {
					maxMatchFields = append(maxMatchFields, fieldPos)
				} else if !ok {
					misMatchFields = append(misMatchFields, fieldPos)
				}
			}
			if len(maxMatchFields) == 1 {
				fieldMatch[maxMatchFields[0]] = rfm
				delete(ruleFieldMatches, rfm)
			} else if len(maxMatchFields) == 0 && len(misMatchFields) == 1 {
				fieldMatch[misMatchFields[0]] = rfm
				delete(ruleFieldMatches, rfm)
			}
		}
	}

	part2Acc := 1
	for fieldPos, ruleID := range fieldMatch {
		if len(ruleID) > len("departure") && ruleID[:9] == "departure" {
			fmt.Printf("%s\t%d\t%d *= %d\n", ruleID, fieldPos, part2Acc, myTicket[fieldPos])
			part2Acc *= myTicket[fieldPos]
		}
	}
	fmt.Printf("part 2: %d\n", part2Acc)
}
