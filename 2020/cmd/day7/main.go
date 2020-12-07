package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

// BagID ...
type BagID string

// WeightedBag ...
type WeightedBag struct {
	w   int
	bag *Bag
}

// Bag ...
type Bag struct {
	id       BagID
	parents  []*Bag
	children []*WeightedBag
}

func newBag(id BagID) *Bag {
	return &Bag{id, make([]*Bag, 0), make([]*WeightedBag, 0)}
}

func (b *Bag) addWeight(wbag *WeightedBag) {
	b.children = append(b.children, wbag)
	wbag.bag.parents = append(wbag.bag.parents, b)
}

func (b *Bag) countParents() int {
	return len(b.findParents(bagNodeIndex{}))
}

func (b *Bag) findParents(idx bagNodeIndex) bagNodeIndex {
	for _, p := range b.parents {
		idx[p.id] = p
		p.findParents(idx)
	}
	return idx
}

func (b *Bag) weight() int {
	w := 0
	for _, c := range b.children {
		w += c.w + c.w*c.bag.weight()
	}
	return w
}

// example: light red bags contain 1 bright white bag, 2 muted yellow bags.
var ruleParser = regexp.MustCompile("([0-9]+)? ?([a-z]+ [a-z]+) bag")

type bagNodeIndex map[BagID]*Bag

func (bi bagNodeIndex) get(id BagID) *Bag {
	bag, ok := bi[id]
	if !ok {
		bag = newBag(id)
		bi[id] = bag
	}
	return bag
}

func (bi bagNodeIndex) parseRule(rule string) {
	ruleParts := ruleParser.FindAllStringSubmatch(rule, -1)
	if len(ruleParts) == 0 {
		log.Fatalf("rule is invalid format: %s", rule)
	}
	bag := bi.get(BagID(ruleParts[0][2]))
	for _, p := range ruleParts[1:] {
		id := BagID(p[2])
		if weight, err := strconv.Atoi(p[1]); err == nil && id != "no other" {
			bag.addWeight(&WeightedBag{weight, bi.get(id)})
		}
	}
}

// main

func main() {
	idx := bagNodeIndex{}

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		idx.parseRule(scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	fmt.Printf("part 1: %d\n", idx.get("shiny gold").countParents())
	fmt.Printf("part 2: %d\n", idx.get("shiny gold").weight())
}
