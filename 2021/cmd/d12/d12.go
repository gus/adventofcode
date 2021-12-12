package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections/maps"
)

type Graph[K comparable] map[K][]K

type VisitFn[T any] func(T)

type Visitor[T any] interface {
	Visit(T, VisitFn[T])
}

type cave string

func (c cave) Big() bool {
	return c[0] >= 'A' && c[0] <= 'Z'
}

type path []cave

const (
	start = cave("start")
	end   = cave("end")
)

type p1visitor map[cave]int8

func (v p1visitor) Visit(c cave, fn VisitFn[cave]) {
	if c.Big() || v[c] < 1 {
		v[c]++
		fn(c)
		v[c]--
	}
}

func allow2visits(c cave, visits int8) bool { return visits > 1 && !c.Big() }

type p2visitor map[cave]int8

func (v p2visitor) Visit(c cave, fn VisitFn[cave]) {
	if c.Big() || v[c] < 1 || (c != start && !maps.Any(v, allow2visits)) {
		v[c]++
		fn(c)
		v[c]--
	}
}

func walkPaths(g Graph[cave], v Visitor[cave], p path, to cave, fn func(path)) {
	if p[len(p)-1] == to {
		fn(p)
		return
	}
	for _, edge := range g[p[len(p)-1]] {
		v.Visit(edge, func(c cave) { walkPaths(g, v, append(p, c), to, fn) })
	}
}

func main() {
	graph := Graph[cave]{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		names := strings.Split(scanner.Text(), "-")
		c0, c1 := cave(names[0]), cave(names[1])
		graph[c0] = append(graph[c0], c1)
		graph[c1] = append(graph[c1], c0)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}
	p1, p2 := 0, 0
	walkPaths(graph, p1visitor{start: 1}, path{start}, end, func(p path) { p1++ })
	walkPaths(graph, p2visitor{start: 1}, path{start}, end, func(p path) { p2++ })
	fmt.Printf("Part 1: %d\nPart 2: %d\n", p1, p2)
}

// Examples
// - Input 0 | Part {1 2}: {10 36}
// - Input 1 | Part {1 2}: {19 103}
// - Input 2 | Part {1 2}: {226 2509}
// Problem
// - Part 1: 3485
// - Part 2: 85062
