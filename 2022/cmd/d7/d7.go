package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/gus/adventofcode/2022/internal/collections/maps"
	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/utils"
)

const (
	MAX_SZ  = 70000000
	NEED_SZ = 30000000
)

type path []string

func (p path) change(d string) path {
	if d == "/" {
		p = []string{}
	} else if d == ".." {
		if len(p) > 0 {
			p = p[:len(p)-1]
		}
	} else {
		p = append(p, d)
	}
	return p
}

func (p path) String() string {
	return "/" + strings.Join(p, "/")
}

type inodesz map[string]int

func (i inodesz) add(p path, f string, sz int) {
	for len(p) > 0 {
		i[p.String()] += sz
		p = p.change("..")
	}
	i[p.String()] += sz
}

func main() {
	dirsz := inodesz{}

	buf := bufio.NewScanner(os.Stdin)
	var curPath path
	for buf.Scan() {
		line := buf.Text()
		if line[0] >= '0' && line[0] <= '9' {
			inode := strings.Split(line, " ")
			dirsz.add(curPath, inode[1], utils.Atoi(inode[0]))
		} else if line[:4] == "$ cd" {
			curPath = curPath.change(line[5:])
		}
	}

	part1 := maps.Reduce(dirsz, 0, func(acc int, key string, val int) int {
		if val <= 100000 {
			acc += val
		}
		return acc
	})

	minFree := NEED_SZ - (MAX_SZ - dirsz["/"])
	bigDirs := slices.Filter(maps.Values(dirsz), func(sz int) bool {
		return sz >= minFree
	})
	sort.Ints(bigDirs)
	part2 := bigDirs[0]

	// TEST: part 1 (95437)	part 2 (1989474)
	// REAL: part 1 (24933642)	part 2 (1111607)
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", part1, part2)
}
