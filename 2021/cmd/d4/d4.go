package main

import (
	"bufio"
	"log"
	"os"
	"strconv"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections"
)

type win struct {
	board       *board
	num         int
	unmarkedSum int
}

type cell struct {
	num int
	row int
	col int
}

type board struct {
	cells     map[int]*cell
	rowMarks  []int
	colMarks  []int
	sum       int
	markedSum int
}

func newBoard() *board {
	return &board{map[int]*cell{}, make([]int, 5, 5), make([]int, 5, 5), 0, 0}
}

func (g *board) Add(num int, row, col int) {
	g.sum += num
	g.cells[num] = &cell{num, row, col}
}

func (g *board) Mark(draw int) *win {
	if cell, ok := g.cells[draw]; ok {
		g.markedSum += cell.num
		g.rowMarks[cell.row]++
		g.colMarks[cell.col]++
		if g.rowMarks[cell.row] == 5 || g.colMarks[cell.col] == 5 {
			return &win{g, draw, g.sum - g.markedSum}
		}
	}
	return nil
}

func (g *board) Reset() {
	g.markedSum = 0
	g.rowMarks = make([]int, 5, 5)
	g.colMarks = make([]int, 5, 5)
}

func solvep1(draws []int, boards []*board) int {
	for _, draw := range draws {
		for _, board := range boards {
			if winner := board.Mark(draw); winner != nil {
				return winner.num * winner.unmarkedSum
			}
		}
	}
	return 0
}

func solvep2(draws []int, boards []*board) int {
	winningBoards := collections.NewSet[int]()
	var lastWin *win
	for _, draw := range draws {
		for bid, board := range boards {
			if winner := board.Mark(draw); winner != nil {
				winningBoards.Add(bid)
				lastWin = winner
				if len(winningBoards) == len(boards) {
					// this was the last board to win
					return winner.num * winner.unmarkedSum
				}
			}
		}
	}
	if lastWin != nil {
		// got through all of the draws without all boards winning,
		// so use the last win?
		return lastWin.num * lastWin.unmarkedSum
	}
	return 0
}

func toInts(elems []string) []int {
	ints := []int{}
	for _, elem := range elems {
		num, _ := strconv.Atoi(elem)
		ints = append(ints, num)
	}
	return ints
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	if !scanner.Scan() {
		panic("nothing to scan")
	}

	draws := toInts(strings.Split(scanner.Text(), ","))

	bctr := -1
	boards := []*board{}

	row := 0
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			bctr++
			boards = append(boards, newBoard())
			row = 0
			continue
		}

		col := 0
		for _, tok := range strings.Split(line, " ") {
			if tok == "" {
				continue
			}

			num, _ := strconv.Atoi(tok)
			boards[bctr].Add(num, row, col)

			col = (col + 1) % 5
		}
		row++
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	log.Printf("part 1: %v\n", solvep1(draws, boards))

	for _, board := range boards {
		board.Reset()
	}
	log.Printf("part 1: %v\n", solvep2(draws, boards))
}
