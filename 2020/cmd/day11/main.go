package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"reflect"
)

type occupiedSeatFunc func(cols, seat int, seating []byte) int

func neighborSeats(cols, seat int, seating []byte) int {
	row, rows, col := int(seat/cols), int(len(seating)/cols)-1, seat%cols
	acc := 0
	for i := row - 1; i <= row+1; i++ {
		if i < 0 || i > rows {
			continue
		}
		for j := col - 1; j <= col+1; j++ {
			if (i == row && j == col) || j < 0 || j >= cols {
				continue
			}
			if seating[i*cols+j] == '#' {
				acc++
			}
		}
	}
	return acc
}

func findSeat(cols, seat int, rowIncr, colIncr int, seating []byte) int {
	row, rows, col := int(seat/cols), int(len(seating)/cols), seat%cols
	row += rowIncr
	col += colIncr
	for row > -1 && col > -1 && row < rows && col < cols {
		ns := row*cols + col
		switch seating[ns] {
		case '#':
			return 1
		case 'L':
			return 0
		}
		row += rowIncr
		col += colIncr
	}
	return 0
}

func nearestNeighborSeats(cols, seat int, seating []byte) int {
	acc := findSeat(cols, seat, -1, -1, seating) // NW
	acc += findSeat(cols, seat, -1, 0, seating)  // N
	acc += findSeat(cols, seat, -1, 1, seating)  // NE
	acc += findSeat(cols, seat, 0, -1, seating)  // W
	acc += findSeat(cols, seat, 0, 1, seating)   // E
	acc += findSeat(cols, seat, 1, -1, seating)  // SW
	acc += findSeat(cols, seat, 1, 0, seating)   // S
	acc += findSeat(cols, seat, 1, 1, seating)   // SE
	return acc
}

func printSeats(cols int, seating []byte) {
	rows := len(seating) / cols
	for i := 0; i < rows; i++ {
		fmt.Printf("%q\n", seating[i*cols:i*cols+cols-1])
	}
}

func applyRound(cols, maxNeighbors int, seating []byte, seatfn occupiedSeatFunc) []byte {
	newSeating := make([]byte, len(seating))
	for i, c := range seating {
		if c == '.' {
			newSeating[i] = c
			continue
		}
		newSeating[i] = c
		adjacentSeats := seatfn(cols, i, seating)
		if c == 'L' && adjacentSeats == 0 {
			newSeating[i] = '#'
		} else if c == '#' && adjacentSeats >= maxNeighbors {
			newSeating[i] = 'L'
		}
	}
	return newSeating
}

func stabilize(cols, maxNeighbors int, seating []byte, seatfn occupiedSeatFunc) []byte {
	last := seating
	cur := applyRound(cols, maxNeighbors, last, seatfn)
	for ; !reflect.DeepEqual(last, cur); cur = applyRound(cols, maxNeighbors, last, seatfn) {
		last = cur
	}
	return last
}

func countOccupied(seating []byte) int {
	occCtr := 0
	for _, c := range seating {
		if c == '#' {
			occCtr++
		}
	}
	return occCtr
}

func main() {
	seating := []byte{}
	scanner := bufio.NewScanner(os.Stdin)
	rows := 0
	for scanner.Scan() {
		rows++
		seating = append(seating, scanner.Bytes()...)
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	cols := len(seating) / rows
	fmt.Printf("part 1: %d\n", countOccupied(stabilize(cols, 4, seating, neighborSeats)))
	fmt.Printf("part 2: %d\n", countOccupied(stabilize(cols, 5, seating, nearestNeighborSeats)))
}
