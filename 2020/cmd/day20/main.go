package main

import (
	"bufio"
	"fmt"
	"log"
	"math/bits"
	"os"
	"strconv"
)

func toBits(data []byte) []int8 {
	bits := make([]int8, len(data), len(data))
	for i, b := range data {
		if b == '#' {
			bits[i] = 1
		}
	}
	return bits
}

func toInt16(data []int8) int16 {
	i16 := int16(0)
	for i, b := range data {
		if b == 1 {
			i16 = i16 | 1<<(len(data)-i-1)
		}
	}
	return i16
}

func colBytes(col int, data [][]byte) []byte {
	e := make([]byte, len(data), len(data))
	for i, r := range data {
		e[i] = r[col]
	}
	return e
}

// Tile ...
type Tile struct {
	id   int64
	rows []int16
	cols []int16
	w    int
}

func newTile(id int64, data [][]int8) *Tile {
	t := &Tile{id, []int16{}, []int16{}, len(data[0])}
	colData := make([][]int8, len(data), len(data))
	for _, bs := range data {
		t.rows = append(t.rows, toInt16(bs))
		for col, b := range bs {
			colData[col] = append(colData[col], b)
		}
	}
	for i := len(colData) - 1; i > -1; i-- {
		t.cols = append(t.cols, toInt16(colData[i]))
	}
	return t
}

func (t *Tile) rotate() *Tile {
	// convert rows to cols
	newCols := make([]int16, len(t.rows), len(t.rows))
	copy(newCols, t.rows)

	// convert cols to rows and reverse the bits
	// left col (bottom or cols) becomes top row
	newRows := make([]int16, 0)
	for i := len(t.cols) - 1; i > -1; i-- {
		bs := int16(bits.Reverse16(uint16(t.cols[i])) >> (16 - t.w))
		newRows = append(newRows, bs)
	}
	return &Tile{t.id, newRows, newCols, t.w}
}

func (t *Tile) flip() *Tile {
	// put top on bottom
	newRows := make([]int16, len(t.rows), len(t.rows))
	for i := 0; i < len(t.rows); i++ {
		newRows = append(newRows, t.rows[len(t.rows)-i-1])
	}

	// reverse the bits in each column
	newCols := make([]int16, 0)
	for i := 0; i < len(t.cols); i++ {
		bs := int16(bits.Reverse16(uint16(t.cols[i])) >> (16 - t.w))
		newCols = append(newCols, bs)
	}
	return &Tile{t.id, newCols, newRows, t.w}
}

func (t Tile) String() string {
	s := fmt.Sprintf("\nTILE=%d ROWS\n", t.id)
	for _, r := range t.rows {
		s += fmt.Sprintf("%0.10b\t%d\n", r, r)
	}
	s += fmt.Sprintf("\nTILE=%d COLS\n", t.id)
	for _, r := range t.cols {
		s += fmt.Sprintf("%0.10b\t%d\n", r, r)
	}
	return s
}

func (t Tile) topID() int16 {
	return t.rows[0]
}

func (t Tile) rightID() int16 {
	return t.cols[0]
}

func (t Tile) bottomID() int16 {
	return t.rows[len(t.rows)-1]
}

func (t Tile) leftID() int16 {
	return t.cols[len(t.cols)-1]
}

func hasEdge(t, t2 *Tile) bool {
	for i := 0; i < 4; i++ {
		x := t2.topID()
		if t.topID() == x || t.rightID() == x || t.bottomID() == x || t.leftID() == x {
			return true
		}

		tf := t2.flip()
		x = tf.topID()
		if t.topID() == x || t.rightID() == x || t.bottomID() == x || t.leftID() == x {
			return true
		}

		t2 = t2.rotate()
	}
	return false
}

func main() {
	tiles := []*Tile{}
	scanner := bufio.NewScanner(os.Stdin)
	curID := int64(0)
	curTileData := [][]int8{}
	for scanner.Scan() {
		if scanner.Text() == "" {
			tiles = append(tiles, newTile(curID, curTileData))
		} else if scanner.Text()[:4] == "Tile" {
			id, _ := strconv.ParseInt(scanner.Text()[5:9], 10, 64)
			curID = id
			curTileData = [][]int8{}
		} else {
			curTileData = append(curTileData, toBits(scanner.Bytes()))
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}
	tiles = append(tiles, newTile(curID, curTileData))

	// base orientation based on first tile
	edgeCtr := map[int64]int{}
	for _, t := range tiles {
		edgeCtr[t.id] = 0
		for _, t2 := range tiles {
			if t == t2 {
				continue
			}

			if hasEdge(t, t2) {
				edgeCtr[t.id]++
			}
		}
	}

	part1Acc := int64(1)
	for tID, cnt := range edgeCtr {
		if cnt == 2 {
			part1Acc *= tID
		}
	}
	fmt.Printf("part1: %d\n", part1Acc)
}
