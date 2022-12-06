package main

import (
	"fmt"
	"io"
	"os"
)

func isUnique(w []byte, winsz int) bool {
	m := map[byte]int{}
	for i := 0; i < len(w); i++ {
		if m[w[i]]++; m[w[i]] == 2 {
			return false
		}
	}
	return len(m) == winsz
}

func uniqSeqAt(buf []byte, winsz int) int {
	pos := winsz
	for !isUnique(buf[pos-winsz:pos], winsz) && pos < len(buf) {
		pos++
	}
	return pos
}

func main() {
	buf, _ := io.ReadAll(os.Stdin)
	// REAL: part 1 (1578)	part 2 (2178)
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n", uniqSeqAt(buf, 4), uniqSeqAt(buf, 14))
}
