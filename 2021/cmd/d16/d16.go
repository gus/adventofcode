package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strings"

	"github.com/gus/adventofcode/2021/internal/collections/slices"
	"github.com/gus/adventofcode/2021/internal/maths"
)

var table = map[byte]int{
	'0': 0, '1': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9,
	'A': 10, 'B': 11, 'C': 12, 'D': 13, 'E': 14, 'F': 15,
}

type Buf4 struct {
	v   int
	l   int
	src []int
}

func NewBuf4(nibbles []int) *Buf4 {
	return &Buf4{src: nibbles}
}

func (buf *Buf4) Size() int {
	return buf.l + len(buf.src)*4
}

func (buf *Buf4) pumpIfNeeded(n int) {
	for n > buf.l {
		if len(buf.src) == 0 {
			panic("source buffer exhausted")
		}
		buf.Push4(buf.src[0])
		buf.src = buf.src[1:]
	}
}

func (buf *Buf4) Pop(n int) int {
	buf.pumpIfNeeded(n)
	delta := buf.l - n
	pop := buf.v >> delta
	buf.v = buf.v ^ (pop << delta)
	buf.l = delta
	return pop
}

func (buf *Buf4) Push(v, n int) {
	buf.v = (buf.v << n) | v
	buf.l += 4
}

func (buf *Buf4) Push4(v int) {
	buf.Push(v, 4)
}

func (buf Buf4) String() string {
	return fmt.Sprintf("{%08b, %d}", buf.v, buf.l)
}

// Txm represents a transmission
type Txm struct {
	txms []Txm

	ver int
	typ int
	val int
}

func (txm Txm) SumVer() int {
	return slices.Reduce(txm.txms, txm.ver, func(acc int, sub Txm) int { return acc + sub.SumVer() })
}

func (txm Txm) Value() int {
	val := 0
	switch txm.typ {
	case 0: // sum
		val = slices.Reduce(txm.txms, 0, func(acc int, sub Txm) int { return acc + sub.Value() })
	case 1: // product
		val = slices.Reduce(txm.txms, 1, func(acc int, sub Txm) int { return acc * sub.Value() })
	case 2: // min
		val = slices.Reduce(txm.txms, math.MaxInt, func(acc int, sub Txm) int { return maths.Min(acc, sub.Value()) })
	case 3: // max
		val = slices.Reduce(txm.txms, math.MinInt, func(acc int, sub Txm) int { return maths.Max(acc, sub.Value()) })
	case 4: // literal
		val = txm.val // type-id 4
	case 5:
		if txm.txms[0].Value() > txm.txms[1].Value() {
			val = 1
		}
	case 6:
		if txm.txms[0].Value() < txm.txms[1].Value() {
			val = 1
		}
	case 7:
		if txm.txms[0].Value() == txm.txms[1].Value() {
			val = 1
		}
	}
	return val
}

func (txm Txm) String() string {
	return fmt.Sprintf("Txm{ver: %d, typ: %d, val: %d, subs: %v}", txm.ver, txm.typ, txm.val, txm.txms)
}

func readLiteral(buf *Buf4) int {
	lit := 0
	end := false
	for !end {
		end = buf.Pop(1) == 0
		lit = (lit << 4) | buf.Pop(4)
	}
	return lit
}

func ParseTxm(buf *Buf4) Txm {
	txm := Txm{ver: buf.Pop(3), typ: buf.Pop(3)}

	switch txm.typ {
	case 4: // literal
		txm.val = readLiteral(buf)
	default: // operator
		switch buf.Pop(1) { // length type id
		case 0: // bits to read
			tbits := buf.Pop(15)
			sz := buf.Size()
			for sz-buf.Size() < tbits {
				txm.txms = append(txm.txms, ParseTxm(buf))
			}
		case 1: // packets to read
			tpkts := buf.Pop(11)
			for len(txm.txms) < tpkts {
				txm.txms = append(txm.txms, ParseTxm(buf))
			}
		}
	}

	return txm
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		src := scanner.Text()
		fmt.Printf("\n# SRC=%s\n", src)
		nibbles := slices.Map(strings.Split(src, ""), func(b string) int { return table[b[0]] })
		txm := ParseTxm(NewBuf4(nibbles))
		fmt.Printf("-> SumVer=%d Value=%d\n", txm.SumVer(), txm.Value())
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}
}

// Examples
// Part 1: version sums
//   Version: 6            - D2FE28
//   Version: 9 (1+6+2)    - 38006F45291200
//   Version: 14 (7+2+4+1) - EE00D40C823060
//   Version: 16 (4+1+5+6) - 8A004A801A8002F478
//   Version: 12 (3+...)   - 620080001611562C8802118E34
//   Version: 23           - C0015000016115A2E0802F182340
//   Version: 31           - A0016C880162017C3686B18A3D4780
// Part 2
//   Value: 3  - C200B40A82
//   Value: 54 - 04005AC33890
//   Value: 7  - 880086C3E88112
//   Value: 9  - CE00C43D881120
//   Value: 1  - D8005AC2A8F0
//   Value: 0  - F600BC2D8F
//   Value: 0  - 9C005AC2F8F0
//   Value: 1  - 9C0141080250320F1802104A08

// Solution
// Part 1: 873          (version sum)
// Part 2: 402817863665 (value)
