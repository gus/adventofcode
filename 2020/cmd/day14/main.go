package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"strconv"
)

var ones uint64 = math.MaxUint64 >> (64 - 36)

type valueMask struct {
	set   uint64
	unset uint64
}

func newValueMask(mask string) valueMask {
	var set, unset uint64 = 0, 0
	n := len(mask) - 1
	// read with lsb on right
	for i := n; i >= 0; i-- {
		sm := uint64(1 << (n - i))
		if mask[i] == '0' {
			unset = unset | sm
		} else if mask[i] == '1' {
			set = set | sm
		}
	}
	return valueMask{set, unset ^ ones}
}

func (m valueMask) apply(v uint64) uint64 {
	return (v | m.set) & m.unset
}

type addrMask struct {
	bmask  uint64
	floats []int
}

func newAddrMask(mask string) addrMask {
	floats := []int{}
	bmask := uint64(0)
	// generate the base mask
	for i := 0; i < len(mask); i++ {
		switch mask[len(mask)-i-1] { // grab char from opposite end
		case '0':
			continue
		case 'X':
			floats = append(floats, i)
		}
		bmask = bmask | uint64(1<<i)
	}
	return addrMask{bmask, floats}
}

func (a addrMask) apply(addr uint64) []uint64 {
	newaddr := addr | a.bmask
	addrs := []uint64{newaddr}
	for _, fl := range a.floats {
		umask := 1<<fl ^ ones
		n := len(addrs)
		for i := 0; i < n; i++ {
			addrs = append(addrs, addrs[i]&umask)
		}
	}
	return addrs
}

var memre = regexp.MustCompile("[0-9]+")

func parseMem(str string) (uint64, uint64) {
	nums := memre.FindAllString(str, -1)
	addr, _ := strconv.ParseUint(nums[0], 10, 64)
	value, _ := strconv.ParseUint(nums[1], 10, 64)
	return addr, value
}

// main

func memsum(mem map[uint64]uint64) (sum uint64) {
	for _, v := range mem {
		sum += v
	}
	return sum
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	vmask, amask := newValueMask(""), newAddrMask("")
	mem1, mem2 := map[uint64]uint64{}, map[uint64]uint64{}
	for scanner.Scan() {
		txt := scanner.Text()
		if txt[0:4] == "mask" {
			vmask = newValueMask(txt[7:])
			amask = newAddrMask(txt[7:])
		} else if txt[0:3] == "mem" {
			addr, value := parseMem(txt[4:])
			mem1[addr] = vmask.apply(value)             // part1
			for _, newaddr := range amask.apply(addr) { // part 2
				mem2[newaddr] = value
			}
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	fmt.Printf("part 1: %d\n", memsum(mem1))
	fmt.Printf("part 2: %d\n", memsum(mem2))
}
