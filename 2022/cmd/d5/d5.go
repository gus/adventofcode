package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/utils"
)

func toStack(stacks [][]byte) []slices.Stack[byte] {
	return slices.Map(stacks, func(stack []byte) slices.Stack[byte] {
		return slices.NewStack(stack...)
	})
}

func readStacks(rdr *bufio.Reader) ([]slices.Stack[byte], []slices.Stack[byte]) {
	stacks := [][]byte{}
	for {
		line, _ := rdr.ReadBytes('\n')

		if len(stacks) == 0 {
			for i := 0; i < len(line)/4; i++ {
				stacks = append(stacks, []byte{})
			}
		}

		if line[1] >= '0' && line[1] <= '9' {
			break // line of stack ids, done reading
		}

		for i := 0; i < len(line); i += 4 {
			if crate := line[i+1]; crate != ' ' {
				id := i / 4
				stacks[id] = append(stacks[id], crate)
			}
		}
	}
	return toStack(stacks), toStack(stacks)
}

func readCommand(line string) (int, int, int) {
	if line[len(line)-1] == '\n' {
		line = line[0 : len(line)-1]
	}
	cmds := strings.Split(line, " ")
	return utils.Atoi(cmds[1]), utils.Atoi(cmds[3]) - 1, utils.Atoi(cmds[5]) - 1
}

func peekTops(stacks []slices.Stack[byte]) string {
	var tops string
	for _, stack := range stacks {
		if crate, ok := stack.Peek(); ok {
			tops += string(crate)
		}
	}
	return tops
}

func main() {
	rdr := bufio.NewReader(os.Stdin)
	stacks1, stacks2 := readStacks(rdr)
	rdr.ReadByte() // skip blank line

	var line []byte
	var err error
	for err != io.EOF {
		line, err = rdr.ReadBytes('\n')
		num, fromId, toId := readCommand(string(line))
		stacks1[toId].Push(stacks1[fromId].PopN(num)...)
		stacks2[toId].PushN(stacks2[fromId].PopN(num)...)
	}

	// test: part 1 (CMZ) part 2 (MCD)
	// real: part 1 (SHQWSRBDL) part 2 (CDTQZHBRS)
	fmt.Printf("part 1 (%s)\tpart 2 (%s)", peekTops(stacks1), peekTops(stacks2))
}
