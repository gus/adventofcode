package main

import (
	"bufio"
	"fmt"
	"log"
	"os"

	"github.com/gus/adventofcode/2022/internal/utils"
)

type CRT struct {
	X      int
	Cycle  int
	Sample int
	Width  int
	pixels []int
}

func (crt *CRT) Tick() {
	// record strength sample
	if crt.Cycle > 0 && (crt.Cycle-20)%40 == 0 {
		crt.Sample += crt.Cycle * crt.X
	}

	// update pixel
	pos := (crt.Cycle - 1) % 40
	crt.pixels = append(crt.pixels, utils.Btoi(pos >= crt.X-1 && pos <= crt.X+1))

	crt.Cycle++
}

func (crt *CRT) Draw() string {
	charmap, screen := ".#", []byte{}
	for pos, lit := range crt.pixels {
		if pos > 0 && pos%40 == 0 {
			screen = append(screen, '\n')
		}
		screen = append(screen, charmap[lit])
	}
	return string(screen)
}

func main() {
	crt := &CRT{X: 1, Cycle: 1, Sample: 0, Width: 40}

	buf := bufio.NewScanner(os.Stdin)
	for buf.Scan() {
		instr := buf.Text()
		crt.Tick()
		switch instr[0:4] {
		case "addx":
			crt.Tick()
			crt.X += utils.Atoi(instr[5:])
		}
	}
	if err := buf.Err(); err != nil {
		log.Fatalf("error reading input: %v", err)
	}

	// TEST: part 1.a (13140)	part 2.a (see below)
	// REAL: part 1 (14920)	part 2 (see below)
	fmt.Printf("part 1 (%d)\n\n", crt.Sample)
	fmt.Print(crt.Draw())
}

// TEST: part 2.a
// ##  ##  ##  ##  ##  ##  ##  ##  ##  ##
// ###   ###   ###   ###   ###   ###   ###
// ####    ####    ####    ####    ####
// #####     #####     #####     #####
// ######      ######      ######      ####
// #######       #######       #######

// REAL: part 2
// ###  #  #  ##   ##   ##  ###  #  # ####
// #  # #  # #  # #  # #  # #  # #  #    #
// ###  #  # #    #  # #    ###  #  #   #
// #  # #  # #    #### #    #  # #  #  #
// #  # #  # #  # #  # #  # #  # #  # #
// ###   ##   ##  #  #  ##  ###   ##  ####
