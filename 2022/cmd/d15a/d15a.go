package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"regexp"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/gus/adventofcode/2022/internal/geom"
	"github.com/gus/adventofcode/2022/internal/maths"
	"github.com/gus/adventofcode/2022/internal/utils"
)

type sensor struct {
	spt geom.P2
	bpt geom.P2
	dis int
}

// ex. "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
var linePtn = regexp.MustCompile("-?[0-9]+")

func taxiDistance(a, b geom.P2) int {
	return maths.Abs(a.X-b.X) + maths.Abs(a.Y-b.Y)
}

func part1(sensors []sensor, mmx *maths.MinMax[int], y int) int {
	cnt := 0
	for x := mmx.Min; x <= mmx.Max; x++ {
		pt := geom.P2{X: x, Y: y}
		for _, sensor := range sensors {
			tdis := taxiDistance(sensor.spt, pt)
			if sensor.spt != pt && sensor.bpt != pt && tdis <= sensor.dis {
				cnt++
				break // only need to count once
			}
		}
	}
	return cnt
}

type rng [2]int

func (r rng) a() int {
	return r[0]
}

func (r rng) b() int {
	return r[1]
}

func applyRange(rngs []rng, newr rng) []rng {
	// apply new range to existing ranges, merging when applicable
	nrngs := []rng{}
	for i := 0; i <= len(rngs); i++ {
		if i == len(rngs) {
			return append(nrngs, newr) // newr outside all previous ranges, so just add it
		}

		curr := rngs[i]
		if curr.b() == newr.a()-1 || curr.a() == newr.b()+1 { // curr and newr are contiguous (merge)
			newr = rng{maths.Min(newr.a(), curr.a()), maths.Max(newr.b(), curr.b())}
			continue
		} else if curr.a() > newr.b() { // newr does not overlap with curr, but precedes it (insert)
			return append(append(nrngs, newr), rngs[i:]...)
		} else if curr.a() <= newr.a() && curr.b() >= newr.a() { // newr begins inside curr (merge)
			if curr.b() < newr.b() {
				newr = rng{curr.a(), newr.b()} // merge curr into newr, continue looking
				continue
			}
			// newr fits entirely inside of curr
			return append(nrngs, rngs[i:]...)
		} else if curr.a() >= newr.a() && curr.a() <= curr.b() { // curr begins inside newr (merge)
			if curr.b() < newr.b() {
				continue // curr fits entirely into newr, continue looking
			}
			// curr and newr overlap (merge)
			return append(append(nrngs, rng{newr.a(), curr.b()}), rngs[i+1:]...)
		}
		nrngs = append(nrngs, curr) // no overlaps, keep searching
	}
	return nrngs
}

func part2(sensors []sensor, xywin rng) int {
	// find first row which does not have full coverage
	minx, miny, maxx, maxy := xywin[0], xywin[0], xywin[1], xywin[1]
	x, y := -1, miny
	for ; y <= maxy && x == -1; y++ {
		rngs := []rng{} // track ranges of coverage per sensor for current y
		for _, s := range sensors {
			// get the -x and +x coverage range for sensor at this y
			xdis := maths.Max(s.dis-maths.Abs(s.spt.Y-y), 0) // x is taxi-dis minus dis from y
			if xdis == 0 || s.spt.X+xdis < minx || s.spt.X-xdis > maxx {
				continue // x-coverage for y is outside of window
			}

			// making some assumptions; constrain coverage at y to x-window
			rngx := rng{maths.Max(s.spt.X-xdis, minx), maths.Min(s.spt.X+xdis, maxx)}
			rngs = applyRange(rngs, rngx) // apply cumulative minmaxes
			if len(rngs) == 1 && rngs[0].a() == xywin.a() && rngs[0].b() == xywin.b() {
				break // whole line is covered
			}
		}
		if len(rngs) == 2 && (rngs[1].a()-rngs[0].b()) == 2 {
			x = rngs[0].b() + 1
			break
		}
	}
	return x*4000000 + y
}

func main() {
	mmx := &maths.MinMax[int]{}
	sensors := []sensor{}
	bs, _ := io.ReadAll(os.Stdin)
	for _, line := range bytes.Split(bs, []byte{'\n'}) {
		nums := slices.Atoi(linePtn.FindAllString(string(line), -1))
		spt := geom.P2{X: nums[0], Y: nums[1]}
		bpt := geom.P2{X: nums[2], Y: nums[3]}
		dis := taxiDistance(spt, bpt)
		sensor := sensor{spt: spt, bpt: bpt, dis: dis}

		mmx.Apply(sensor.spt.X - dis)
		mmx.Apply(sensor.spt.X + dis)
		sensors = append(sensors, sensor)
	}

	// TEST ARGS: 10 20
	// TEST: part 1.a (26)	part 2.a (4879972)
	// REAL ARGS: 2000000 4000000
	// REAL: part 1 (56000011)	part 2 (12525726647448)
	fmt.Printf("part 1 (%d)\tpart 2 (%d)\n",
		part1(sensors, mmx, utils.Atoi(os.Args[1])),
		part2(sensors, rng{0, utils.Atoi(os.Args[2])}))
}
