package slices_test

import (
	"testing"

	"github.com/gus/adventofcode/2021/internal/slices"
	"github.com/stretchr/testify/assert"
)

func TestIntSeq(t *testing.T) {
	testCases := []struct {
		desc      string
		a         int
		b         int
		inclusive bool
		expected  []int
	}{
		{desc: "excl int(a < b)", a: 0, b: 3, inclusive: false, expected: []int{0, 1, 2}},
		{desc: "excl int(a > b)", a: 3, b: 0, inclusive: false, expected: []int{3, 2, 1}},
		{desc: "excl int(a = b)", a: 3, b: 3, inclusive: false, expected: []int{}},

		{desc: "incl int(a < b)", a: 0, b: 3, inclusive: true, expected: []int{0, 1, 2, 3}},
		{desc: "incl int(a > b)", a: 3, b: 0, inclusive: true, expected: []int{3, 2, 1, 0}},
		{desc: "incl int(a = b)", a: 3, b: 3, inclusive: true, expected: []int{3}},
	}
	for _, tt := range testCases {
		t.Run(tt.desc, func(t *testing.T) {
			seq := slices.Sequence(tt.a, tt.b, tt.inclusive)
			assert.Equal(t, tt.expected, seq)
		})
	}
}

func TestInt16Seq(t *testing.T) {
	testCases := []struct {
		desc      string
		a         int16
		b         int16
		inclusive bool
		expected  []int16
	}{
		{desc: "excl int(a < b)", a: 0, b: 3, inclusive: false, expected: []int16{0, 1, 2}},
		{desc: "excl int(a > b)", a: 3, b: 0, inclusive: false, expected: []int16{3, 2, 1}},
		{desc: "excl int(a = b)", a: 3, b: 3, inclusive: false, expected: []int16{}},

		{desc: "incl int(a < b)", a: 0, b: 3, inclusive: true, expected: []int16{0, 1, 2, 3}},
		{desc: "incl int(a > b)", a: 3, b: 0, inclusive: true, expected: []int16{3, 2, 1, 0}},
		{desc: "incl int(a = b)", a: 3, b: 3, inclusive: true, expected: []int16{3}},
	}
	for _, tt := range testCases {
		t.Run(tt.desc, func(t *testing.T) {
			seq := slices.Sequence(tt.a, tt.b, tt.inclusive)
			assert.Equal(t, tt.expected, seq)
		})
	}
}
