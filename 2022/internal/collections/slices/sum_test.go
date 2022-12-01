package slices_test

import (
	"testing"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/stretchr/testify/assert"
)

func TestSum_Ints(t *testing.T) {
	testCases := []struct {
		desc string
		in   []int
		out  int
	}{
		{
			desc: "postive ints",
			in:   []int{1, 2, 3, 4, 5},
			out:  15,
		},
		{
			desc: "negative ints",
			in:   []int{-1, -2, -3, -4, -5},
			out:  -15,
		},
	}
	for _, tt := range testCases {
		t.Run(tt.desc, func(t *testing.T) {
			assert.Equal(t, tt.out, slices.Sum(tt.in))
		})
	}
}

func TestSum_Uints(t *testing.T) {
	testCases := []struct {
		desc string
		in   []uint
		out  uint
	}{
		{
			desc: "postive uints",
			in:   []uint{1, 2, 3, 4, 5},
			out:  15,
		},
	}
	for _, tt := range testCases {
		t.Run(tt.desc, func(t *testing.T) {
			assert.Equal(t, tt.out, slices.Sum(tt.in))
		})
	}
}
