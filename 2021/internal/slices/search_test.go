package slices_test

import (
	"testing"

	"github.com/gus/adventofcode/2021/internal/slices"
	"github.com/stretchr/testify/assert"
)

func TestFindStrings(t *testing.T) {
	testCases := []struct {
		desc string
		in   []string
		fn   slices.SearchFn[string]
		out  string
	}{
		{
			desc: "strings",
			in:   []string{"abcd", "def", "ghijk"},
			fn:   func(elem string) bool { return len(elem) == 3 },
			out:  "def",
		},
	}
	for _, tt := range testCases {
		t.Run(tt.desc, func(t *testing.T) {
			assert.Equal(t, tt.out, slices.Find(tt.in, tt.fn))
		})
	}
}
