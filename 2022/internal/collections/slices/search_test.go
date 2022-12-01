package slices_test

import (
	"testing"

	"github.com/gus/adventofcode/2022/internal/collections/slices"
	"github.com/stretchr/testify/require"
)

func TestFindStrings(t *testing.T) {
	type wants struct {
		idx  int
		elem string
	}
	testCases := []struct {
		desc string
		in   []string
		fn   slices.SearchFn[string]
		want wants
	}{
		{
			desc: "strings",
			in:   []string{"abcd", "def", "ghijk"},
			fn:   func(elem string) bool { return len(elem) == 3 },
			want: wants{idx: 1, elem: "def"},
		},
		{
			desc: "not found",
			in:   []string{"abcd", "def", "ghijk"},
			fn:   func(elem string) bool { return len(elem) == 10 },
			want: wants{idx: -1, elem: ""},
		},
	}
	for _, tt := range testCases {
		t.Run(tt.desc, func(t *testing.T) {
			idx, elem := slices.First(tt.in, tt.fn)
			require.Equal(t, tt.want.elem, elem)
			require.Equal(t, tt.want.idx, idx)
		})
	}
}
