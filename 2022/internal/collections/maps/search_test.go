package maps_test

import (
	"testing"

	"github.com/gus/adventofcode/2022/internal/collections/maps"
	"github.com/stretchr/testify/require"
)

func TestAny_StringsInts(t *testing.T) {
	testCases := []struct {
		desc string
		in   map[string]int
		fn   maps.SearchFn[string, int]
		want bool
	}{
		{
			desc: "found via value",
			in:   map[string]int{"a": 0, "b": 1, "c": 2, "d": 3},
			fn:   func(k string, v int) bool { return v == 3 },
			want: true,
		},
		{
			desc: "found via key",
			in:   map[string]int{"a": 0, "b": 1, "c": 2, "d": 3},
			fn:   func(k string, v int) bool { return k == "b" },
			want: true,
		},
		{
			desc: "not found",
			in:   map[string]int{"a": 0, "b": 1, "c": 2, "d": 3},
			fn:   func(k string, v int) bool { return false },
			want: false,
		},
	}
	for _, tt := range testCases {
		t.Run(tt.desc, func(t *testing.T) {
			got := maps.Any(tt.in, tt.fn)
			require.Equal(t, tt.want, got)
		})
	}
}
