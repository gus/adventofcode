package slices

import "sort"

// Int64 represents a slice of int64s.
type Int64 []int64

// Append appends an int64 to this slice.
func (s Int64) Append(v int64) Int64 { return append(s, v) }

// Get returns element at position v. If v is negative, return from the end.
func (s Int64) Get(i int) int64 {
	if i < 0 {
		return s[len(s)+i]
	}
	return s[i]
}

// LastIndex returns the index of the last element.
func (s Int64) LastIndex() int { return len(s) - 1 }

func (s Int64) Len() int           { return len(s) }
func (s Int64) Less(i, j int) bool { return s[i] < s[j] }
func (s Int64) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }

// Sort is a convenience method.
func (s Int64) Sort() { sort.Sort(s) }
