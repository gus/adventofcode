package maps

// Int64 is maps of ints mapping to int64 keys to int64 values
type Int64 map[int64]int64

// Get returns the value for k or the default and returns the same value. The default
// value will also be assigned to k.
func (m Int64) Get(k, def int64) int64 {
	if _, ok := m[k]; !ok {
		m[k] = def
	}
	return m[k]
}

// Incr increments (or decrements) the value of k by the amount provided and returns
// the new value.
func (m Int64) Incr(k, amt int64) int64 {
	m[k] = m.Get(k, 0) + amt
	return m[k]
}
