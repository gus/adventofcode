package num

// AbsInt returns the absolute value of int `n`.
func AbsInt(n int) int {
	if n > -1 {
		return n
	}
	return -1 * n
}

// MinInt returns the min int between a and b.
func MinInt(a, b int) int {
	if b < a {
		return b
	}
	return a
}

// MaxInt returns the max int between a and b.
func MaxInt(a, b int) int {
	if b > a {
		return b
	}
	return a
}
