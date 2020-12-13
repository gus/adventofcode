package num

// AbsInt returns the absolute value of int `n`.
func AbsInt(n int) int {
	if n > -1 {
		return n
	}
	return -1 * n
}
