package utils

import "strconv"

func Atoi(s string) int {
	i, _ := strconv.Atoi(s)
	return i
}

func Btoi(b bool) int {
	if b {
		return 1
	}
	return 0
}
