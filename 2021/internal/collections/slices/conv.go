package slices

import "strconv"

func Atoi(strs []string) ([]int, error) {
	return MapErr(strs, func(str string) (int, error) {
		return strconv.Atoi(str)
	})
}
