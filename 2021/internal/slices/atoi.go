package slices

import "strconv"

func Atoi(strs []string) ([]int, error) {
	ints := []int{}
	for _, elem := range strs {
		num, err := strconv.Atoi(elem)
		if err != nil {
			return ints, err
		}
		ints = append(ints, num)
	}
	return ints, nil
}
