package slices

import (
	"github.com/gus/adventofcode/2022/internal/utils"
)

func Atoi(strs []string) []int {
	return Map(strs, func(str string) int {
		return utils.Atoi(str)
	})
}
