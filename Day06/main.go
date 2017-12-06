package main

import (
	"fmt"
	"strconv"
	"strings"
)

func contains(xs []string, x string) bool {
	for _, e := range xs {
		if e == x {
			return true
		}
	}
	return false
}

func max(xs []int) (int, int) {
	idx := -1
	max := 0
	for i, e := range xs {
		if e > max {
			if i != -1 {
				idx = i
			}
			max = e
		}
	}
	return idx, max
}

func toStr(xs []int) string {
	str := ""
	for _, e := range xs {
		str = str + " " + strconv.Itoa(e)
	}
	return str
}

func part2(xs []string, x string) int {
	for i, e := range xs {
		if e == x {
			return len(xs) - i
		}
	}
	return -1
}

func main() {
	input := "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4"
	var seen []string
	blocks := strings.Split(input, " ")
	inputLength := len(blocks)
	var blockNums []int
	for _, b := range blocks {
		num, _ := strconv.Atoi(b)
		blockNums = append(blockNums, num)
	}

	steps := 0
	blockNumStrings := input
	for !contains(seen, blockNumStrings) {
		seen = append(seen, blockNumStrings)
		idx, maxNum := max(blockNums)
		blockNums[idx] = 0
		for maxNum > 0 {
			idx = (idx + 1) % inputLength
			blockNums[idx] = blockNums[idx] + 1
			maxNum = maxNum - 1
		}
		blockNumStrings = toStr(blockNums)
		steps = steps + 1
	}

	fmt.Println("Part 1: " + strconv.Itoa(steps))
	fmt.Println("Part 2: " + strconv.Itoa(part2(seen, blockNumStrings)))
}
