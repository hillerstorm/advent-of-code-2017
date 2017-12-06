package main

import (
	"fmt"
	"strconv"
	"strings"
)

func contains(xs []string, x string, cnt int) bool {
	seenCount := 0
	for _, e := range xs {
		if e == x {
			seenCount = seenCount + 1
			if seenCount == cnt {
				return true
			}
		}
	}
	return false
}

func max(xs []int64) (int, int64) {
	idx := -1
	max := int64(0)
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

func toStr(xs []int64) string {
	str := ""
	for _, e := range xs {
		str = str + " " + strconv.FormatInt(e, 10)
	}
	return str
}

func main() {
	input := "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4"
	var seen []string
	blocks := strings.Split(input, " ")
	inputLength := len(blocks)
	var blockNums []int64
	for _, b := range blocks {
		num, _ := strconv.ParseInt(b, 10, 0)
		blockNums = append(blockNums, num)
	}

	steps := int64(0)
	blockNumStrings := toStr(blockNums)
	for !contains(seen, blockNumStrings, 1) {
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

	fmt.Println("Part 1: " + strconv.FormatInt(steps, 10))

	steps = int64(0)
	for !contains(seen, blockNumStrings, 2) {
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

	fmt.Println("Part 2: " + strconv.FormatInt(steps, 10))
}
