package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func readLines(path string) ([]int, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var lines []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line, _ := strconv.Atoi(scanner.Text())
		lines = append(lines, line)
	}
	return lines, scanner.Err()
}

func countSteps(lines []int, part2 bool) int {
	clone := append([]int(nil), lines...)
	steps := 0
	numLines := len(clone)
	for idx := 0; idx >= 0 && idx < numLines; {
		num := clone[idx]
		if num >= 3 && part2 {
			clone[idx] = num - 1
		} else {
			clone[idx] = num + 1
		}
		steps = steps + 1
		idx = idx + num
	}
	return steps
}

func main() {
	lines, err := readLines("input.txt")
	if err != nil {
		log.Fatalf("readLines: %s", err)
	}
	fmt.Println("Part 2: " + strconv.Itoa(countSteps(lines, false)))
	fmt.Println("Part 1: " + strconv.Itoa(countSteps(lines, true)))
}
