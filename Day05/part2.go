package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func readLines(path string) ([]int64, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var lines []int64
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line, _ := strconv.ParseInt(scanner.Text(), 10, 0)
		lines = append(lines, line)
	}
	return lines, scanner.Err()
}

func main() {
	lines, err := readLines("input.txt")
	if err != nil {
		log.Fatalf("readLines: %s", err)
	}
	idx := int64(0)
	steps := 0
	numLines := int64(len(lines))

  part := 2

	for idx >= 0 && idx < numLines {
		num := lines[idx]
		if num >= 3 && part == 2 {
			lines[idx] = num - 1
		} else {
			lines[idx] = num + 1
		}
		steps = steps + 1
		idx = idx + num
	}
	fmt.Println(steps)
}
