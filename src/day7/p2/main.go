package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"

	"github.com/thoas/go-funk"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

const TOTAL_DISK_SPACE = 70000000
const UPDATE_SPACE = 30000000

type FileSystemItem struct {
	name     string
	size     int64
	contents []FileSystemItem
}

func main() {
	dat, err := ioutil.ReadFile("./input.txt")
	check(err)
	input := string(dat)

	tree := FileSystemItem{"/", 0, []FileSystemItem{}}

	parse(input, &tree, []*FileSystemItem{})
	fmt.Println(tree)
	printTree(tree, 0)

	part2(tree)
}

func parse(input string, tree *FileSystemItem, treeStack []*FileSystemItem) {
	line, lines := partition(input)

	// No-op on the start
	if line == "$ cd /" {
		println("Handling", line)
		parse(lines, tree, treeStack)
		return
	}

	// Handle reading a directories content
	if strings.HasPrefix(line, "$ ls") {
		files := getLsOutput(lines)
		fmt.Println("Handling", line, files)
		tree.contents = files
		linesLeft := strings.Join(strings.Split(lines, "\n")[len(files):], "\n")
		parse(linesLeft, tree, treeStack)
		return
	}

	if strings.HasPrefix(line, "$ cd ..") {
		println("Handling", line)
		stackSize := len(treeStack) - 1
		parse(lines, treeStack[stackSize], treeStack[:stackSize])
		return
	}

	if strings.HasPrefix(line, "$ cd") {
		println("Handling", line)
		vals := strings.Split(line, " ")
		dirName := vals[2]
		subTree := getSubTree(tree, dirName)
		parse(lines, subTree, append(treeStack, tree))
		return
	}
}

func getSubTree(tree *FileSystemItem, dirName string) *FileSystemItem {
	for i, item := range tree.contents {
		if item.name == dirName {
			return &tree.contents[i]
		}
	}

	panic("Failed to find dir" + dirName)

}

func partition(input string) (string, string) {
	lines := strings.Split(input, "\n")
	return lines[0], strings.Join(lines[1:], "\n")
}

func getLsOutput(input string) []FileSystemItem {
	lines := strings.Split(input, "\n")[:]

	isEndOfInput := func(line string) bool {
		return strings.HasPrefix(line, "$")
	}

	index := len(lines)
	for i, s := range lines {
		if isEndOfInput(s) {
			index = i
			break
		}
	}

	files := lines[0:index]

	return map2(files, mapToFileSystemItem)
}

// dir a
// 14848514 b.txt
func mapToFileSystemItem(line string) FileSystemItem {
	vals := strings.Split(line, " ")
	if vals[0] == "dir" {
		return FileSystemItem{vals[1], 0, []FileSystemItem{}}
	}

	size, err := strconv.ParseInt(vals[0], 10, 64)
	check(err)

	return FileSystemItem{
		vals[1],
		size,
		[]FileSystemItem{},
	}
}

func map2(data []string, f func(string) FileSystemItem) []FileSystemItem {
	mapped := make([]FileSystemItem, len(data))

	for i, e := range data {
		mapped[i] = f(e)
	}

	return mapped
}

// Utils
func printTree(tree FileSystemItem, depth int) {
	for i := 0; i < depth; i++ {
		fmt.Print(" ")
	}
	if tree.size != 0 {
		fmt.Print("- ", tree.name, " (file, size=", tree.size, ")\n")
	} else {
		fmt.Print("- ", tree.name, "(dir)\n")
		for i, _ := range tree.contents {
			printTree(tree.contents[i], depth+2)
		}
	}
}

func size(tree FileSystemItem) int64 {
	if tree.size > 0 {
		return tree.size
	}
	totalSize := int64(0)
	for _, item := range tree.contents {
		totalSize += size(item)
	}
	return totalSize
}

func part2(tree FileSystemItem) {
	sizeOfDirs := []int64{}

	search(&tree, &sizeOfDirs)
	fmt.Println(sizeOfDirs)

	spaceLeft := (TOTAL_DISK_SPACE - size(tree))
	spaceToBeFreed := UPDATE_SPACE - spaceLeft
	fmt.Println(spaceToBeFreed)

	validDirs := funk.Filter(sizeOfDirs, func(size int64) bool {
		return size > spaceToBeFreed
	})

	smallestDir := funk.Reduce(validDirs, func(a int64, b int64) int64 {
		if a < b {
			return a
		}
		return b
	}, TOTAL_DISK_SPACE)

	fmt.Println(smallestDir)
}

func search(tree *FileSystemItem, sizeOfDirs *[]int64) {
	if tree.size == 0 {
		dirSize := size(*tree)
		*sizeOfDirs = append(*sizeOfDirs, dirSize)
	}

	for i, _ := range tree.contents {
		search(&tree.contents[i], sizeOfDirs)
	}
}
