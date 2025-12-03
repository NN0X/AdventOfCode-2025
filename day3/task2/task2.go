package main

import (
        "fmt"
        "io/ioutil"
        "strings"
        "math"
        "slices"
)

func getTwelveLargestFromLeftToRight(line string) []int {
        lineLen := len([]rune(line))
        if lineLen < 12 {
                return []int{0}
        }

        result := []int{}
        largestPos := -1
        for i := 0; i < 12; i++ {
                largest := 0
                for k := largestPos + 1; lineLen - k >= 12 - i; k++ {
                        charNum := int(line[k] - '0')
                        if charNum > largest {
                                largest = charNum
                                largestPos = k
                        }
                }
                result = append(result, largest)
        }

        return result
}

func getNumFromDigits(digits []int) int {
        num := 0
        slices.Reverse(digits)
        for pos, digit := range digits {
                num += digit * int(math.Pow(10.0, float64(pos)))
        }
        return num
}

func main() {
        data, err := ioutil.ReadFile("../../inputs/day3")
        if err != nil {
                panic(err)
        }

        dataString := string(data)
        lines := strings.Split(dataString, "\n")
        total := 0
        for _, line := range lines {
                digits := getTwelveLargestFromLeftToRight(line)
                num := getNumFromDigits(digits)
                total+=num
        }

        fmt.Println(total)
}
