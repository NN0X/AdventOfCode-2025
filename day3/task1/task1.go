package main

import (
        "io/ioutil"
        "fmt"
        "strings"
)

func getTwoLargestLeftToRight(line string) (int, int) {
        a := 0
        aPos := 0

        lineLen := len([]rune(line))
        if lineLen < 2 {
                return 0, 0
        }

        for pos, char := range line[0:lineLen - 1] {
                charNum := int(char - '0')
                if charNum > a {
                        a = charNum
                        aPos = pos
                }
        }

        b := 0
        for _, char := range line[aPos + 1:lineLen] {
                charNum := int(char - '0')
                if charNum > b {
                        b = charNum
                }
        }

        return a, b
}

func getNumFromTwoDigits(tens int, ones int) int {
        return 10*tens + ones
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
                tens, ones := getTwoLargestLeftToRight(line)
                num := getNumFromTwoDigits(tens, ones)
                total+=num
        }

        fmt.Println(total)
}
