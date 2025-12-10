package task1

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"

printFlags16b :: proc(flags : []u16)
{
        for flag in flags
        {
                fmt.printf("%16b\n", flag)
        }
}

getFewestButtonPresses :: proc(light: u16, buttons: []u16) -> (result: u64)
{
        costs := make(map[u16]u64)
        defer delete(costs)

        queue := make([dynamic][2]u16)
        defer delete(queue)

        costs[0] = 0
        append(&queue, [2]u16{0, 0})

        for len(queue) > 0
        {
                current := queue[0]
                state := current[0]
                cost := u64(current[1])
                ordered_remove(&queue, 0)

                if state in costs && costs[state] < cost
                {
                        continue
                }

                for button in buttons
                {
                        newState := state ~ button
                        newCost := cost + 1

                        if newState not_in costs || costs[newState] > newCost
                        {
                                costs[newState] = newCost
                                append(&queue, [2]u16{newState, u16(newCost)})
                        }
                }
        }

        if light in costs
        {
                result = costs[light]
        }
        else
        {
                result = 0xFFFFFFFFFFFFFFFF
        }

        return
}
getFewestButtonPressesPerLight :: proc(lights : []u16, buttons : [][dynamic]u16) -> (result: [dynamic]u64)
{
        result = make([dynamic]u64, 0, 200)
        for i := 0; i < len(lights); i += 1
        {
                light   := lights[i]
                buttonSet := buttons[i]

                presses := getFewestButtonPresses(light, buttonSet[:])
                append(&result, presses)
        }

        return
}

main :: proc()
{
        data, ok := os.read_entire_file("../inputs/day10")
        if !ok
        {
                return
        }
        defer delete(data)

        targetLights := make([dynamic]u16, 0, 200)
        buttonsPerLight := make([dynamic][dynamic]u16, 0, 200)

        dataIt := string(data)
        for line in strings.split_lines_iterator(&dataIt)
        {
                splits := strings.split(line, " ")
                buttons := make([dynamic]u16, 0, 200)
                for split in splits
                {
                        splitSignificant, ok := strings.substring(split, 1, len(split) - 1)
                        switch split[0]
                        {
                        case '[':
                                targetLight: u16
                                shift: u16
                                for char in splitSignificant
                                {
                                        if char == '#'
                                        {
                                                targetLight |= 1 << shift
                                        }
                                        shift+=1
                                }
                                append(&targetLights, targetLight)
                        case '(':
                                buttonValue: u16 = 0
                                numbers := strings.split(splitSignificant, ",")
                                for number in numbers
                                {
                                        num, ok := strconv.parse_uint(number, 10)
                                        if !ok
                                        {
                                                return
                                        }
                                        buttonValue |= 1 << u16(num)
                                }
                                append(&buttons, buttonValue)
                        }
                }
                append(&buttonsPerLight, buttons)
        }

        buttonsToPress := getFewestButtonPressesPerLight(targetLights[:], buttonsPerLight[:])

        sumOfPresses: u64 = 0
        for presses in buttonsToPress
        {
                sumOfPresses += presses
        }
        fmt.println(sumOfPresses)
}
