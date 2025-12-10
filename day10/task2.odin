package task1

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"

foreign import glpk "system:glpk"

GLP_MIN :: 1
GLP_MAX :: 2
GLP_CV :: 1
GLP_IV :: 2
GLP_BV :: 3
GLP_FX :: 5
GLP_LO :: 2

glp_prob :: struct {}

foreign glpk
{
        glp_create_prob :: proc() -> ^glp_prob ---
        glp_delete_prob :: proc(lp: ^glp_prob) ---
        glp_set_prob_name :: proc(lp: ^glp_prob, name: cstring) ---
        glp_set_obj_dir :: proc(lp: ^glp_prob, dir: i32) ---
        glp_add_rows :: proc(lp: ^glp_prob, nrs: i32) -> i32 ---
        glp_add_cols :: proc(lp: ^glp_prob, ncs: i32) -> i32 ---
        glp_set_col_kind :: proc(lp: ^glp_prob, j: i32, kind: i32) ---
        glp_set_obj_coef :: proc(lp: ^glp_prob, j: i32, coef: f64) ---
        glp_set_row_bnds :: proc(lp: ^glp_prob, i: i32, type: i32, lb: f64, ub: f64) ---
        glp_set_mat_row :: proc(lp: ^glp_prob, i: i32, len: i32, ind: [^]i32, val: [^]f64) ---
        glp_simplex :: proc(lp: ^glp_prob, parm: rawptr) -> i32 ---
        glp_intopt :: proc(lp: ^glp_prob, parm: rawptr) -> i32 ---
        glp_mip_obj_val :: proc(lp: ^glp_prob) -> f64 ---
        glp_mip_col_val :: proc(lp: ^glp_prob, j: i32) -> f64 ---
        glp_set_col_bnds :: proc(lp: ^glp_prob, j: i32, type: i32, lb: f64, ub: f64) ---
}

printFlags16b :: proc(flags: []u16)
{
        for flag in flags
        {
                fmt.printf("%16b\n", flag)
        }
}

vectorize :: proc(buttons: []u16) -> (buttonVecs: [dynamic][16]u8)
{
        buttonVecs = make([dynamic][16]u8, 0, 200)
        for button in buttons
        {
                vec: [16]u8
                for i := 0; i < 16; i += 1
                {
                        if (button & (1 << u16(i))) != 0
                        {
                                vec[i] = 1
                        }
                        else
                        {
                                vec[i] = 0
                        }
                }
                append(&buttonVecs, vec)
        }
        return
}

getFewestButtonPresses :: proc(counters: []u16, buttons: []u16) -> u64
{
        lp := glp_create_prob()
        defer glp_delete_prob(lp)

        nButtons := i32(len(buttons))
        glp_add_cols(lp, nButtons)

        for i in 0 ..< nButtons
        {
                glp_set_col_kind(lp, i + 1, GLP_IV)
                glp_set_col_bnds(lp, i + 1, GLP_LO, 0.0, 0.0)
                glp_set_obj_coef(lp, i + 1, 1.0)
        }

        nConstraints := i32(len(counters))
        glp_add_rows(lp, nConstraints)

        for counterIdx in 0 ..< len(counters)
        {
                target := counters[counterIdx]

                indices := make([dynamic]i32, 0, nButtons)
                values := make([dynamic]f64, 0, nButtons)
                defer delete(indices)
                defer delete(values)

                append(&indices, 0)
                append(&values, 0.0)

                for btnIdx in 0 ..< len(buttons)
                {
                        if (buttons[btnIdx] & (1 << u16(counterIdx))) != 0
                        {
                                append(&indices, i32(btnIdx) + 1)
                                append(&values, 1.0)
                        }
                }

                glp_set_row_bnds(lp, i32(counterIdx) + 1, GLP_FX, f64(target), f64(target))
                glp_set_mat_row(lp, i32(counterIdx) + 1, i32(len(indices)) - 1, raw_data(indices), raw_data(values))
        }

        glp_set_obj_dir(lp, GLP_MIN)
        glp_simplex(lp, nil)
        glp_intopt(lp, nil)

        return u64(glp_mip_obj_val(lp) + 0.5)
}

getFewestButtonPressesPerCounters :: proc(countersPerLine: [][dynamic]u16, buttons: [][dynamic]u16) -> (result: [dynamic]u64)
{
        result = make([dynamic]u64, 0, 200)
        for i := 0; i < len(countersPerLine); i += 1
        {
                counters := countersPerLine[i]
                buttonSet := buttons[i]

                presses := getFewestButtonPresses(counters[:], buttonSet[:])
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

        countersPerLine := make([dynamic][dynamic]u16, 0, 200)
        buttonsPerCounter := make([dynamic][dynamic]u16, 0, 200)

        dataIt := string(data)
        for line in strings.split_lines_iterator(&dataIt)
        {
                splits := strings.split(line, " ")
                buttons := make([dynamic]u16, 0, 200)
                counters := make([dynamic]u16, 0, 200)
                for split in splits
                {
                        splitSignificant, ok := strings.substring(split, 1, len(split) - 1)
                        switch split[0]
                        {
                        case '{':
                                numbers := strings.split(splitSignificant, ",")
                                for number in numbers
                                {
                                        num, ok := strconv.parse_uint(number, 10)
                                        if !ok
                                        {
                                                return
                                        }
                                        append(&counters, cast(u16)num)
                                }
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
                append(&buttonsPerCounter, buttons)
                append(&countersPerLine, counters)
        }

        buttonsToPress := getFewestButtonPressesPerCounters(countersPerLine[:], buttonsPerCounter[:])

        sumOfPresses: u64 = 0
        for presses in buttonsToPress
        {
                sumOfPresses += presses
        }
        fmt.println(sumOfPresses)
}
