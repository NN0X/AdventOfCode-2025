; nasm -f elf64 task1.asm && ld task1.o -o task1 && ./task1

section .data
filename: db "../inputs/day4", 0
filesize: dq 0

conversionbuff: times 21 db " "
conversionlen: dq 0

conversionpointer: dq 0

printbuffsize: dq 0

diagrambuffsize: dq 0

rollsmarkedforremovalbuffsize: dq 0

numnewlines: dq 0
curnewline: dq 0
curpos: dq 0

numaround: dq 0
result: dq 0
totalresult: dq 0

empty: db '.'
taken: db '@'
newline: db 10

section .bss
filebuffpointer: resb 8                 ; pointer to buffer used by _load_file
filemetabuff: resb 144                  ; fstat buffer

printbuffpointer: resb 8                ; pointer to buffer used by _print
diagrambuffpointer: resb 8              ; pointer to input diagram (buffer contains 0 or 1)
rollsmarkedforremovalpointer: resb 8    ; pointer to buffer of rolls marked for removal
newlinelenbufferpointer: resb 8         ; pointer to buffer of line lengths

topleftpointer: resb 8
toppointer: resb 8
toprightpointer: resb 8
rightpointer: resb 8
bottomrightpointer: resb 8
bottompointer: resb 8
bottomleftpointer: resb 8
leftpointer: resb 8

section .text
global _start

_start:
        call _load_file

        call _load_rolls_and_setup_buffers

loop_until_result_zero_start:

        call _get_num_accessible_rolls_and_mark_deletion

        mov rbx, [totalresult]
        add rbx, [result]
        mov [totalresult], rbx

        call _delete_marked_rolls

        cmp qword [result], 0
        jne loop_until_result_zero_start

        ; print result
        mov r8, [totalresult]
        call _int_to_string
        mov r10, [conversionpointer]
        mov [printbuffpointer], r10
        mov r10, [conversionlen]
        mov [printbuffsize], r10
        call _print

        mov r10, newline
        mov [printbuffpointer], r10
        mov qword [printbuffsize], 1
        call _print

        ; exit
        mov rax, 60
        mov rdi, 0
        syscall

_int_to_string:
        push rbx
        push rsi

        mov rax, r8
        mov rbx, 10
        lea rsi, [conversionbuff+19]

        cmp rax, 0
        jne conversion_loop
        ; special case: 0
        mov byte [conversionbuff+19], '0'
        lea rax, [conversionbuff+19]
        mov rdx, 1
        jmp conversion_end

conversion_loop:
        xor rdx, rdx
        div rbx                         ; rax / 10 -> quotient in rax, remainder in rdx
        add dl, '0'                     ; convert remainder to ASCII
        dec rsi
        mov [rsi], dl
        test rax, rax
        jnz conversion_loop

        ; compute length and return pointer
        lea rax, [rsi]                  ; start of string
        mov rdx, conversionbuff+19
        sub rdx, rax                    ; length = end - start

conversion_end:
        mov [conversionpointer], rax
        mov [conversionlen], rdx

        pop rsi
        pop rbx
        ret

; print [printbuffsize] bytes from at address [printbuffpointer]
_print:
        mov rax, 1
        mov rdi, 1
        mov rsi, [printbuffpointer]
        mov rdx, [printbuffsize]
        syscall

        ret

; load filename file at address [filebuffpointer]
_load_file:
        ; open filename
        mov rax, 2      ; open
        mov rdi, filename
        mov rsi, 0
        mov rdx, 0
        syscall

        ; move fd to rbx
        mov rbx, rax

        ; get filesize
        mov rax, 5      ; fstat
        mov rdi, rbx
        mov rsi, filemetabuff
        syscall

        ; save file size
        mov rax, [filemetabuff + 48]
        mov [filesize], rax

        ; allocate mem for filebuff
        mov rax, 9      ; mmap
        mov rdi, 0
        mov rsi, [filesize]
        mov rdx, 3
        mov r10, 0x22   ; PROT_READ | PROT_WRITE
        mov r8, -1
        mov r9, 0
        syscall

        ; save file buffer pointer
        mov [filebuffpointer], rax

        ; read from file filesize bytes to filebuffpointer
        mov rax, 0
        mov rdi, rbx
        mov rsi, [filebuffpointer]
        mov rdx, [filesize]
        syscall

        ; print file contents
        ;mov rax, [filebuffpointer]
        ;mov [printbuffpointer], rax
        ;mov rax, [filesize]
        ;mov [printbuffsize], rax
        ;call _print

        ;mov r10, newline
        ;mov [printbuffpointer], r10
        ;mov [printbuffsize], 1
        ;call _print

        ; close file
        mov rax, 3
        mov rdi, rbx
        syscall

        ret

; load diagram into diagrambuffpointer and setup other buffers
_load_rolls_and_setup_buffers:

        ; save size of diagram
        mov rdx, [filesize]
        shl rdx, 3      ; shift left 3 times to multiply by 2^3
        mov [diagrambuffsize], rdx

        ; allocate mem for diagram
        mov rax, 9      ; mmap
        mov rdi, 0
        mov rsi, [diagrambuffsize]
        mov rdx, 3
        mov r10, 0x22   ; PROT_READ | PROT_WRITE
        mov r8, -1
        mov r9, 0
        syscall

        ; save pointer to diagram buffer
        mov [diagrambuffpointer], rax

        ; save rolls marked for removal buffer size
        mov rdx, [diagrambuffsize]
        mov [rollsmarkedforremovalbuffsize], rdx

        ; allocate mem for rolls marked for removal
        mov rax, 9      ; mmap
        mov rdi, 0
        mov rsi, [rollsmarkedforremovalbuffsize]
        mov rdx, 3
        mov r10, 0x22   ; PROT_READ | PROT_WRITE
        mov r8, -1
        mov r9, 0
        syscall

        mov [rollsmarkedforremovalpointer], rax

        ; allocate mem for line lengths
        mov rax, 9      ; mmap
        mov rdi, 0
        mov rsi, [diagrambuffsize]
        mov rdx, 3
        mov r10, 0x22   ; PROT_READ | PROT_WRITE
        mov r8, -1
        mov r9, 0
        syscall
        mov [newlinelenbufferpointer], rax

        ; init for loop
        mov r8, 0

        ; move consts to registers
        movzx r11, byte [taken]
        movzx r12, byte [empty]
        movzx r13, byte [newline]

; iterate characters in file
iterate_chars_start:
        ; if r8 < filesize
        cmp r8, [filesize]
        jge iterate_chars_end

        mov r10, r8
        add r10, [filebuffpointer]
        cmp [r10], r11b
        je set_taken
        cmp [r10], r12b
        je set_empty
        cmp [r10], r13b
        je set_newline

        ; default
        jmp set_if_end

set_taken:
        mov r10, r8
        shl r10, 3
        add r10, [diagrambuffpointer]
        mov qword [r10], 1
        inc qword [curpos]
        jmp set_if_end
set_empty:
        mov r10, r8
        shl r10, 3
        add r10, [diagrambuffpointer]
        mov qword [r10], 0
        inc qword [curpos]
        jmp set_if_end
set_newline:
        mov r10, r8
        shl r10, 3
        add r10, [diagrambuffpointer]
        mov qword [r10], 2
        mov rbx, [newlinelenbufferpointer]
        mov rax, [curnewline]
        shl rax, 3
        add rbx, rax
        mov rax, [curpos]
        mov [rbx], rax
        mov qword [curpos], 0
        inc qword [numnewlines]
        inc qword [curnewline]
set_if_end:

        inc r8
        jmp iterate_chars_start

iterate_chars_end:

        mov qword [curpos], 0
        mov qword [curnewline], 0

; count accessible rolls
_get_num_accessible_rolls_and_mark_deletion:

        ; reset variables
        mov qword [result], 0
        mov qword [numaround], 0
        mov qword [curpos], 0
        mov qword [curnewline], 0

        ; init for loop
        mov r9, 0

; iterate characters in file
iterate_diagram_start:
        ; if r8 < diagrambuffsize
        cmp r9, [diagrambuffsize]
        jge iterate_diagram_end

        mov r10, [diagrambuffpointer]
        add r10, r9

        mov r8, [r10]

        ; ..@..... len = 8
        ; ...@...  pos = 3
        ; ....@.

        ; calculate pointers
        mov rbx, [newlinelenbufferpointer]
        mov rax, [curnewline]
        shl rax, 3
        add rbx, rax
        mov rbx, [rbx]
        inc rbx
        shl rbx, 3
        mov r14, rbx

        mov r15, [diagrambuffpointer]
        add r15, r9

        lea rax, [r15 - 8]
        mov [leftpointer], rax

        lea rax, [r15 + 8]
        mov [rightpointer], rax

        mov rax, r15
        sub rax, r14
        mov [toppointer], rax

        lea rbx, [rax - 8]
        mov [topleftpointer], rbx

        lea rbx, [rax + 8]
        mov [toprightpointer], rbx

        mov rax, r15
        add rax, r14
        mov [bottompointer], rax

        lea rbx, [rax - 8]
        mov [bottomleftpointer], rbx

        lea rbx, [rax + 8]
        mov [bottomrightpointer], rbx

        ; if (r8 == 1) check
        cmp r8, 1
        jne roll_check_end

        ; if (curnewline != 0) checktop
        cmp qword [curnewline], 0
        je top_check_end

        ; check topleft
        mov rax, [topleftpointer]
        mov rbx, [rax]
        cmp rbx, 1
        jne check_top
        inc qword [numaround]

check_top:
        ; check top
        mov rax, [toppointer]
        mov rbx, [rax]
        cmp rbx, 1
        jne check_topright
        inc qword [numaround]

check_topright:
        ; check topright
        mov rax, [toprightpointer]
        mov rbx, [rax]
        cmp rbx, 1
        jne top_check_end
        inc qword [numaround]

top_check_end:

        ; if (curpos != 0) checkleft
        cmp qword [curpos], 0
        je left_check_end

        ; check left
        mov rax, [leftpointer]
        mov rbx, [rax]
        cmp rbx, 1
        jne left_check_end
        inc qword [numaround]

left_check_end:

        ; check right (if next pos is not newline)
        mov rax, [rightpointer]
        mov rbx, [rax]
        cmp rbx, 2
        je right_check_end
        cmp rbx, 1
        jne right_check_end
        inc qword [numaround]

right_check_end:

        ; if (curnewline != numnewlines - 1) checkbot
        mov rax, [numnewlines]
        dec rax
        cmp [curnewline], rax
        jge bottom_check_end

        ; check bottomleft
        mov rax, [bottomleftpointer]
        mov rbx, [rax]
        cmp rbx, 1
        jne check_bottom
        inc qword [numaround]

check_bottom:
        ; check bottom
        mov rax, [bottompointer]
        mov rbx, [rax]
        cmp rbx, 1
        jne check_bottomright
        inc qword [numaround]

check_bottomright:
        ; check bottomright
        mov rax, [bottomrightpointer]
        mov rbx, [rax]
        cmp rbx, 1
        jne bottom_check_end
        inc qword [numaround]

bottom_check_end:

        ; if (numaround < 4) result++
        cmp qword [numaround], 4
        jge reset_numaround

        ; mark for removal by adding r9 to rollsmarkedforremovalpointer
        mov rax, [rollsmarkedforremovalpointer]
        mov rbx, [result]
        shl rbx, 3
        add rax, rbx
        mov qword [rax], r9

        inc qword [result]

reset_numaround:
        mov qword [numaround], 0

roll_check_end:

        ; if (r8 == 2) curnewline++; curpos = 0
        cmp r8, 2
        jne newline_check_end
        inc qword [curnewline]
        mov qword [curpos], 0
        jmp after_curpos_inc

newline_check_end:

        ;call _int_to_string
        ;mov r10, [conversionpointer]
        ;mov [printbuffpointer], r10
        ;mov r10, [conversionlen]
        ;mov [printbuffsize], r10
        ;call _print

        ;mov r10, newline
        ;mov [printbuffpointer], r10
        ;mov [printbuffsize], 1
        ;call _print

        inc qword [curpos]

after_curpos_inc:
        add r9, 8
        jmp iterate_diagram_start

iterate_diagram_end:

        ret

_delete_marked_rolls:

        ; init for loop
        mov r8, 0

iterate_removals_start:
        ; if r8 < result
        cmp r8, [result]
        jge iterate_removals_end

        ; get index to remove
        mov rax, [rollsmarkedforremovalpointer]
        mov rbx, r8
        shl rbx, 3
        add rax, rbx
        mov rbx, [rax]         ; rbx = index to remove

        ; set diagram at index to 0 (empty)
        mov rax, [diagrambuffpointer]
        add rax, rbx
        mov qword [rax], 0

        inc r8
        jmp iterate_removals_start

iterate_removals_end:

        ret
