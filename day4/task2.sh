#!/bin/bash
nasm -f elf64 task2.asm
ld task2.o -o task2
rm task2.o
./task2
