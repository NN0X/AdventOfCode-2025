#!/bin/bash
nasm -f elf64 task1.asm
ld task1.o -o task1
rm task1.o
./task1
