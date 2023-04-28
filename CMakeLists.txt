cmake_minimum_required(VERSION 3.25)
project(asm6502 C)

set(CMAKE_C_STANDARD 90)

add_executable(asm6502.c itbl.c asm6502.c)
