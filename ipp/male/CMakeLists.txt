cmake_minimum_required(VERSION 3.21)
project(ipp_male C)

set(CMAKE_C_STANDARD 99)

add_executable(ipp_male main.c array.c array.h input.c input.h errors.c errors.h bitvectors.c bitvectors.h bfs.c bfs.h queue.c queue.h types.h)
