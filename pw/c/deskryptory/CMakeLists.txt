cmake_minimum_required(VERSION 3.1)
project(MIMUW-FORK C)

set(CMAKE_CXX_STANDARD "17")
set(CMAKE_C_STANDARD "11")
set(CMAKE_C_FLAGS "-g -Wall -Wextra -Wno-sign-compare")

function(add_example_program target_name)
  add_executable(${target_name} "${target_name}.c")
  target_link_libraries(${target_name} err)
endfunction()

add_library(err err.c)
add_example_program(child-pipe)
add_example_program(parent-pipe)
add_example_program(parent-dup)
add_example_program(child-fifo)
add_example_program(parent-fifo)

add_library(pipeline-utils pipeline-utils.c)
target_link_libraries(pipeline-utils err)

add_example_program(pipeline)
target_link_libraries(pipeline pipeline-utils)

add_example_program(pipeline-tester)
target_link_libraries(pipeline-tester pipeline-utils)
