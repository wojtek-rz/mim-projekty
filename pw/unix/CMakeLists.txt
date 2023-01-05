cmake_minimum_required(VERSION 3.1)
project(MIMUW-FORK C)

set(CMAKE_CXX_STANDARD "17")
set(CMAKE_C_STANDARD "11")
set(CMAKE_C_FLAGS "-g -Wall -Wextra -Wno-sign-compare")

function(add_example_program target_name)
  add_executable(${target_name} "${target_name}.c")
  target_link_libraries(${target_name} err)
  target_link_libraries(${target_name} pthread)
  target_link_libraries(${target_name} utils)
  target_link_libraries(${target_name} pipeline-utils)
endfunction()

add_library(err err.c)

add_library(utils utils.c)
target_link_libraries(utils err)

add_library(pipeline-utils pipeline-utils.c)
target_link_libraries(pipeline-utils err)

add_example_program(executor)

add_executable(01_out tests/simple1/01_out.c)
#add_executable(02_err tests/02_err.c)
#add_executable(03_out_err tests/03_out_err.c)
add_executable(04_kill tests/simple1/04_kill.c)
#add_executable(t1 tests/t1.c)
add_executable(mikolaj tests/mikolaj.c)