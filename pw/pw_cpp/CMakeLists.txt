cmake_minimum_required(VERSION 3.14)
project(pw_cpp)

set(CMAKE_CXX_STANDARD  20)
set(CMAKE_CXX_FLAGS "-g -Wall")
# set(CMAKE_CXX_CLANG_TIDY clang-tidy)

set(THREADS_PREFER_PTHREAD_FLAG TRUE)
find_package(Threads REQUIRED)

add_library(system system.hpp system.cpp my_tests.cpp)

function(add_example_program target_name)
    add_executable(${target_name} "${target_name}.cpp")
    target_link_libraries(${target_name} Threads::Threads)
    target_link_libraries(${target_name} system)

endfunction()


add_example_program(demo)
add_example_program(my_tests)