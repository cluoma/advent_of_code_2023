cmake_minimum_required(VERSION 3.27)
project(advent_of_code_2023)

set(CMAKE_CXX_STANDARD 23)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(day1_x main.cpp)

add_executable(day2_x day2.cpp)

add_executable(day3_x day3.cpp)

add_executable(day4_x day4.cpp)

add_executable(day5_x day5/day5.cpp)
