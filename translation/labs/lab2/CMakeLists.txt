cmake_minimum_required(VERSION 3.15)
project(lab2)

set(CMAKE_CXX_STANDARD 17)

add_executable(lab2
        main.cpp
        src/lexer.cpp
        src/parser.cpp
        src/tree.cpp)

add_executable(lab2-test
        tests/test.cpp
        src/lexer.cpp
        src/parser.cpp
        src/tree.cpp)
