cmake_minimum_required(VERSION 3.15)
project(lab2)

set(CMAKE_CXX_STANDARD 17)

add_executable(lab2
        main.cpp
        src/lexer.cpp
        src/tree.cpp
        include/parser_info.cpp
        include/parser.cpp
        include/project_rule_structures.cpp
        )

