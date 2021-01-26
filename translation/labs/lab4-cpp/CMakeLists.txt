cmake_minimum_required(VERSION 3.17)
project(lab4_cpp)

set(CMAKE_CXX_STANDARD 20)
add_compile_options(-Wall -D_GLIBCXX_DEBUG)


find_package(BISON)
BISON_TARGET(exprgram gram.y ${CMAKE_CURRENT_SOURCE_DIR}/gram.cpp)

add_executable(lab4_cpp main.cpp parsing_table_generator.cpp parser.cpp project_rule_structures.cpp "${BISON_exprgram_OUTPUTS}")
#add_executable(test testparser/main.cpp parsing_table_generator.cpp parser.cpp project_rule_structures.cpp test.cpp)
#add_executable(test testparser/main.cpp test.cpp parsing_table_generator.h project_rule_structures.cpp parser.cpp)
