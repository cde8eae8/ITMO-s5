//
// Created by nikita on 11/14/20.
//

#include <string>
#include <fstream>
#include <sstream>

#include "include/parser.h"

int main() {
    std::string s;
    std::getline(std::cin, s);
    std::istringstream in(s);
    std::unique_ptr<Tree> tree;
    try {
        tree = parse(in);
    } catch (ParsingError &error) {
        std::cerr << error.what() << std::endl;
        return 0;
    }
    std::cout << "digraph G { \n"
           "labelloc=\"t\";\n"
           "label=\"" + s + "\";";
    tree->dot(std::cout);
    std::cout << "}";
    return 0;
}
