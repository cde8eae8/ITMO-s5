//
// Created by nikita on 11/14/20.
//

#include <string>
#include <fstream>
#include <sstream>

#include "include/tree.h"
#include "include/parser.h"
#include "include/parser_info.h"
#include "src/lexer.h"

int main(int argc, char **argv) {
    if (argc == 1) {
        std::cout << "args" << std::endl;
        return 1;
    }
    std::string s;
    std::getline(std::cin, s);
    std::istringstream in(s);
    Tree* tree;
    try {
        auto lexer = LexicalAnalyzer(in);
        tree = std::any_cast<Tree*>(parse(start, *grammar(), lexer, rules, makeAction));
    } catch (std::exception &error) {
        std::cerr << error.what() << std::endl;
        return 0;
    }
    std::ofstream out (argv[1]);
    out << "digraph G { \n"
                 "labelloc=\"t\";\n"
                 "label=\"" + s + "\";";
    tree->dot(out);
    out << "}" << std::endl;
    return 0;
}
