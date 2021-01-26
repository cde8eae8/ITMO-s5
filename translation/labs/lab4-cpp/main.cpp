#include <iostream>
#include <any>
#include <vector>
#include <sstream>
#include "parsing_table_generator.h"
#include "parser.h"

std::unordered_map<std::string, std::vector<std::vector<std::string>>> testGrammar = {
        std::make_pair("S", std::vector<std::vector<std::string>>{{"C", "C"}}),
        std::make_pair("C", std::vector<std::vector<std::string>>{{"c", "C"},
                                                                  {"d"}}),
};

std::unordered_map<std::string, std::vector<std::vector<std::string>>> testGrammar2 = {
        std::make_pair("S'", std::vector<std::vector<std::string>>{{"A"}}),

        std::make_pair("A", std::vector<std::vector<std::string>>{{"C", "C"},
                                                                  {"B", "b", "n"},
                                                                  {"A"},
                                                                  {"B", "B", "B", "t"},
                                                                  {"B", "B", "B", "H"}}),
        std::make_pair("B", std::vector<std::vector<std::string>>{{"C", "d"},
                                                                  {"b", "bbb"},
                                                                  {"B", "f"},
                                                                  {"A"},
                                                                  {}}),
        std::make_pair("C", std::vector<std::vector<std::string>>{{"c", "C"},
                                                                  {"d"}}),
        std::make_pair("H", std::vector<std::vector<std::string>>{{"h"}}),
        std::make_pair("T", std::vector<std::vector<std::string>>{{"B", "B", "B", "B", "B"},
                                                                  {"g"}})
};

std::unordered_map<std::string, std::vector<std::vector<std::string>>> testGrammar3 = {
        std::make_pair("S'", std::vector<std::vector<std::string>>{{"S"}}),
        std::make_pair("S", std::vector<std::vector<std::string>>{{"C", "C"}}),
        std::make_pair("C", std::vector<std::vector<std::string>>{{"c", "C"},
                                                                  {"d"}}),
};

//struct Lexer {
//    Lexer() {
//        s = "cdcccccccccd";
//        pos = -1;
//    }
//
//    TerminalGrammarSymbol next() {
//        pos++;
//        if (pos >= s.size()) return terminal_EOF();
////        if (s[pos] == 'c') return terminal_c();
////        if (s[pos] == 'd') return terminal_d();
//    }
//
//private:
//    std::string s;
//    size_t pos;
//};

#include "gram.hpp"
#include <memory>

std::map<std::string, std::string> identifiers;
std::unordered_map<std::string, std::vector<RawRule>> rawGrammar;
std::string startNonterminal;
std::string header;

std::ostream &operator<<(std::ostream &out, RawRule const &rule) {
    out << rule.body << " " << rule.code << std::endl;
    return out;
}

int main(int argc, char** argv) {
    if (argc == 1) {
        std::cout << "args" << std::endl;
        return 1;
    }
    yy::parser parseInput;
    parseInput ();
    rawGrammar.insert(std::make_pair("S'", std::vector<RawRule>{RawRule{{startNonterminal}, {"return _1;"}}}));
    for (auto const& p : rawGrammar) {
        std::cout << p.first << " : " << p.second << std::endl;
    }
    identifiers.insert(std::make_pair("S'", identifiers[startNonterminal]));
    convertGrammar(argv[1], header, rawGrammar, identifiers);
    return 0;
}
