//
// Created by nikita on 1/26/21.
//

#include "../test.h"

#include <iostream>
#include <any>
#include <sstream>
#include "../parsing_table_generator.h"
#include "../parser.h"

struct CalcLexer {
    explicit CalcLexer(std::string const& ss) : s{ss} { }

    auto next() {
        int c = s.get();
        while (isspace(c)) {
            c = s.get();
        }
        if (c == EOF) return terminal_EOF();

        if (c == '(') return terminal_lBracket();
        if (c == ')') return terminal_rBracket();
        if (c == '*') {
            c = s.get();
            if (c == '*') return terminal_power();
            s.unget();
            return terminal_mul();
        }
        if (c == '+') return terminal_plus();
        if (c == '-') return terminal_minus();
        if (c == '/') return terminal_div();
        if (isdigit(c)) {
            std::string name;
            while (isalpha(c) || isdigit(c)) {
                name += c;
                c = s.get();
            }
            s.unget();
            return terminal_identifier(name);
        }
        assert(false);
//        if (pos >= s.size()) return terminal_EOF();
//        if (s[pos] == 'c') return terminal_c();
//        if (s[pos] == 'd') return terminal_d();
    }

private:
    std::istringstream s;
};

int main(int argc, char** argv) {
    if (argc == 1) {
        std::cout << "expected expression" << std::endl;
        return 1;
    }
    CalcLexer lex{argv[1]};
    int res = std::any_cast<int>(parse(start, *grammar(), lex, rules, makeAction));
    std::cout << argv[1] << " = " << res << std::endl;
}
