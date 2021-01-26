//
// Created by nikita on 11/14/20.
//

#include <string>
#include <cassert>
#include "lexer.h"
#include "../include/parser_info.h"

std::string to_string(LexicalAnalyzer::Token t) {
    using T = LexicalAnalyzer::Token;
    std::map<LexicalAnalyzer::Token, std::string> tokens {
            {T::AND,      "&"},
            {T::OR,       "|"},
            {T::XOR,      "^"},
            {T::NOT,      "!"},
            {T::VAR,      "variable"},
            {T::LBRACKET, "("},
            {T::RBRACKET, ")"},
            {T::END,      "EOF"},
    };
    return tokens[t];
}

LexicalAnalyzer::LexicalAnalyzer(std::istream &in) : m_in(in), m_pos(0) { }

std::pair<TerminalGrammarSymbol, std::string> LexicalAnalyzer::next() {
    int c;
    do {
        c = m_in.get();
        m_pos++;
    } while (isspace(c));
    m_curChar = c;
    if (c == EOF) return terminal_EOF();
    if (isalpha(c)) return terminal_identifier(std::string(1, c));
    switch (c) {
        case '&':
            return terminal_and();
        case '|':
            return terminal_or();
        case '^':
            return terminal_xor();
        case '!':
            return terminal_not();
        case '(':
            return terminal_lBracket();
        case ')':
            return terminal_rBracket();
        default:
            assert(false);
    }
}
