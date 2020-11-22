//
// Created by nikita on 11/14/20.
//

#include <string>
#include "lexer.h"
#include "../include/parser.h"

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

LexicalAnalyzer::LexicalAnalyzer(std::istream &in) : m_in(in), m_pos(0) {
    nextToken();
}

LexicalAnalyzer::Token LexicalAnalyzer::getToken() {
    int c;
    do {
        c = m_in.get();
        m_pos++;
    } while (isspace(c));
    m_curChar = c;
    if (c == EOF) return Token::END;
    if (isalpha(c)) return Token::VAR;
    switch (c) {
        case '&':
            return Token::AND;
        case '|':
            return Token::OR;
        case '^':
            return Token::XOR;
        case '!':
            return Token::NOT;
        case '(':
            return Token::LBRACKET;
        case ')':
            return Token::RBRACKET;
        default:
            throw ParsingError("Lexer: unexpected char " + std::to_string(c), m_pos);
    }
}
