//
// Created by nikita on 10/10/20.
//

#ifndef LAB2_LEXER_H
#define LAB2_LEXER_H

#include <vector>
#include <map>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <iostream>

class LexicalAnalyzer {
public:
    enum class Token {
        AND, OR, XOR, NOT, VAR, LBRACKET, RBRACKET, END
    };

    explicit LexicalAnalyzer(std::istream &in);

    [[nodiscard]] Token token() const { return m_curToken; }

    void nextToken() { m_curToken = getToken(); }

    std::string getTokenText() { return std::string(1, m_curChar); }

    [[nodiscard]] size_t pos() const { return m_pos; }

private:
    Token getToken();

    char m_curChar{};
    std::istream &m_in;
    size_t m_pos;
    Token m_curToken;
};

std::string to_string(LexicalAnalyzer::Token t);

#endif //LAB2_LEXER_H
