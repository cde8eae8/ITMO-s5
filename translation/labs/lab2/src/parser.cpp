#include <iostream>
#include <utility>
#include <memory>
#include <algorithm>
#include "../include/parser.h"
#include "lexer.h"

/**
 * X -> X ^ O
 * X -> O
 * O -> O | A
 * O -> A
 * A -> A & ! N
 * A -> A & V
 * A -> ! N
 * A -> V
 * N -> ! N
 * N -> V
 * V -> Var
 * V -> ( X )
 */

/**
 * X -> O X'
 * X' -> ^ O X'
 * X' -> eps
 * O -> A O'
 * O' -> | A O'
 * O' -> eps
 * A -> ! N A'
 * A -> V A'
 * A' -> & V
 * A' -> & ! N
 * A' -> eps
 * N -> ! N
 * N -> V
 * V -> Var
 * V -> ( X )
 */

/*
+----+---------+---------+
|    | FIRST   | FOLLOW  |
+----+---------+---------+
| X  | var ( ! | $       |
+----+---------+---------+
| X' | ^ eps   | $       |
+----+---------+---------+
| O  | var ( ! | $ ^     |
+----+---------+---------+
| O' | | eps   | $ ^     |
+----+---------+---------+
| A  | var ( ! | $ | ^   |
+----+---------+---------+
| A' | & eps   | $ | ^   |
+----+---------+---------+
| N  | var ( ! | $ ^ | $ |
+----+---------+---------+
| V  | var (   | $ ^ | $ |
+----+---------+---------+
 */

#define STRICT

using T = LexicalAnalyzer::Token;

bool contains(LexicalAnalyzer::Token t, std::initializer_list<LexicalAnalyzer::Token> tokens);
void badToken(size_t pos, T token);
void expectToken(LexicalAnalyzer const &lexer, LexicalAnalyzer::Token expected);
std::unique_ptr<Tree> X(LexicalAnalyzer &lexer);
std::unique_ptr<Tree> O(LexicalAnalyzer &lexer);
std::unique_ptr<Tree> A(LexicalAnalyzer &lexer);
std::unique_ptr<Tree> N(LexicalAnalyzer &lexer);
std::unique_ptr<Tree> V(LexicalAnalyzer &lexer);

std::unique_ptr<Tree> parse(std::istream &in) {
    LexicalAnalyzer lex(in);
    auto tree = X(lex);
    return tree;
}

bool contains(LexicalAnalyzer::Token t, std::initializer_list<LexicalAnalyzer::Token> tokens) {
    return std::find(tokens.begin(), tokens.end(), t) != tokens.end();
}

void badToken(size_t pos, T token) {
    throw ParsingError("Unexpected token " + to_string(token), pos);
}

void expectToken(LexicalAnalyzer const &lexer, LexicalAnalyzer::Token expected) {
    LexicalAnalyzer::Token actual = lexer.token();
    if (actual != expected) {
        throw ParsingError("Expected " + to_string(expected) + ", found " + to_string(actual), lexer.pos());
    }
}

template<LexicalAnalyzer::Token Sign, char GramNodeName>
std::unique_ptr<Tree> seq(LexicalAnalyzer &lexer, void (*childParser)(Tree*, LexicalAnalyzer &)) {
    LexicalAnalyzer::Token token = lexer.token();
    std::string nodeName = std::string(1, GramNodeName);
    std::unique_ptr<Tree> tree = std::make_unique<Tree>(nodeName);
#ifdef STRICT
    if (contains(token, {T::VAR, T::NOT, T::LBRACKET})) {
        childParser(tree.get(), lexer);
        Tree *child = tree.get();
        while (lexer.token() == Sign) {
            child = child->addChild(nodeName + "'");
            lexer.nextToken();
            child->addChild(to_string(Sign));
            childParser(child, lexer);
        }
        child->addChild(nodeName + "'")->addChild("eps");
    } else {
        badToken(lexer.pos(), token);
    }
#else
    if (contains(token, {T::VAR, T::NOT, T::LBRACKET})) {
        std::unique_ptr<Tree> child = childParser(lexer);
        if (lexer.token() != Sign) {
            return child;
        }
        tree->addChild(std::move(child));
        while (lexer.token() == Sign) {
            lexer.nextToken();
            tree->addChild(to_string(Sign));
            tree->addChild(childParser(lexer));
        }
    } else {
        badToken(lexer.pos(), token);
    }
#endif
    return tree;
}

std::unique_ptr<Tree> X(LexicalAnalyzer &lexer) {
    return seq<T::XOR, 'X'>(lexer, [](Tree* tree, LexicalAnalyzer &lexer) {
        tree->addChild(O(lexer));
    });
}

std::unique_ptr<Tree> O(LexicalAnalyzer &lexer) {
    return seq<T::OR, 'O'>(lexer, [](Tree* tree, LexicalAnalyzer &lexer) {
        tree->addChild(A(lexer));
    });
}

std::unique_ptr<Tree> A(LexicalAnalyzer &lexer) {
    return seq<T::AND, 'A'>(lexer, [](Tree* tree, LexicalAnalyzer &lexer) {
        if (lexer.token() == T::NOT) {
            lexer.nextToken();
            tree->addChild("!");
            tree->addChild(N(lexer));
        } else if (contains(lexer.token(), {T::VAR, T::LBRACKET})) {
            tree->addChild(V(lexer));
        } else {
            badToken(lexer.pos(), lexer.token());
        }
    });
}

std::unique_ptr<Tree> N(LexicalAnalyzer &lexer) {
    LexicalAnalyzer::Token token = lexer.token();
    std::unique_ptr<Tree> tree = std::make_unique<Tree>("N");
    if (token == LexicalAnalyzer::Token::NOT) {
        lexer.nextToken();
        tree->addChild("!");
        tree->addChild(N(lexer));
    } else if (contains(token, {T::VAR, T::LBRACKET})) {
        tree->addChild(V(lexer));
    } else {
        badToken(lexer.pos(), token);
    }
    return tree;
}

std::unique_ptr<Tree> V(LexicalAnalyzer &lexer) {
    LexicalAnalyzer::Token token = lexer.token();
    std::unique_ptr<Tree> tree = std::make_unique<Tree>("V");
    if (token == LexicalAnalyzer::Token::LBRACKET) {
        lexer.nextToken();
        tree->addChild("(");
        tree->addChild(X(lexer));
        expectToken(lexer, LexicalAnalyzer::Token::RBRACKET);
        lexer.nextToken();
        tree->addChild(")");
    } else if (token == LexicalAnalyzer::Token::VAR) {
        tree->addChild(lexer.getTokenText());
        lexer.nextToken();
    } else {
        badToken(lexer.pos(), token);
    }
    return tree;
}
