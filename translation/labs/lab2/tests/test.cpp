//
// Created by nikita on 11/14/20.
//

#include <string>
#include <array>
#include <vector>
#include <iostream>
#include <fstream>
#include <iomanip>

#include "../include/parser.h"

std::unique_ptr<Tree> var(const std::string &name) {
    auto tree = std::make_unique<Tree>("V");
    tree->addChild(name);
    return tree;
}

std::unique_ptr<Tree> br(std::unique_ptr<Tree> arg) {
    auto tree = std::make_unique<Tree>("V");
    tree->addChild("(");
    tree->addChild(std::move(arg));
    tree->addChild(")");
    return tree;
}

std::unique_ptr<Tree> N(std::unique_ptr<Tree> arg) {
    std::unique_ptr<Tree> tree = std::make_unique<Tree>("N");
    if (arg->name() == "N")
        tree->addChild("!");
    tree->addChild(std::move(arg));
    return tree;
}

std::unique_ptr<Tree> N(size_t times, std::unique_ptr<Tree> arg) {
    std::unique_ptr child = std::move(arg);
    while (times--) {
        child = N(std::move(child));
    }
    return child;
}

std::unique_ptr<Tree> seq_helper(std::string const &name, std::string const &sign) {
    auto tree = std::make_unique<Tree>(name);
    tree->addChild("eps");
    return tree;
}

template<typename... T>
std::unique_ptr<Tree> seq_helper(std::string const &name, std::string const &sign, std::unique_ptr<Tree> f, T... args) {
    std::unique_ptr<Tree> tree = std::make_unique<Tree>(name);
    tree->addChild(sign);
    if (f->name() == "N")
        tree->addChild("!");
    tree->addChild(std::move(f));
    tree->addChild(seq_helper(name, sign, std::move(args)...));
    return tree;
}

std::unique_ptr<Tree> seq(std::string const &name, std::string const &sign) {
    auto tree = std::make_unique<Tree>(name);
    return tree;
}

template<typename... T>
std::unique_ptr<Tree> seq(std::string const &name, std::string const &sign, std::unique_ptr<Tree> f, T... args) {
    std::unique_ptr<Tree> tree = std::make_unique<Tree>(name);
    if (f->name() == "N")
        tree->addChild("!");
    tree->addChild(std::move(f));
    tree->addChild(seq_helper(name + "'", sign, std::move(args)...));
    return tree;
}

template<typename... T>
std::unique_ptr<Tree> X(T... args) {
    return seq("X", "^", std::move(args)...);
}

template<typename... T>
std::unique_ptr<Tree> O(T... args) {
    return seq("O", "|", std::move(args)...);
}

template<typename... T>
std::unique_ptr<Tree> A(T... args) {
    return seq("A", "&", std::move(args)...);
}

std::unique_ptr<Tree> path(std::string const &max, std::unique_ptr<Tree> arg) {
    if (max == "V" || max == "N") return arg;
    if (arg->name() == "V" || arg->name() == "N") {
        arg = seq("A", "&", std::move(arg));
    }
    if (max == "A") return arg;
    if (arg->name() == "A") {
        arg = seq("O", "O", std::move(arg));
    }
    if (max == "O") return arg;
    if (arg->name() == "O") {
        arg = seq("X", "^", std::move(arg));
    }
    return arg;
}

std::unique_ptr<Tree> path(std::unique_ptr<Tree> arg) {
    return path("X", std::move(arg));
}

bool eqTrees(const Tree *lhs, const Tree *rhs) {
    if (lhs->name() != rhs->name()) {
        return false;
    }
    if (lhs->nChildren() != rhs->nChildren()) {
        return false;
    }
    for (size_t i = 0; i < lhs->nChildren(); ++i) {
        if (!eqTrees(lhs->children(i), rhs->children(i))) {
            return false;
        }
    }
    return true;
}

const char *error_tests[] = {
        "(", ")",
        "&", "|", "^",
        "()", "(()", "())",
        "!", "!()", "!&", "!^", "!|", "!!!",
        // TODO
};

const char *lexer_tests = {
        // TODO
};


std::array<std::pair<const char *, std::unique_ptr<Tree>>, 28> parser_tests = {
        // N, V
        std::make_pair("a", path(var("a"))),
        std::make_pair("!a", path(N(var("a")))),
        std::make_pair("!!a", path(N(2, var("a")))),
        std::make_pair("!!!a", path(N(3, var("a")))),
        // a OP b
        std::make_pair("a ^ b", X(path("O", var("a")), path("O", var("b")))),
        std::make_pair("! a ^ !b  ", X(path("O", N(var("a"))), path("O", N(var("b"))))),
        std::make_pair("a | b", path(O(path("A", var("a")), path("A", var("b"))))),
        std::make_pair("a & b", path(A(var("a"), var("b")))),
        std::make_pair("a & ! b", path(A(var("a"), N(var("b"))))),
        std::make_pair("! a & ! b", path(A(N(var("a")), N(var("b"))))),
        // a OP b OP c
        std::make_pair("a ^ b ^ c", X(
                path("O", var("a")),
                path("O", var("b")),
                path("O", var("c")))),
        std::make_pair("a | b | c", path(O(
                path("A", var("a")),
                path("A", var("b")),
                path("A", var("c"))))),
        std::make_pair("! a & b &! c", path(A(
                N(var("a")),
                var("b"),
                N(var("c"))))),

        std::make_pair("! a | b &! c", path(O(
                path("A", N(var("a"))),
                A(
                        var("b"),
                        N(var("c")))))),

        std::make_pair("! a ^ b |! c & d", X(
                path("O", N(var("a"))), O(
                        path("A", var("b")), A(
                                N(var("c")),
                                var("d"))))),
        // priorities
        std::make_pair("a & b | c & d", path(O(
                A(var("a"), var("b")),
                A(var("c"), var("d"))
        ))),
        std::make_pair("a | b & c | d", path(O(
                A(var("a")),
                A(var("b"), var("c")),
                A(var("d"))
        ))),
        std::make_pair("a | b ^ c | d", X(
                O(A(var("a")), A(var("b"))),
                O(A(var("c")), A(var("d"))))),
        std::make_pair("a ^ b | c ^ d", X(
                path("O", var("a")),
                O(path("A", var("b")), path("A", var("c"))),
                path("O", var("d"))
        )),
        std::make_pair("a ^ b ^ c ^ d ^ e", X(
                path("O", var("a")),
                path("O", var("b")),
                path("O", var("c")),
                path("O", var("d")),
                path("O", var("e")))),
        std::make_pair("a | b | c | d | e", X(O(
                path("A", var("a")),
                path("A", var("b")),
                path("A", var("c")),
                path("A", var("d")),
                path("A", var("e"))))),
        std::make_pair("a & b & c & d & e", X(O(A(
                var("a"), var("b"), var("c"),
                var("d"), var("e"))))),
        // brackets
        std::make_pair("(a & b & c & d & e)", path(br(X(O(A(
                var("a"), var("b"), var("c"),
                var("d"), var("e"))))))),
        std::make_pair("(a)", path(br(path(var("a"))))),
        std::make_pair("(!a)", path(br(path(N(var("a")))))),
        std::make_pair("!(!a)", path(N(br(path(N(var("a"))))))),
        std::make_pair("(a | b )& (c | d)", path(A(
                br(X(O(A(var("a")), A(var("b"))))),
                br(X(O(A(var("c")), A(var("d")))))))),
        std::make_pair("a & (b | c) & d)", path(A(
                var("a"),
                br(X(O(A(var("b")), A(var("c"))))),
                var("d"))))
};

std::string dotName(std::string const &name) {
    std::string res;
    for (auto c : name) {
        if (c == '!')
            res += 'I';
        else if (c == '(')
            res += 'L';
        else if (c == ')')
            res += 'R';
        else if (c == '^')
            res += "x";
        else if (c == '&')
            res += 'a';
        else if (c == '|')
            res += 'o';
        else if (isspace(c)) {
            continue;
        } else {
            res += c;
        }
    }
    return res + ".dot";
}

int main() {
    size_t id = 0;
    for (std::pair<const char *, std::unique_ptr<Tree>> const &testSet : parser_tests) {
        std::istringstream in(testSet.first);
        std::unique_ptr<Tree> actual = parse(in);
        if (!eqTrees(actual.get(), testSet.second.get())) {
            std::cout << "[ ]";
        } else {
            std::cout << "[x]";
        }
        std::cout << " " << std::left << std::setw(30) << std::string("\"") + testSet.first + "\"";
        std::string filename = dotName(testSet.first);
        std::cout << filename << std::endl;
        std::ofstream out(filename);
        out << "digraph G { \n"
               "labelloc=\"t\";\n"
               "label=\"" + std::string(testSet.first) + "\";\n";
        actual->dot(out);
        testSet.second->dot(out);
        out << "}\n";
    }

    for (const char* testSet : error_tests) {
        try {
            std::istringstream in(testSet);
            std::cout << testSet << std::endl;
            parse(in);
            std::cout << "failed" << std::endl;
        } catch (ParsingError &e) {
            std::cout << "passed" << std::endl;
        }
    }
    return 0;
}
