//
// Created by nikita on 1/20/21.
//

#ifndef LAB4_CPP_PARSING_RULE_STRUCTURES_H
#define LAB4_CPP_PARSING_RULE_STRUCTURES_H

#include <variant>
#include <ostream>
#include <vector>
#include <unordered_set>
#include <unordered_map>

// helper type for the visitor #4
template<class... Ts>
struct overloaded : Ts ... {
    using Ts::operator()...;
};
// explicit deduction guide (not needed as of C++20)
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

struct RawRule {
    std::vector<std::string> body;
    std::string code;
};

struct NonterminalDescription;
struct UserDefinedTerminalGrammarSymbol;
//struct TerminalGrammarSymbol;
struct NonterminalGrammarSymbol;
struct Rule;

struct Epsilon {
    std::string name() const {
        return "@eps";
    }
};

struct EndOfText {
    std::string name() const {
        return "@$";
    }
};


using TerminalGrammarSymbol = std::variant<UserDefinedTerminalGrammarSymbol, Epsilon, EndOfText>;
using GrammarSymbol = std::variant<TerminalGrammarSymbol, NonterminalGrammarSymbol>;


struct NonterminalGrammarSymbol {
public:
    explicit NonterminalGrammarSymbol(const NonterminalDescription *descr) : m_description(descr) {}

    [[nodiscard]] std::string name() const;

    [[nodiscard]] std::vector<Rule> const &rules() const;

    [[nodiscard]] std::unordered_set<TerminalGrammarSymbol> const &firstSet() const;

    [[nodiscard]] NonterminalDescription const *internal() const {
        return m_description;
    }

private:
    const NonterminalDescription *m_description;
};

struct UserDefinedTerminalGrammarSymbol {
public:
    explicit UserDefinedTerminalGrammarSymbol(std::string name) : m_name(std::move(name)) {}

    [[nodiscard]] std::string name() const {
        return m_name;
    }

private:
    std::string m_name;
};

namespace std {
    template<>
    struct hash<NonterminalGrammarSymbol> {
        std::size_t operator()(NonterminalGrammarSymbol const &nonterminal) const noexcept {
            return std::hash<std::string>{}(nonterminal.name());
        }
    };
}

namespace std {
    template<>
    struct hash<TerminalGrammarSymbol> {
        std::size_t operator()(TerminalGrammarSymbol const &terminal) const noexcept {
            std::string name = std::visit([](auto const &t) { return t.name(); }, terminal);
            return std::hash<std::string>{}(name);
        }
    };
}

struct ParserRule {
    std::string head;
    size_t length;
};

struct Rule {
public:
    NonterminalDescription* description;
    size_t id;
    std::vector<GrammarSymbol> body;
    std::string code;
};

struct NonterminalDescription {
public:
    size_t id;
    const std::string name;
    std::vector<Rule> rules;
    std::unordered_set<TerminalGrammarSymbol> firstSet;
};

struct Accept {
};

struct Error {
};

struct Shift {
    size_t stateId;
};

struct Reduce {
    const Rule* rule;
};

using Action = std::variant<Shift, Reduce, Accept, Error>;

struct ParsingTable {
    std::vector<std::unordered_map<TerminalGrammarSymbol, Action>> actions;
    std::vector<std::unordered_map<NonterminalGrammarSymbol, size_t>> gotoActions;
};

std::ostream &operator<<(std::ostream &out, const Reduce &kernel);

std::ostream &operator<<(std::ostream &out, const Shift &kernel);

std::ostream &operator<<(std::ostream &out, const Accept &kernel);

std::ostream &operator<<(std::ostream &out, const Error &kernel);

std::ostream &operator<<(std::ostream &out, const Action &kernel);

std::ostream &operator<<(std::ostream &out, NonterminalDescription const &nonterminal);

std::ostream &operator<<(std::ostream &out, Rule const &rule);

std::ostream &operator<<(std::ostream &out, NonterminalGrammarSymbol const &nonterminal);

std::ostream &operator<<(std::ostream &out, TerminalGrammarSymbol const &terminal);

std::ostream &operator<<(std::ostream &out, GrammarSymbol const &sym);

bool operator==(NonterminalGrammarSymbol const &lhs, NonterminalGrammarSymbol const &rhs);

bool operator==(TerminalGrammarSymbol const &lhs, TerminalGrammarSymbol const &rhs);

#endif //LAB4_CPP_PARSING_RULE_STRUCTURES_H
