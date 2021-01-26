//
// Created by nikita on 1/26/21.
//

#include "parsing_rule_structures.h"
//#include "parsing_table_generator.h"
#include <ostream>
#include "debug/debug.h"

std::ostream &operator<<(std::ostream &out, const Reduce &kernel) {
//    out << "Reduce{" << kernel.rule->id << "@" << kernel.rule->body << "}";
    return out;
}

std::ostream &operator<<(std::ostream &out, const Shift &kernel) {
    out << "Shift{" << kernel.stateId << "}";
    return out;
}

std::ostream &operator<<(std::ostream &out, const Accept &kernel) {
    out << "Accept{}";
    return out;
}

std::ostream &operator<<(std::ostream &out, const Error &kernel) {
    out << "Error{}";
    return out;
}

std::ostream &operator<<(std::ostream &out, const Action &kernel) {
    std::visit([&](auto& v) { out << v; }, kernel);
    return out;
}

bool operator==(NonterminalGrammarSymbol const &lhs, NonterminalGrammarSymbol const &rhs) {
    return lhs.name() == rhs.name();
}

bool operator==(TerminalGrammarSymbol const &lhs, TerminalGrammarSymbol const &rhs) {
    std::string lname = std::visit([](auto const &t) { return t.name(); }, lhs);
    std::string rname = std::visit([](auto const &t) { return t.name(); }, rhs);
    return lname == rname;
}

std::string NonterminalGrammarSymbol::name() const {
    return m_description->name;
}

std::vector<Rule> const &NonterminalGrammarSymbol::rules() const {
    return m_description->rules;
}

std::unordered_set<TerminalGrammarSymbol> const &NonterminalGrammarSymbol::firstSet() const {
    return m_description->firstSet;
}

