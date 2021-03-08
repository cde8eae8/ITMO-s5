#ifndef __PARSER_H__
#define __PARSER_H__

#include <any>
#include <variant>
#include <functional>
#include <string>
#include <vector>
#include <stack>
#include <optional>
#include <unordered_map>
#include <iostream>
#include "parsing_rule_structures.h"
#include "debug/debug.h"

struct ParserAccept {
};

struct ParserError {
};

struct ParserShift {
    size_t stateId;
};

struct ParserReduce {
//    size_t length;
    size_t rule;
};

struct ParserState {
    ParserState(size_t i) : stateId(i) {}

    size_t stateId;
    std::any attribute;
};

std::ostream &operator<<(std::ostream &out, ParserState const &state);

using ParserAction = std::variant<ParserShift, ParserReduce, ParserAccept, ParserError>;

struct SimpleParsingTable {
    std::vector<std::unordered_map<TerminalGrammarSymbol, ParserAction>> actions;
    std::vector<std::unordered_map<std::string, ParserState>> gotoActions;
};

template <typename Lexer>
std::any parse(size_t startingState, SimpleParsingTable const &parserTable, Lexer &lexer, std::vector<ParserRule> const& rules,
               std::function<std::any(size_t, std::vector<std::any> const&)> makeActions) {
    /* TODO: check eof */
    std::vector<ParserState> workingStack{};
    workingStack.emplace_back(startingState);
    std::pair<TerminalGrammarSymbol, std::string> currentInputSymbol = lexer.next();
    std::optional<ParserError> maybeError{};
    bool finished = false;
    while (!finished) {
//        std::cout << workingStack << " | " << currentInputSymbol << std::endl;
        auto const &top = workingStack.back();
        if (parserTable.actions[top.stateId].find(currentInputSymbol.first) == parserTable.actions[top.stateId].end()) {
            auto name = [](TerminalGrammarSymbol const& v) -> std::string {
                return std::visit([](auto const& t) { return t.name(); }, v);
            };

            std::string message = "bad expression: expected ";
            for (auto &p : parserTable.actions[top.stateId]) {
                message += name(p.first) + ", ";
            }
            message += "found " + name(currentInputSymbol.first);
            throw std::runtime_error(message);
        }
        ParserAction action = parserTable.actions[top.stateId].at(currentInputSymbol.first);
        std::visit(overloaded{
                [&](ParserShift const &shift) {
                    std::cout << "shift " << shift.stateId << std::endl;
                    workingStack.emplace_back(shift.stateId);
                    workingStack.back().attribute = currentInputSymbol.second;
                    currentInputSymbol = lexer.next();
//                    std::cout << "input:" << currentInputSymbol << " " << workingStack.back().attribute.type().name() << std::endl;
                },
                [&](ParserReduce const &reduce) {
                    std::cout << "reduce " << reduce.rule << std::endl;
                    ParserRule const& rule = rules[reduce.rule];
                    std::vector<std::any> arguments;
                    for (size_t i = 0; i < rule.length; ++i) {
                        arguments.emplace_back(workingStack.back().attribute);
                        workingStack.pop_back();
                    }
                    /* TODO: some actions */
                    std::reverse(arguments.begin(), arguments.end());
                    auto newState = parserTable.gotoActions[workingStack.back().stateId].at(rule.head);
                    std::cout << newState.attribute.type().name() << std::endl;
                    newState.attribute = makeActions(reduce.rule, arguments);
                    workingStack.push_back(newState);
                },
                [&](ParserError const &error) {
                    maybeError = error;
                },
                [&](ParserAccept const &accept) {
                    finished = true;
                },
        }, action);
        if (maybeError.has_value()) {
            return maybeError.value();
        }
        if (finished) {
//            return /*TODO:...*/ std::get<Accept>(action);
            return workingStack.back().attribute;
        }
    }
}

#endif
