//
// Created by nikita on 1/20/21.
//

#ifndef LAB4_CPP_GENERATOR_H
#define LAB4_CPP_GENERATOR_H

#include <string>
#include <fstream>
#include <vector>

#include "parsing_rule_structures.h"

/* просто запиши в плюсовый файл две таблицы в виде статического объекта и создай класс, который юзает код парсера
 * и вызывает его от этих табличек
 *
 * Действия над атрибутами:
 *  нагенерить функцию, которая реализует действия через касты
 *  передавать ее как темплейт в вызов парсинга, там сами разберутся
 */

std::ostream &writeConstructor(std::ostream &out, const TerminalGrammarSymbol &terminal) {
//    std::string name = std::visit([](auto const &t) { return t.name(); }, terminal);
    std::string name = std::visit(overloaded{
            [](UserDefinedTerminalGrammarSymbol const &t) {
                return "UserDefinedTerminalGrammarSymbol{\"" + t.name() + "\"}";
            },
            [](EndOfText const &t) { return std::string("EndOfText{}"); },
            [](Epsilon const &t) { return std::string("$@"); },
    }, terminal);
    out << name;
    return out;
}

std::ostream &writeConstructor(std::ostream &out, const NonterminalGrammarSymbol &nonterminal) {
    out << "NonterminalGrammarSymbol{&descriptions[" << nonterminal.internal()->id << "]}" << std::endl;
    return out;
}

std::ostream &writeConstructor(std::ostream &out, const Action &action) {
    std::visit(overloaded{
            [&](Reduce const &t) {
                out << "ParserReduce{" << t.rule->id << "}";
            },
            [&](Shift const &t) { out << "ParserShift{" << t.stateId << "}"; },
            [&](Accept const &t) { out << "ParserAccept{}"; },
            [&](Error const &t) { out << "ParserError{}"; }
    }, action);
    return out;
}


std::ostream &writeConstructor(std::ostream &out, GrammarSymbol const &symbol) {
    std::visit(overloaded{
            [&](TerminalGrammarSymbol const &t) { writeConstructor(out, t); },
            [&](NonterminalGrammarSymbol const &t) { writeConstructor(out, t); },
    }, symbol);
    return out;
}

const char *cppHead =
        "\n"
        "ParsingTable *grammar() {\n"
        "       static SimpleParsingTable {\n";

const char *actionsType = "std::vector<std::unordered_map<TerminalGrammarSymbol, ParserAction>>";
const char *gotoType = "std::vector<std::unordered_map<std::string, ParserState>>";

void generateCode(std::string const &filename, std::string const& head, ParsingTable const &table,
                  std::unordered_map<std::string, NonterminalDescription> const &descriptions,
                  size_t startIdx,
                  std::map<std::string, std::string> const &types) {

    std::map<size_t, const Rule *> rulesIds;
    for (auto const &descr : descriptions) {
        for (Rule const &rule : descr.second.rules) {
            rulesIds.insert(std::make_pair(rule.id, &rule));
        }
    }

    std::ofstream source(filename + ".cpp");
    std::cout << "writing to " << filename << std::endl;
    source <<
           "#include <unordered_map>\n"
           "#include <vector>\n"
           "#include <string>\n"
           "#include \"parsing_rule_structures.h\"\n"
           "#include \"parser.h\"\n"
           << head << "\n";
//           "#include \"test.h\"\n";


    std::ofstream header(filename + ".h");
    header <<
           "#ifndef __PARSER_HEADER__\n"
           "#define __PARSER_HEADER__\n";
    header << head <<
           "#include \"parsing_rule_structures.h\"\n"
           "#include <any>\n"
           "struct SimpleParsingTable;\n"
           "SimpleParsingTable* grammar();\n"
           "const size_t start = " << startIdx << ";\n"
                                                  "const std::vector<ParserRule> rules {\n";
    for (auto &p : rulesIds) {
        header << "\tParserRule{\"" << p.second->description->name << "\", " << p.second->body.size();
        header << "/*" << p.second->id << "*/";
        header << "},\n";
    }
    header << "};\n";

    header << "std::any makeAction(size_t i, std::vector<std::any> const& args);\n";
    source << "std::any makeAction(size_t i, std::vector<std::any> const& args) {\n";
    for (auto &p : rulesIds) {
        source << "\tif(i == " << p.first << ") {\n";
        source << "\t/* " << p.second->description->name << " -> " << p.second->body << " */\n";
        source << "\tauto f = [](";
        for (size_t i = 0; i < p.second->body.size(); ++i) {
            std::string type = std::visit(overloaded{
                    [&](NonterminalGrammarSymbol const &n) { return types.at(n.name()); },
                    [](TerminalGrammarSymbol const &t) { return std::string("std::string const&"); }
            }, p.second->body[i]);
            source << type << " _" << i + 1;
            if (i != p.second->body.size() - 1) {
                source << ", ";
            }
        }
        source << ") -> " << types.at(p.second->description->name) << "{\n";
        source << "\t\t" << p.second->code << "\n";
        source << "\t};\n"; // lambda end
        source << "\treturn f(";
        for (size_t i = 0; i < p.second->body.size(); ++i) {
            std::string type = std::visit(overloaded{
                    [&](NonterminalGrammarSymbol const &n) { return types.at(n.name()); },
                    [](TerminalGrammarSymbol const &t) { return std::string("std::string"); }
            }, p.second->body[i]);
            source << "std::any_cast<" << type << ">(args[" << i << "])";
            if (i != p.second->body.size() - 1) {
                source << ", ";
            }
        }
        source << ");\n";
        source << "\t}\n";
    }
    source << "}\n";


    header << "inline std::pair<TerminalGrammarSymbol, std::string> terminal_EOF() {\n"
              "return " << "std::make_pair(EndOfText{}, \"$\"); " << ";\n" <<
           "}\n";

    for (auto &p : rulesIds) {
        for (GrammarSymbol const &sym : p.second->body) {
            if (std::holds_alternative<TerminalGrammarSymbol>(sym)) {
                std::visit(overloaded{
                        [&](EndOfText const &t) {
                            assert(false);
                        },
                        [&](UserDefinedTerminalGrammarSymbol const &t) {
                            header << "inline std::pair<TerminalGrammarSymbol, std::string> terminal_" << t.name()
                                   << "(std::string const& value = \"\") {\n" <<
                                   "return std::make_pair(UserDefinedTerminalGrammarSymbol{\"" << t.name()
                                   << "\"}, value);\n" <<
                                   "}\n";
                        },
                        [&](Epsilon const &t) {
                            assert(false);
                        }
                }, std::get<TerminalGrammarSymbol>(sym));
            }
        }
    }

    header << "#endif\n";
    header.close();

    source << "SimpleParsingTable *grammar() {\n"
              "       static SimpleParsingTable t {\n";

//    std::map<size_t, const NonterminalDescription*> descriptionsIds;
//    size_t i = 0;
//    for (auto const &descr : descriptions) {
//        descriptionsIds.insert(std::make_pair(i++, &descr.second));
//    }
//
//    for (auto const &descr : descriptionsIds) {
//        source << "NonterminalDescription{{}, \"" << descr.second->name << "\",\n std::vector<Rule>{\n";
//        for (Rule const &rule : descr.second->rules) {
//    rules[rule.id] = &rule;
//            source << "Rule{ ";
//            for (auto const& sym : rule.body) {
//                writeConstructor(source, sym) << ", ";
//            }
//            source << " },\n";
//        }
//        source << "},\n {}},\n";
//    }
//
//    for (size_t i = 0; i < rules.size(); ++i) {
//        source << "NonterminalDescription{SIZE_MAX, std::vector<GrammarSymbol>{\n" <<
//               rules[i]->body <<
//               "\n},";
//    }
    source << actionsType << "{\n";

    for (size_t i = 0; i < table.actions.size(); ++i) {
        source << "{\n";
        for (auto const &p : table.actions[i]) {
            source << "\tstd::make_pair(TerminalGrammarSymbol{";
            writeConstructor(source, p.first) << "}, ";
            writeConstructor(source, p.second) << "),\n";
        }
        source << "},\n";
    }
    source << "},\n";

    source << gotoType << "{\n";
    for (size_t i = 0; i < table.gotoActions.size(); ++i) {
        source << "{\n";
        for (auto const &p : table.gotoActions[i]) {
            source << "\tstd::make_pair(\"" << p.first.name() << "\", " << p.second << "),\n";
        }
        source << "},\n";
    }
    source << "},\n";
    source << "};\n";
    source << "return &t;\n";
    source << "}\n";
    source.close();
}

#endif //LAB4_CPP_GENERATOR_H
