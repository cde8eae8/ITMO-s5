//
// Created by nikita on 1/20/21.
//

#ifndef LAB4_CPP_PARSING_TABLE_GENERATOR_H
#define LAB4_CPP_PARSING_TABLE_GENERATOR_H

#include <string>
#include <vector>
#include <variant>
#include <unordered_set>
#include <unordered_map>
#include <map>
#include <cassert>
#include <iostream>


//ParsingTable generateParsingTable(std::vector<GrammarSymbol> const &grammar);

struct RawRule;
void convertGrammar(std::string const& file,
                    std::unordered_map<std::string, std::vector<RawRule>> const& rawGrammar,
                    std::map<std::string, std::string> const& types);

#endif //LAB4_CPP_PARSING_TABLE_GENERATOR_H
