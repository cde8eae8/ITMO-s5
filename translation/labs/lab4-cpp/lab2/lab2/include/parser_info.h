#ifndef __PARSER_HEADER__
#define __PARSER_HEADER__

#include "tree.h"
#include "parsing_rule_structures.h"
#include <any>
struct SimpleParsingTable;
SimpleParsingTable* grammar();
const size_t start = 0;
const std::vector<ParserRule> rules {
	ParserRule{"S'", 1/*0*/},
	ParserRule{"Not", 2/*1*/},
	ParserRule{"Not", 1/*2*/},
	ParserRule{"And", 3/*3*/},
	ParserRule{"And", 1/*4*/},
	ParserRule{"Or", 3/*5*/},
	ParserRule{"Or", 1/*6*/},
	ParserRule{"Xor", 3/*7*/},
	ParserRule{"Xor", 1/*8*/},
	ParserRule{"Var", 3/*9*/},
	ParserRule{"Var", 1/*10*/},
	ParserRule{"Expression", 1/*11*/},
};
std::any makeAction(size_t i, std::vector<std::any> const& args);
inline std::pair<TerminalGrammarSymbol, std::string> terminal_EOF() {
return std::make_pair(EndOfText{}, "$"); ;
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_not(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"not"}, value);
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_and(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"and"}, value);
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_or(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"or"}, value);
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_xor(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"xor"}, value);
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_lBracket(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"lBracket"}, value);
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_rBracket(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"rBracket"}, value);
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_identifier(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"identifier"}, value);
}
#endif
