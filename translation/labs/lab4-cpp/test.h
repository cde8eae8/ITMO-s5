#ifndef __PARSER_HEADER__
#define __PARSER_HEADER__

#include <cmath>
#include "parsing_rule_structures.h"
#include <any>
struct SimpleParsingTable;
SimpleParsingTable* grammar();
const size_t start = 0;
const std::vector<ParserRule> rules {
	ParserRule{"S'", 1/*0*/},
	ParserRule{"Power", 3/*1*/},
	ParserRule{"Power", 1/*2*/},
	ParserRule{"MulDiv", 3/*3*/},
	ParserRule{"MulDiv", 3/*4*/},
	ParserRule{"MulDiv", 1/*5*/},
	ParserRule{"AddSub", 3/*6*/},
	ParserRule{"AddSub", 3/*7*/},
	ParserRule{"AddSub", 1/*8*/},
	ParserRule{"Var", 3/*9*/},
	ParserRule{"Var", 1/*10*/},
	ParserRule{"Expression", 1/*11*/},
};
std::any makeAction(size_t i, std::vector<std::any> const& args);
inline std::pair<TerminalGrammarSymbol, std::string> terminal_EOF() {
return std::make_pair(EndOfText{}, "$"); ;
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_power(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"power"}, value);
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_mul(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"mul"}, value);
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_div(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"div"}, value);
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_plus(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"plus"}, value);
}
inline std::pair<TerminalGrammarSymbol, std::string> terminal_minus(std::string const& value = "") {
return std::make_pair(UserDefinedTerminalGrammarSymbol{"minus"}, value);
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
