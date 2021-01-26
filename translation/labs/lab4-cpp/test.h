#ifndef __PARSER_HEADER__
#define __PARSER_HEADER__
 #include "parsing_rule_structures.h"
#include <any>
struct SimpleParsingTable;
SimpleParsingTable* grammar();
const size_t start = 0;
const std::vector<ParserRule> rules {
	ParserRule{"S'", 1/*0*/},
	ParserRule{"MulDiv", 3/*1*/},
	ParserRule{"MulDiv", 3/*2*/},
	ParserRule{"MulDiv", 1/*3*/},
	ParserRule{"AddSub", 3/*4*/},
	ParserRule{"AddSub", 3/*5*/},
	ParserRule{"AddSub", 1/*6*/},
	ParserRule{"Var", 3/*7*/},
	ParserRule{"Var", 1/*8*/},
	ParserRule{"Expression", 1/*9*/},
};
std::any makeAction(size_t i, std::vector<std::any> const& args);
inline std::pair<TerminalGrammarSymbol, std::string> terminal_EOF() {
return std::make_pair(EndOfText{}, "$"); ;
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
