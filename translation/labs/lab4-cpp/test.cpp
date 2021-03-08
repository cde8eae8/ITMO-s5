#include <unordered_map>
#include <vector>
#include <string>
#include "parsing_rule_structures.h"
#include "parser.h"

#include <cmath>

std::any makeAction(size_t i, std::vector<std::any> const& args) {
	if(i == 0) {
	/* S' -> [ Expression ] */
	auto f = [](int _1) -> int{
		return _1;
	};
	return f(std::any_cast<int>(args[0]));
	}
	if(i == 1) {
	/* Power -> [ Var power Power ] */
	auto f = [](int _1, std::string const& _2, int _3) -> int{
		 return pow(_1, _3); 
	};
	return f(std::any_cast<int>(args[0]), std::any_cast<std::string>(args[1]), std::any_cast<int>(args[2]));
	}
	if(i == 2) {
	/* Power -> [ Var ] */
	auto f = [](int _1) -> int{
		 return _1; 
	};
	return f(std::any_cast<int>(args[0]));
	}
	if(i == 3) {
	/* MulDiv -> [ MulDiv mul Power ] */
	auto f = [](int _1, std::string const& _2, int _3) -> int{
		 return _1 * _3; 
	};
	return f(std::any_cast<int>(args[0]), std::any_cast<std::string>(args[1]), std::any_cast<int>(args[2]));
	}
	if(i == 4) {
	/* MulDiv -> [ MulDiv div Power ] */
	auto f = [](int _1, std::string const& _2, int _3) -> int{
		 return _1 / _3; 
	};
	return f(std::any_cast<int>(args[0]), std::any_cast<std::string>(args[1]), std::any_cast<int>(args[2]));
	}
	if(i == 5) {
	/* MulDiv -> [ Power ] */
	auto f = [](int _1) -> int{
		 return _1; 
	};
	return f(std::any_cast<int>(args[0]));
	}
	if(i == 6) {
	/* AddSub -> [ AddSub plus MulDiv ] */
	auto f = [](int _1, std::string const& _2, int _3) -> int{
		 return _1 + _3; 
	};
	return f(std::any_cast<int>(args[0]), std::any_cast<std::string>(args[1]), std::any_cast<int>(args[2]));
	}
	if(i == 7) {
	/* AddSub -> [ AddSub minus MulDiv ] */
	auto f = [](int _1, std::string const& _2, int _3) -> int{
		 return _1 - _3; 
	};
	return f(std::any_cast<int>(args[0]), std::any_cast<std::string>(args[1]), std::any_cast<int>(args[2]));
	}
	if(i == 8) {
	/* AddSub -> [ MulDiv ] */
	auto f = [](int _1) -> int{
		 return _1; 
	};
	return f(std::any_cast<int>(args[0]));
	}
	if(i == 9) {
	/* Var -> [ lBracket Expression rBracket ] */
	auto f = [](std::string const& _1, int _2, std::string const& _3) -> int{
		 return _2; 
	};
	return f(std::any_cast<std::string>(args[0]), std::any_cast<int>(args[1]), std::any_cast<std::string>(args[2]));
	}
	if(i == 10) {
	/* Var -> [ identifier ] */
	auto f = [](std::string const& _1) -> int{
		 return std::stoi(_1); 
	};
	return f(std::any_cast<std::string>(args[0]));
	}
	if(i == 11) {
	/* Expression -> [ AddSub ] */
	auto f = [](int _1) -> int{
		 return _1; 
	};
	return f(std::any_cast<int>(args[0]));
	}
}
SimpleParsingTable *grammar() {
       static SimpleParsingTable t {
std::vector<std::unordered_map<TerminalGrammarSymbol, ParserAction>>{
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{10}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"div"}}, ParserReduce{10}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserReduce{10}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"mul"}}, ParserReduce{10}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserReduce{10}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"power"}}, ParserReduce{10}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{10}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserAccept{}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{5}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"div"}}, ParserReduce{5}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserReduce{5}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"mul"}}, ParserReduce{5}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserReduce{5}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{5}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"div"}}, ParserReduce{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserReduce{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"mul"}}, ParserReduce{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserReduce{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"power"}}, ParserShift{9}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{8}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserReduce{8}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserReduce{8}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{8}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"div"}}, ParserShift{11}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"mul"}}, ParserShift{10}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{11}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{11}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserShift{13}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserShift{12}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserShift{14}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{9}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"div"}}, ParserReduce{9}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserReduce{9}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"mul"}}, ParserReduce{9}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserReduce{9}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"power"}}, ParserReduce{9}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{9}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{1}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"div"}}, ParserReduce{1}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserReduce{1}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"mul"}}, ParserReduce{1}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserReduce{1}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{3}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"div"}}, ParserReduce{3}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserReduce{3}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"mul"}}, ParserReduce{3}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserReduce{3}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{3}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{4}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"div"}}, ParserReduce{4}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserReduce{4}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"mul"}}, ParserReduce{4}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserReduce{4}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{4}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserReduce{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserReduce{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"div"}}, ParserShift{11}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"mul"}}, ParserShift{10}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{7}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"minus"}}, ParserReduce{7}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"plus"}}, ParserReduce{7}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{7}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"div"}}, ParserShift{11}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"mul"}}, ParserShift{10}),
},
},
std::vector<std::unordered_map<std::string, ParserState>>{
{
	std::make_pair("AddSub", 7),
	std::make_pair("MulDiv", 6),
	std::make_pair("Power", 4),
	std::make_pair("Var", 5),
	std::make_pair("Expression", 3),
},
{
},
{
	std::make_pair("AddSub", 7),
	std::make_pair("MulDiv", 6),
	std::make_pair("Power", 4),
	std::make_pair("Var", 5),
	std::make_pair("Expression", 8),
},
{
},
{
},
{
},
{
},
{
},
{
},
{
	std::make_pair("Var", 5),
	std::make_pair("Power", 15),
},
{
	std::make_pair("Var", 5),
	std::make_pair("Power", 16),
},
{
	std::make_pair("Var", 5),
	std::make_pair("Power", 17),
},
{
	std::make_pair("MulDiv", 18),
	std::make_pair("Var", 5),
	std::make_pair("Power", 4),
},
{
	std::make_pair("MulDiv", 19),
	std::make_pair("Var", 5),
	std::make_pair("Power", 4),
},
{
},
{
},
{
},
{
},
{
},
{
},
},
};
return &t;
}
