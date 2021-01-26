#include <unordered_map>
#include <vector>
#include <string>
#include "parsing_rule_structures.h"
#include "parser.h"

#include "tree.h"

std::any makeAction(size_t i, std::vector<std::any> const& args) {
	if(i == 0) {
	/* S' -> [ Expression ] */
	auto f = [](Tree* _1) -> Tree*{
		return _1;
	};
	return f(std::any_cast<Tree*>(args[0]));
	}
	if(i == 1) {
	/* Not -> [ not Var ] */
	auto f = [](std::string const& _1, Tree* _2) -> Tree*{
		
        Tree* tree = new Tree("N");
        tree->addChild("!");
        tree->addChild(_2);
        return tree;
    
	};
	return f(std::any_cast<std::string>(args[0]), std::any_cast<Tree*>(args[1]));
	}
	if(i == 2) {
	/* Not -> [ Var ] */
	auto f = [](Tree* _1) -> Tree*{
		
        return _1;
    
	};
	return f(std::any_cast<Tree*>(args[0]));
	}
	if(i == 3) {
	/* And -> [ And and Not ] */
	auto f = [](Tree* _1, std::string const& _2, Tree* _3) -> Tree*{
		
        Tree* tree = new Tree("A");
        tree->addChild(_1);
        tree->addChild("&");
        tree->addChild(_3);
        return tree;
    
	};
	return f(std::any_cast<Tree*>(args[0]), std::any_cast<std::string>(args[1]), std::any_cast<Tree*>(args[2]));
	}
	if(i == 4) {
	/* And -> [ Not ] */
	auto f = [](Tree* _1) -> Tree*{
		 return _1; 
	};
	return f(std::any_cast<Tree*>(args[0]));
	}
	if(i == 5) {
	/* Or -> [ Or or And ] */
	auto f = [](Tree* _1, std::string const& _2, Tree* _3) -> Tree*{
		
        Tree* tree = new Tree("O");
        tree->addChild(_1);
        tree->addChild("|");
        tree->addChild(_3);
        return tree;
    
	};
	return f(std::any_cast<Tree*>(args[0]), std::any_cast<std::string>(args[1]), std::any_cast<Tree*>(args[2]));
	}
	if(i == 6) {
	/* Or -> [ And ] */
	auto f = [](Tree* _1) -> Tree*{
		 return _1; 
	};
	return f(std::any_cast<Tree*>(args[0]));
	}
	if(i == 7) {
	/* Xor -> [ Xor xor Or ] */
	auto f = [](Tree* _1, std::string const& _2, Tree* _3) -> Tree*{
		
        Tree* tree = new Tree("X");
        tree->addChild(_1);
        tree->addChild("^");
        tree->addChild(_3);
        return tree;
    
	};
	return f(std::any_cast<Tree*>(args[0]), std::any_cast<std::string>(args[1]), std::any_cast<Tree*>(args[2]));
	}
	if(i == 8) {
	/* Xor -> [ Or ] */
	auto f = [](Tree* _1) -> Tree*{
		 return _1; 
	};
	return f(std::any_cast<Tree*>(args[0]));
	}
	if(i == 9) {
	/* Var -> [ lBracket Expression rBracket ] */
	auto f = [](std::string const& _1, Tree* _2, std::string const& _3) -> Tree*{
		
        Tree* tree = new Tree("V");
        tree->addChild("(");
        tree->addChild(_2);
        tree->addChild(")");
        return tree;
    
	};
	return f(std::any_cast<std::string>(args[0]), std::any_cast<Tree*>(args[1]), std::any_cast<std::string>(args[2]));
	}
	if(i == 10) {
	/* Var -> [ identifier ] */
	auto f = [](std::string const& _1) -> Tree*{
		
        Tree* tree = new Tree("V");
        tree->addChild(_1);
        return tree;
    
	};
	return f(std::any_cast<std::string>(args[0]));
	}
	if(i == 11) {
	/* Expression -> [ Xor ] */
	auto f = [](Tree* _1) -> Tree*{
		 return _1; 
	};
	return f(std::any_cast<Tree*>(args[0]));
	}
}
SimpleParsingTable *grammar() {
       static SimpleParsingTable t {
std::vector<std::unordered_map<TerminalGrammarSymbol, ParserAction>>{
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"not"}}, ParserShift{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{10}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"and"}}, ParserReduce{10}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"or"}}, ParserReduce{10}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserReduce{10}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{10}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"not"}}, ParserShift{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserAccept{}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"and"}}, ParserReduce{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"or"}}, ParserReduce{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserReduce{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{2}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{4}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"and"}}, ParserReduce{4}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"or"}}, ParserReduce{4}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserReduce{4}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{4}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{11}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{11}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserShift{12}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"or"}}, ParserReduce{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserReduce{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"and"}}, ParserShift{13}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{8}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserReduce{8}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{8}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"or"}}, ParserShift{14}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserShift{15}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{1}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"and"}}, ParserReduce{1}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"or"}}, ParserReduce{1}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserReduce{1}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"not"}}, ParserShift{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"not"}}, ParserShift{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"lBracket"}}, ParserShift{2}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"not"}}, ParserShift{6}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"identifier"}}, ParserShift{1}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{9}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"and"}}, ParserReduce{9}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"or"}}, ParserReduce{9}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserReduce{9}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{9}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{7}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserReduce{7}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{7}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"or"}}, ParserShift{14}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{3}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"and"}}, ParserReduce{3}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"or"}}, ParserReduce{3}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserReduce{3}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{3}),
},
{
	std::make_pair(TerminalGrammarSymbol{EndOfText{}}, ParserReduce{5}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"or"}}, ParserReduce{5}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"xor"}}, ParserReduce{5}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"rBracket"}}, ParserReduce{5}),
	std::make_pair(TerminalGrammarSymbol{UserDefinedTerminalGrammarSymbol{"and"}}, ParserShift{13}),
},
},
std::vector<std::unordered_map<std::string, ParserState>>{
{
	std::make_pair("Or", 9),
	std::make_pair("And", 8),
	std::make_pair("Xor", 7),
	std::make_pair("Not", 5),
	std::make_pair("Var", 4),
	std::make_pair("Expression", 3),
},
{
},
{
	std::make_pair("Or", 9),
	std::make_pair("And", 8),
	std::make_pair("Xor", 7),
	std::make_pair("Not", 5),
	std::make_pair("Var", 4),
	std::make_pair("Expression", 10),
},
{
},
{
},
{
},
{
	std::make_pair("Var", 11),
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
	std::make_pair("Or", 16),
	std::make_pair("And", 8),
	std::make_pair("Not", 5),
	std::make_pair("Var", 4),
},
{
	std::make_pair("Not", 17),
	std::make_pair("Var", 4),
},
{
	std::make_pair("And", 18),
	std::make_pair("Not", 5),
	std::make_pair("Var", 4),
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
