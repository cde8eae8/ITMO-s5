%require "3.2"
%language "c++"
%define api.value.type variant
%define api.token.constructor

%code requires
{
    #include <vector>
    #include <string>
    #include <unordered_map>
    #include <map>
    #include "parsing_rule_structures.h"

    extern std::map<std::string, std::string> identifiers;
    extern std::unordered_map<std::string, std::vector<RawRule>> rawGrammar;
    extern std::string startNonterminal;
    extern std::string header;
}

%code
{
namespace yy
{
    // Report an error to the user.
    auto parser::error (const std::string& msg) -> void
    {
        std::cerr << msg << '\n';
    }
}
}

%code { }

%%

SourceFile:
  code Types StartNonterminal Nonterminals { header = $1; }
  ;

StartNonterminal: '@' identifier { startNonterminal = $2; }
    ;

Types:
    Type
  | Types Type
  ;

Type:
  identifier '<' identifier '>' { identifiers.insert(std::make_pair($1, $3)); }
  ;

Nonterminals:
   Nonterminal
 | Nonterminals Nonterminal
 ;

Nonterminal:
    identifier Rules ';' { rawGrammar.insert(std::make_pair($1, $2)); }
    ;

Rules:
   Rule { $$ = std::vector<RawRule>{std::move($1)}; }
 | Rules Rule { $1.push_back($2); $$ = std::move($1); }
 ;

Rule:
   '|' Tokens code { $$ = RawRule{std::move($2), std::move($3)}; }
   ;

Tokens:
   identifier { $$ = std::vector<std::string>{$1}; }
 | Tokens identifier { $1.push_back($2); $$ = std::move($1); }
 ;

%nterm <std::string> StartNonterminal;
%nterm <std::vector<std::string>> Tokens;
%nterm <RawRule> Rule;
%nterm <std::vector<RawRule>> Rules;

%token <std::string> identifier;
%token <std::string> code;

%code {
namespace yy {
        // Return the next token.
        auto yylex() -> parser::symbol_type {
            int c;
            do {
                c = getchar();
            } while (c == ' ' || c == '\t' || c == '\r' || c == '\n');
            if (c == EOF) return parser::make_YYEOF();
            if (isalpha(c)) {
                std::string s;
                while (isalpha(c) || isdigit(c) || c == '*') {
                    s += c;
                    c = getchar();
                }
                ungetc(c, stdin);
                std::cout << "id: " << s << std::endl;
                return parser::make_identifier(s);
            }

            if (c == '{') {
                std::string s;
                c = getchar();
                while (c != '}') {
                    s += c;
                    c = getchar();
                }
                // ungetc(c, stdin);
                std::cout << "code: " << s << std::endl;
                return parser::make_code(s);
            }
            return c;
        }
    }
};
