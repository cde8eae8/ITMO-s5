{
module Lexer where

}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters


tokens :-
    $white+                         ;
    "//".*                          ;
    \(                              { tok TokLBracket }
    \)                              { tok TokRBracket }
    \}                              { tok TokRBrace }
    \{                              { tok TokLBrace }
    "[]"                            { tok TokArray }
    "."                             { tok TokDot }
    ","                             { tok TokComma }
    ";"                             { tok TokDotComma }
    "if"                            { tok TokIf }
    "else"                          { tok TokElse }
    "while"                         { tok TokWhile }
    "class"                         { tok TokClass }
    "import"                         { tok TokImport }
    "public"                        { \_ s -> TokAttribute s }
    "static"                        { \_ s -> TokAttribute s }
    "new"                           { tok TokNew }
    "="                             { tok TokAssign }
    $digit+                         { \_ s -> TokLiteral s }
    "+"|"-"|"*"|"/"|"=="|"!="|"&"|"|"|"^"|"&&"|"||"|">="|"<="|">"|"<"
                                    { \_ s -> TokSign (s) }
    $alpha [$alpha $digit \_]*      { \_ s -> TokIdentifier s }

{
-- Each right-hand side has type :: String -> Token
tok :: Token -> AlexPosn -> String -> Token
tok t = (\_ _ -> t)

-- The token type:
data Token = TokLiteral String
           | TokIdentifier String
           | TokSign String
           | TokIf 
           | TokElse 
           | TokFor
           | TokWhile
           | TokLBracket
           | TokRBracket
           | TokArray
           | TokLBrace
           | TokRBrace
           | TokDot 
           | TokComma 
           | TokDotComma 
           | TokEOF
           | TokAssign
           | TokClass
           | TokImport
           | TokAttribute String
           | TokNew 
           deriving (Eq,Show)
}

