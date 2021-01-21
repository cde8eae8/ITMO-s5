{
module Grammar where
import Lexer
import Expression
import Data.List.NonEmpty
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    if  { TokIf }
    else { TokElse }
    while { TokWhile }
    identifier { TokIdentifier $$ }
    '(' { TokLBracket }
    ')' { TokRBracket }
    '{' { TokLBrace }
    '}' { TokRBrace }
    '.' { TokDot }
    ',' { TokComma }
    ';' { TokDotComma }
    '=' { TokAssign } 
    sign { TokSign $$ }
%%

SourceFile : Block { Block $1 }

Block: BlockR { Prelude.reverse $1 }

-- Block :: [Expression]
BlockR : Expression { [$1] }
       | BlockR Expression { $2 : $1 }

-- expression :: Expression 
Expression: 
    if '(' OpExpression ')' '{' Block '}'    { If $3 (Block $6) }
  | while '(' OpExpression ')' '{' Block '}' { While $3 (Block $6) }
  | if '(' OpExpression ')' '{' Block '}' else '{' Block '}' { IfElse $3 (Block $6) (Block $10) }
  | OpExpression ';'                         { $1 }
  | Type identifier '=' OpExpression ';'     { DefinitionInitialized $1 $2 $4 }
  | Type identifier ';'                      { Definition $1 $2 }
  | OpExpression '=' OpExpression ';'        { Assign $1 $3 }

Expression1: BaseExpression { Variable $1 }

-- expression1 :: Expression
BaseExpression : PathIdentifier 
                { Field $1 }
               | PathIdentifier '(' FuncCallArgs ')'
                { FunctionCall $1 $3 }
               | PathIdentifier '(' FuncCallArgs ')' '.' BaseExpression
                { Extended (FunctionCall $1 $3) $6 }

-- op-expression :: Expression
OpExpression:
      Expression1 { $1 }
    | OpExpression sign Expression1 { OpExpression $2 $1 $3 }

FuncCallArgs:
  FuncCallArgsR { Prelude.reverse $1 }

-- func args :: [Expressions]
FuncCallArgsR:
    {- empty -} { [] }
    | FuncCallArg { [$1] }
    | FuncCallArgsR ',' FuncCallArg { $3 : $1 }

-- func arg :: Expression
FuncCallArg : OpExpression { $1 }

-- type :: String
Type: identifier { Type $1 }

PathIdentifier:
  PathIdentifierR { Data.List.NonEmpty.reverse $1 }

-- path-identifier :: [String]
PathIdentifierR:
      identifier { $1 :| [] } 
    | PathIdentifierR '.' identifier { $3 <| $1 } 

{

parseError :: [Token] -> a
parseError x = error $ "Parse error" ++ show x

}

