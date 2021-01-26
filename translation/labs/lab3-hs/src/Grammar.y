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
    classT { TokClass }
    identifier { TokIdentifier $$ }
    '(' { TokLBracket }
    ')' { TokRBracket }
    '{' { TokLBrace }
    '}' { TokRBrace }
    array { TokArray }
    '.' { TokDot }
    ',' { TokComma }
    ';' { TokDotComma }
    '=' { TokAssign } 
    sign { TokSign $$ }
    literal { TokLiteral $$ }
    attribute { TokAttribute $$ }
    new { TokNew }
    import { TokImport }
%%

SourceFile : Imports Class { ($1, $2) } -- Block { Block $1 }

Imports : 
    {- empty -} { [] }
  | Import         { [$1] }
  | Imports Import { $2 : $1 }

Import : import PathIdentifier ';' { Import $2 }

-- Class
Class : Attributes classT identifier '{' FunctionsR '}' { Class $1 $3 $5 }

-- [Function]
FunctionsR : Functions { Prelude.reverse $1 }

-- [Function] 
Functions : 
      Function { [$1] }
    | Functions Function { $2 : $1 }

-- Function
Function : 
  -- Type identifier '(' ')' '{' Block '}' { Function $1 $2 [] (Block $6) }
  Attributes Type identifier '(' FunctionsDefinitionArgsR ')' '{' Block '}' 
    { Function (Prelude.reverse $1) $2 $3 $5 (Block $8) }

Attributes : 
    {- empty -}          { [] }
  | Attribute            { [$1] }
  | Attributes Attribute { $2 : $1 }

Attribute : attribute { $1 }

FunctionsDefinitionArgsR : FunctionsDefinitionArgs { Prelude.reverse $1 }

-- [(Type, String)]
FunctionsDefinitionArgs : 
      FunctionsDefinitionArg { [$1] }
    | FunctionsDefinitionArgs ',' FunctionsDefinitionArg { $3 : $1 }

-- (Type, String)
FunctionsDefinitionArg : Type identifier { ($1, $2) }

Block: BlockR { Prelude.reverse $1 }

-- Block :: [Expression]
BlockR : Expression { [$1] }
       | BlockR Expression { $2 : $1 }

-- expression :: Expression 
Expression: 
    if '(' OpExpression ')' '{' Block '}'    { If $3 (Block $6) }
  | while '(' OpExpression ')' '{' Block '}' { While $3 (Block $6) }
  | if '(' OpExpression ')' '{' Block '}' else '{' Block '}' { IfElse $3 (Block $6) (Block $10) }
  | OpExpression ';'                         { SingleOpExpression $1 }
  | Type identifier '=' OpExpression ';'     { DefinitionInitialized $1 $2 $4 }
  | Type identifier ';'                      { Definition $1 $2 }
  | OpExpression '=' OpExpression ';'        { Assign $1 $3 }

-- expression1 :: Expression
Expression1
  : BaseExpression { Variable $1 }
  | '(' OpExpression ')' { BracketExpression $2 }

BaseExpression 
  : PathIdentifier 
    { Field $1 }
  | PathIdentifier '(' FuncCallArgs ')'
    { FunctionCall $1 $3 }
  | PathIdentifier '(' FuncCallArgs ')' '.' BaseExpression
    { Extended (FunctionCall $1 $3) $6 }
  | literal 
    { Literal $1 }
  | new BaseExpression
    { NewExpression $2 }

-- OpExpression :: Expression
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
Type: 
      identifier { Type $1 }
    | identifier array { Type ($1 ++ "[]") } 

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

