{-# LANGUAGE ExistentialQuantification #-}
module Expression ( Block(..)
                  , unpack
                  , toString
                  , Expression(..)
                  , BaseOperand(..)
                  , Type(..)
                  , Class(..)
                  , Function(..)
                  , Import(..)
) where

import Prelude hiding (init)
import Data.List
import Data.List.NonEmpty

newtype Type = Type String 

instance Show Type where
  show (Type s) = s

data BaseOperand
  = Field (NonEmpty String)
  | FunctionCall (NonEmpty String) [Expression]
  | Extended BaseOperand BaseOperand
  | Literal String
  | NewExpression BaseOperand

instance Show BaseOperand where
  show (Field paths) = 
    intercalate "." (toList paths)
  show (FunctionCall paths args) = 
    intercalate "." (toList paths) ++ "(" ++ intercalate ", " (fmap show args) ++ ")" 
  show (Extended l r) = show l ++ "." ++ show r
  show (Literal t) = t
  show (NewExpression t) = "new " ++ show t

data Import = Import (NonEmpty String)

instance Show Import where
  show (Import path) = "import " ++ intercalate "." (toList path) ++ ";"

data Class = Class [String] String [Function]

instance Show Class where
  show (Class attrs name funcs) = 
    unwords attrs ++ " " ++
    "class " ++ name ++ "{\n" ++
      intercalate "\n" (show <$> funcs) ++
    "}\n" 

data Function = Function [String] Type String [(Type, String)] Block

instance Show Function where
  show (Function attrs typ name args body) = 
    unwords attrs ++ " " ++
    show typ ++ " " ++ name ++ "(" ++ argsList ++ ") {\n" ++
      show body ++
    "}\n" 
    where 
      argsList :: String
      argsList = intercalate "," ((\(t, n) -> show t ++ " " ++ n) <$> args)

data Expression 
  = Definition Type String
  | DefinitionInitialized Type String Expression 
  | For Expression Expression Expression Block
  | While Expression Block 
  | If Expression Block
  | IfElse Expression Block Block
  | Assign Expression Expression 
  | Variable BaseOperand
  | OpExpression String Expression Expression
  | SingleOpExpression Expression
  | BracketExpression Expression

toString :: Expression -> [String]
toString (Definition typ name) = 
  [show typ ++ " " ++ name ++ ";"]

toString (DefinitionInitialized typ name initValue) = 
  [show typ ++ " " ++ name ++ " = " ++ show initValue ++ ";"]

toString (For init cond update body) = 
  [ "for (" ++ show init ++ "; " ++ show cond ++ "; " ++ show update ++ ") {"] 
  ++   indentBlock body ++
  [ "}" ]

toString (While cond body) = 
  [ "while (" ++ show cond ++ ") {" ]
  ++   indentBlock body ++
  [ "}" ]

toString (If cond body) = 
  [ "if (" ++ show cond ++ ") {" ]
  ++   indentBlock body ++
  [ "}" ]

toString (IfElse cond body elseBody) = 
  ["if (" ++ show cond ++ ") {"]
    ++ indentBlock body ++ 
  ["} else {"]
    ++ indentBlock elseBody ++
  ["}"]

toString (Assign left right) = [show left ++ " = " ++ show right ++ ";"]

toString (Variable v) = [show v]

toString (OpExpression sign l r) = [show l ++ " " ++ sign ++ " " ++ show r]

toString (SingleOpExpression op) = [Prelude.head (toString op) ++ ";"]

toString (BracketExpression expr) = ["(" ++ show expr ++ ")"]

instance Show Expression where
  show expr = intercalate "\n" $ toString expr
 -- show (Definition typ name) = 
 --   show typ ++ " " ++ name ++ ";"

 -- show (DefinitionInitialized typ name initValue) = 
 --   show typ ++ " " ++ name ++ " = " ++ show initValue ++ ";"

 -- show (For init cond update body) = 
 --   "for (" ++ show init ++ "; " ++ show cond ++ "; " ++ show update ++ ") {\n" 
 --     ++ indent body ++ 
 --   "}"

 -- show (While cond body) = 
 --   "while (" ++ show cond ++ ") {\n" 
 --     ++ indent body ++ 
 --   "}"

 -- show (If cond body) = 
 --   "if " ++ show cond ++ " {\n"
 --     ++ indent body ++ 
 --   "}"

 -- show (IfElse cond body elseBody) = 
 --   "if " ++ show cond ++ " {\n"
 --     ++ indent body ++ 
 --   "} else {\n"
 --     ++ indent elseBody ++
 --   "}"

 -- show (Assign left right) = show left ++ " = " ++ show right

 -- show (Variable v) = show v
 -- 
 -- show (OpExpression sign l r) = show l ++ sign ++ show r

 -- show (BracketExpression expr) = "(" ++ show expr ++ ")"


--- Block ---
newtype Block = Block [Expression]

unpack :: Block -> [Expression]
unpack (Block b) = b

instance Show Block where
  show (Block exprs) = concatMap (\e -> show e ++ "\n") exprs

indentBlock :: Block -> [String]
indentBlock block = indent $ unpack block

indent :: [Expression]-> [String]
indent block = ("  " ++) <$> concat (toString <$> block)

