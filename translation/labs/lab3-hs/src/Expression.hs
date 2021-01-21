{-# LANGUAGE ExistentialQuantification #-}
module Expression ( Block(..)
                  , unpack
                  , Expression(..)
                  , BaseOperand(..)
                  , Type(..)
) where

import Data.List
import Data.List.NonEmpty

newtype Type = Type String 

instance Show Type where
  show (Type s) = s

data BaseOperand = Field (NonEmpty String)
                 | FunctionCall (NonEmpty String) [Expression]
                 | Extended BaseOperand BaseOperand

instance Show BaseOperand where
  show (Field paths) = 
    intercalate "." (toList paths)
  show (FunctionCall paths args) = 
    intercalate "." (toList paths) ++ "(" ++ intercalate ", " (fmap show args) ++ ")" 
  show (Extended l r) = show l ++ "." ++ show r

data Expression = Definition Type String
                | DefinitionInitialized Type String Expression 
                | For Expression Expression Expression Block
                | While Expression Block 
                | If Expression Block
                | IfElse Expression Block Block
                | Assign Expression Expression 
                | Variable BaseOperand
                | OpExpression String Expression Expression

instance Show Expression where
  show (Definition typ name) = 
    show typ ++ " " ++ name ++ ";"

  show (DefinitionInitialized typ name initValue) = 
    show typ ++ " " ++ name ++ " = " ++ show initValue ++ ";"

  show (For init cond update body) = 
    "for (" ++ show init ++ "; " ++ show cond ++ "; " ++ show update ++ ") {\n" 
      ++ show body ++ 
    "}"

  show (While cond body) = 
    "while (" ++ show cond ++ ") {\n" 
      ++ show body ++ 
    "}"

  show (If cond body) = 
    "if " ++ show cond ++ " {\n"
      ++ show body ++ 
    "}"

  show (IfElse cond body elseBody) = 
    "if " ++ show cond ++ " {\n"
      ++ show body ++ 
    "} else {\n"
      ++ show elseBody ++
    "}"

  show (Assign left right) = show left ++ " = " ++ show right

  show (Variable v) = show v
  
  show (OpExpression sign l r) = show l ++ sign ++ show r


--- Block ---
newtype Block = Block [Expression]

unpack :: Block -> [Expression]
unpack (Block b) = b

instance Show Block where
  show (Block exprs) = concatMap (\e -> show e ++ "\n") exprs


