module AST
	( module AST
	, module Lexer
	) where

import Lexer (AlexPosn (..))

data Program
	= Program [Declaration] [Command]
	deriving (Show)

data Declaration
	= Scalar String AlexPosn
	| Array String Integer AlexPosn
	deriving (Eq, Show)

data Command
	= Asgn Identifier Expression
	| If Condition [Command] [Command]
	| While Condition [Command]
	| ForUp String Value Value [Command]
	| ForDown String Value Value [Command]
	| Read Identifier
	| Write Value
	| Skip
	deriving (Eq, Show)

data Expression
	= Value Value
	| Plus Value Value
	| Minus Value Value
	| Mul Value Value
	| Div Value Value
	| Mod Value Value
	deriving (Eq, Show)

data Condition
	= Eq Value Value
	| Neq Value  Value
	| Lt Value Value
	| Gt Value Value
	| Le Value Value
	| Ge Value Value
	deriving (Eq, Show)

data Value
	= Num Integer
	| Identifier Identifier
	deriving (Eq, Show)

-- Identifiers are either just names of scalars (Pidentifier, e. g. name), names of arrays
-- indexed by names of scalars (ArrayPidentifier, e. g. arr[i]) or names of arrays indexed
-- by numerical constacts (ArrayNum, e. g. arr[5]).
data Identifier
	= Pidentifier String AlexPosn
	| ArrayPidentifier String String AlexPosn AlexPosn
	| ArrayNum String Integer AlexPosn
	deriving (Eq)

instance Show Identifier where
	show (Pidentifier name _) = name
	show (ArrayPidentifier name indexName _ _) = name ++ "[" ++ indexName ++ "]"
	show (ArrayNum name index _) = name ++ "[" ++ (show index) ++ "]"
