{
module Main where

import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (break)
}

%name calc
%tokentype {Token}
%error {parseError}

%token
      let             {TLet}
      in              {TIn}
      int             {TInt $$}
      var             {TVar $$}
      '='             {TEq}
      '+'             {TPlus}
      '-'             {TMinus}
      '*'             {TTimes}
      '/'             {TDiv}
      '('             {TLParen}
      ')'             {TRParen}

%right in
%left '+' '-'
%left '*' '/'
%left NEG

%%

-- Old version that computes an AST.
{-Exp	: let var '=' Exp in Exp		{Let $2 $4 $6}
	| Exp1					{Exp1 $1}

Exp1	: Exp1 '+' Term				{Plus $1 $3}
	| Exp1 '-' Term				{Minus $1 $3}
	| Term					{Term $1}

Term	: Term '*' Factor			{Times $1 $3}
	| Term '/' Factor			{Div $1 $3}
	| Factor				{Factor $1}

Factor	: int					{Int $1}
	| var					{Var $1}
	| '(' Exp ')'				{Parens $2}

-- New version that computes a value.
Exp	: let var '=' Exp in Exp		{\p -> $6 (($2, $4 p) : p)}
	| Exp1					{$1}

Exp1	: Exp1 '+' Term				{\p -> $1 p + $3 p}
	| Exp1 '-' Term				{\p -> $1 p - $3 p}
	| Term					{$1}

Term	: Term '*' Factor			{\p -> $1 p * $3 p}
	| Term '/' Factor			{\p -> $1 p `div` $3 p}
	| Factor				{$1}

Factor	: int					{\p -> $1}
	| var					{\p -> case lookup $1 p of
							Nothing -> error "Variable not found"
							Just v -> v}
	| '(' Exp ')'				{$2}
--}

Exp	: let var '=' Exp in Exp		{Let $2 $4 $6}
	| Exp '+' Exp				{Plus $1 $3}
	| Exp '-' Exp				{Minus $1 $3}
	| Exp '*' Exp				{Times $1 $3}
	| Exp '/' Exp				{Div $1 $3}
	| '-' Exp %prec NEG			{Negate $2}
	| '(' Exp ')'				{Parens $2}
	| int					{Int $1}
	| var					{Var $1}


{
parseError :: [Token] -> a
parseError _ = error "Errur wihle parsink"

data Token
	= TLet
	| TIn
	| TInt Int
	| TVar String
	| TEq
	| TPlus
	| TMinus
	| TTimes
	| TDiv
	| TLParen
	| TRParen
	deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer str@(c:cs)
	| isAlpha c = lexVar str
	| isDigit c = lexNum str
lexer ('=':cs) = TEq : lexer cs
lexer ('+':cs) = TPlus : lexer cs
lexer ('-':cs) = TMinus : lexer cs
lexer ('*':cs) = TTimes : lexer cs
lexer ('/':cs) = TDiv : lexer cs
lexer ('(':cs) = TLParen : lexer cs
lexer (')':cs) = TRParen : lexer cs
lexer (_:cs) = lexer cs

lexVar :: String -> [Token]
lexVar str = TVar name : lexer rest where
	(name, rest) = break isSpace str

lexNum :: String -> [Token]
lexNum str = TInt n : lexer rest where
	(numStr, rest) = break (not . isDigit) str
	n = read numStr :: Int

data Exp
	= Let String Exp Exp
	| Plus Exp Exp
	| Minus Exp Exp
	| Times Exp Exp
	| Div Exp Exp
	| Negate Exp
	| Parens Exp
	| Int Int
	| Var String
	deriving (Show)


-- Old version.
{-data Exp
	= Let String Exp Exp
	| Exp1 Exp1
	deriving (Show)

data Exp1
	= Plus Exp1 Term
	| Minus Exp1 Term
	| Term Term
	deriving (Show)

data Term
	= Times Term Factor
	| Div Term Factor
	| Factor Factor
	deriving (Show)

data Factor
	= Int Int
	| Var String
	| Parens Exp
	deriving (Show)
--}

--main = putStrLn "xd"
--main = getContents >>= print . (\toks -> calc toks []) . lexer
main = getContents >>= print . calc . lexer
}
