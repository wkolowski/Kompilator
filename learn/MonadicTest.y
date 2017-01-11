{
module Main where

import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (break)

data E a = Ok a | Failed String deriving (Show)

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =  case m of 
	Ok a -> k a
	Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = case m of
	Ok a -> Ok a
	Failed e -> k e
}

%name calc
%monad {E} {thenE} {returnE}
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

Exp	: let var '=' Exp in Exp		{Let $2 $4 $6}
	| Exp '+' Exp				{Plus $1 $3}
	| Exp '-' Exp				{Minus $1 $3}
	| Exp '*' Exp				{Times $1 $3}
	| Exp '/' Exp				{% if $3 == Int 0 then failE "Division by zero" else returnE $ Div $1 $3}
	| '-' Exp %prec NEG			{Negate $2}
	| '(' Exp ')'				{% case $2 of
							Parens _ -> failE "Double parens? U mad bro?"
							_ -> returnE $ Parens $2}
	| int					{Int $1}
	| var					{Var $1}


{
--parseError :: [Token] -> a
parseError _ = failE "Parse Error" --error "Errur wihle parsink"

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
	deriving (Eq, Show)


--main = getContents >>= print . (\toks -> calc toks []) . lexer
main = getContents >>= print . calc . lexer
}
