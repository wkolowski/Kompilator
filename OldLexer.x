{
	module Lexer where
}

%wrapper "basic"

tokens :-
	-- Ignore whitespace and comments.
	$white			;
	\{[^\}]*\}		;

	-- Keywords.
	VAR			{\_ -> Kw VAR}
	BEGIN			{\_ -> Kw BEGIN}
	END			{\_ -> Kw END}
	IF 			{\_ -> Kw IF}
	THEN			{\_ -> Kw THEN}
	ELSE			{\_ -> Kw ELSE}
	ENDIF			{\_ -> Kw ENDIF}
	WHILE			{\_ -> Kw WHILE}
	DO			{\_ -> Kw DO}
	ENDWHILE		{\_ -> Kw ENDWHILE}
	FOR			{\_ -> Kw FOR}
	FROM			{\_ -> Kw FROM}
	TO			{\_ -> Kw TO}
	ENDFOR			{\_ -> Kw ENDFOR}
	DOWNTO			{\_ -> Kw DOWNTO}
	READ			{\_ -> Kw READ}
	WRITE			{\_ -> Kw WRITE}
	SKIP			{\_ -> Kw SKIP}

	-- Semicolon and various parentheses/brackets.
	\;			{\_ -> Semicolon}
	\(			{\_ -> LParen}
	\)			{\_ -> RParen}
	\[			{\_ -> LBracket}
	\]			{\_ -> RBracket}

	-- Arithemtic operators.
	\+			{\_ -> ArOp Plus}
	\-			{\_ -> ArOp Minus}
	\*			{\_ -> ArOp Mul}
	\/			{\_ -> ArOp Div}
	\%			{\_ -> ArOp Mod}

	-- Relational operators.
	\=			{\_ -> RelOp Eq}
	\<\>			{\_ -> RelOp Neq}
	\<			{\_ -> RelOp Lt}
	\<\=			{\_ -> RelOp Le}
	\>\=			{\_ -> RelOp Ge}
	\>			{\_ -> RelOp Gt}

	-- Assignment.
	\:\=			{\_ -> Asgn}

	-- Numbers.
	[0-9]+			{\s -> Num (read s :: Integer)}

	-- Identifiers.
	[_a-z]+			{\s -> Id s}
	[_a-z]+\[[0-9]+\]	{\s -> ArrayNum (filter (`elem` '-' : ['a'..'z']) s) ((read $ filter (`elem` ['0'..'9']) s) :: Integer)}
	[_a-z]+\[[_a-z]+\]	{\s -> ArrayIdent (takeWhile (/= '[') s) (init . tail $ dropWhile (/= '[') s)}

{

data Keyword = VAR | BEGIN | END | IF | THEN | ELSE | ENDIF | WHILE | DO | ENDWHILE | FOR | FROM | TO | ENDFOR | DOWNTO | READ | WRITE | SKIP deriving (Eq, Show)
data ArithmeticOperator = Plus | Minus | Mul | Div | Mod deriving (Eq, Show)
data RelationalOperator = Eq | Neq | Lt | Le | Ge | Gt deriving (Eq, Show)
type Ident = String

data Token = Kw Keyword | Semicolon | ArOp ArithmeticOperator | RelOp RelationalOperator | Asgn | Num Integer | Id Ident | ArrayNum Ident Integer | ArrayIdent Ident Ident | LParen | RParen | LBracket | RBracket deriving (Eq, Show)

{--main = do
	s <- getContents
	print (alexScanTokens s)--}
}
