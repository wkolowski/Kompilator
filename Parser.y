{
module Main where

import Lexer
}

%name parse
-- %monad {Either String}
%tokentype {Token}
%error {parseError}

%token
	-- Keywords.
	var			{TVAR}
	begin			{TBEGIN}
	end			{TEND}
	if			{TIF}
	then			{TTHEN}
	else			{TELSE}
	endif			{TENDIF}
	while			{TWHILE}
	do			{TDO}
	endwhile		{TENDWHILE}
	for			{TFOR}
	from			{TFROM}
	to			{TTO}
	endfor			{TENDFOR}
	downto			{TDOWNTO}
	read			{TREAD}
	write			{TWRITE}
	skip			{TSKIP}

	-- Arithemtic operators.
	'+'			{TPlus}
	'-'			{TMinus}
	'*'			{TMul}
	'/'			{TDiv}
	'%'			{TMod}

	-- Relational operators.
	'='			{TEq}
	"<>"			{TNeq}
	'<'			{TLt}
	"<="			{TLe}
	">="			{TGe}
	'>'			{TGt}

	-- Assingment and semicolon.
	":="			{TAsgn}
	';'			{TSemicolon}

	-- Parentheses and brackets.
	'('			{TLParen}
	')'			{TRParen}
	'['			{TLBracket}
	']'			{TRBracket}

	-- Numbers.
	num			{TNum $$}

	-- Identifiers.
	pidentifier		{TId $$}

-- %left '+' '-'
-- %left '*' '/' '%'
%%
Program		: var Declarations begin Commands end					{\ctx -> Program (reverse ($2 ctx)) (reverse ($4 ctx))}

Declarations	: Declarations pidentifier						{\ctx -> case $2 `elem` ctx of
												False -> Scalar $2 : ($1 ($2 : ctx))
												_ -> error ("Variable named " ++ (show $2) ++ " already used!")}
		| Declarations pidentifier '[' num ']'					{\ctx -> case $2 `elem` ctx of
												False -> Array $2 $4 : $1 ($2 : ctx)
												_ -> error ("Variable named " ++ (show $2) ++ " already used!")}
		| {- empty -}								{\_ -> []}

Commands	: Commands Command							{\ctx -> ($2 ctx) : ($1 ctx)}
		| Command								{\ctx -> [$1 ctx]}

Command		: Identifier ":=" Expression ';'					{\ctx -> case (nameOfIdent $ $1 ctx) `elem` ctx of
													True -> Asgn ($1 ctx) ($3 ctx)
													_ -> error ("Unknown variable: " ++ (show $ nameOfIdent ($1 ctx)))}
		| if Condition then Commands else Commands endif			{\ctx -> If ($2 ctx) (reverse ($4 ctx)) (reverse ($6 ctx))}
		| while Condition do Commands endwhile					{\ctx -> While ($2 ctx) (reverse ($4 ctx))}
		| for pidentifier from Value to Value do Commands endfor		{\ctx -> ForUp $2 ($4 ctx) ($6 ctx) (reverse ($8 ctx))}
		| for pidentifier from Value downto Value do Commands endfor		{\ctx -> ForDown $2 ($4 ctx) ($6 ctx) (reverse ($8 ctx))}
		| read Identifier ';'							{\ctx -> Read ($2 ctx)}
		| write Value ';'							{\ctx -> Write ($2 ctx)}
		| skip ';'								{\_ -> Skip}

Expression	: Value									{\ctx -> Value ($1 ctx)}
		| Value '+' Value							{\ctx -> Plus ($1 ctx) ($3 ctx)}
		| Value '-' Value							{\ctx -> Minus ($1 ctx) ($3 ctx)}
		| Value '*' Value							{\ctx -> Mul ($1 ctx) ($3 ctx)}
		| Value '/' Value							{\ctx -> Div ($1 ctx) ($3 ctx)}
		| Value '%' Value							{\ctx -> Mod ($1 ctx) ($3 ctx)}

Condition	: Value '=' Value							{\ctx -> Eq ($1 ctx) ($3 ctx)}
		| Value "<>" Value							{\ctx -> Neq ($1 ctx) ($3 ctx)}
		| Value '<' Value							{\ctx -> Lt ($1 ctx) ($3 ctx)}
		| Value '>' Value							{\ctx -> Gt ($1 ctx) ($3 ctx)}
		| Value "<=" Value							{\ctx -> Le ($1 ctx) ($3 ctx)}
		| Value ">=" Value							{\ctx -> Ge ($1 ctx) ($3 ctx)}

Value		: num									{\ctx -> Num $1}
		| Identifier								{\ctx -> Identifier ($1 ctx)}

Identifier	: pidentifier								{\ctx -> Pidentifier $1}
		| pidentifier '[' pidentifier ']'					{\ctx -> ArrayPidentifier $1 $3}
		| pidentifier '[' num ']'						{\ctx -> ArrayNum $1 $3}

{
parseError :: [Token] -> a
parseError _ = error "Errur wihle parsink"

data Program
	= Program [Declaration] [Command]
	deriving (Show)

data Declaration
	= Scalar String
	| Array String Integer
	deriving (Show)

data Command
	= Asgn Identifier Expression
	| If Condition [Command] [Command]
	| While Condition [Command]
	| ForUp String Value Value [Command]
	| ForDown String Value Value [Command]
	| Read Identifier
	| Write Value
	| Skip
	deriving (Show)

data Expression
	= Value Value
	| Plus Value Value
	| Minus Value Value
	| Mul Value Value
	| Div Value Value
	| Mod Value Value
	deriving (Show)

data Condition
	= Eq Value Value
	| Neq Value  Value
	| Lt Value Value
	| Gt Value Value
	| Le Value Value
	| Ge Value Value
	deriving (Show)

data Value
	= Num Integer
	| Identifier Identifier
	deriving (Show)

data Identifier
	= Pidentifier String
	| ArrayPidentifier String String
	| ArrayNum String Integer
	deriving (Show)

nameOfIdent :: Identifier -> String
nameOfIdent (Pidentifier str) = str
nameOfIdent (ArrayPidentifier str _) = str
nameOfIdent (ArrayNum str _) = str

main = getContents >>= print . (\tokens -> parse tokens []) . alexScanTokens
}
