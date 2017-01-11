{
module Main where

import Lexer
}

%name parse
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
Program		: var Declarations begin Commands end					{Program (reverse $2) (reverse $4)}

Declarations	: Declarations pidentifier						{Scalar $2 : $1}
		| Declarations pidentifier '[' num ']'					{Array $2 $4 : $1}
		| {- empty -}								{[]}

Commands	: Commands Command							{$2 : $1}
		| Command								{[$1]}

Command		: Identifier ":=" Expression ';'					{Asgn $1 $3}
		| if Condition then Commands else Commands endif			{If $2 (reverse $4) (reverse $6)}
		| while Condition do Commands endwhile					{While $2 (reverse $4)}
		| for pidentifier from Value to Value do Commands endfor		{ForUp $2 $4 $6 (reverse $8)}
		| for pidentifier from Value downto Value do Commands endfor		{ForDown $2 $4 $6 (reverse $8)}
		| read Identifier ';'							{Read $2}
		| write Value ';'							{Write $2}
		| skip ';'								{Skip}

Expression	: Value									{Value $1}
		| Value '+' Value							{Plus $1 $3}
		| Value '-' Value							{Minus $1 $3}
		| Value '*' Value							{Mul $1 $3}
		| Value '/' Value							{Div $1 $3}
		| Value '%' Value							{Mod $1 $3}

Condition	: Value '=' Value							{Eq $1 $3}
		| Value "<>" Value							{Neq $1 $3}
		| Value '<' Value							{Lt $1 $3}
		| Value '>' Value							{Gt $1 $3}
		| Value "<=" Value							{Le $1 $3}
		| Value ">=" Value							{Ge $1 $3}

Value		: num									{Num $1}
		| Identifier								{Identifier $1}

Identifier	: pidentifier								{Pidentifier $1}
		| pidentifier '[' pidentifier ']'					{ArrayPidentifier $1 $3}
		| pidentifier '[' num ']'						{ArrayNum $1 $3}

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

main = getContents >>= print . parse . alexScanTokens
}
