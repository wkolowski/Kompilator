{
module Parser where

import Control.Monad.State			-- Needed to carry context along while parsing.
import qualified Data.Map.Strict as Map		-- Needed to implement context.
import Data.Maybe

import Lexer

-- Any program must be of the form VAR declarations BEGIN commands END
data Program
	= Program [Declaration] [Command]
	deriving (Show)

-- Declarations can be either scalars or arrays.
data Declaration
	= Scalar String AlexPosn
	| Array String Integer AlexPosn
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
}

%name parse
%tokentype {TokWrap}
%error {parseError}

%token
	-- Keywords.
	var			{TW TVAR $$}
	begin			{TW TBEGIN $$}
	end			{TW TEND $$}
	if			{TW TIF $$}
	then			{TW TTHEN $$}
	else			{TW TELSE $$}
	endif			{TW TENDIF $$}
	while			{TW TWHILE $$}
	do			{TW TDO $$}
	endwhile		{TW TENDWHILE $$}
	for			{TW TFOR $$}
	from			{TW TFROM $$}
	to			{TW TTO $$}
	endfor			{TW TENDFOR $$}
	downto			{TW TDOWNTO $$}
	read			{TW TREAD $$}
	write			{TW TWRITE $$}
	skip			{TW TSKIP $$}

	-- Arithemtic operators.
	'+'			{TW TPlus $$}
	'-'			{TW TMinus $$}
	'*'			{TW TMul $$}
	'/'			{TW TDiv $$}
	'%'			{TW TMod $$}

	-- Relational operators.
	'='			{TW TEq $$}
	"<>"			{TW TNeq $$}
	'<'			{TW TLt $$}
	"<="			{TW TLe $$}
	">="			{TW TGe $$}
	'>'			{TW TGt $$}

	-- Assingment and semicolon.
	":="			{TW TAsgn $$}
	';'			{TW TSemicolon $$}

	-- Parentheses and brackets.
	'('			{TW TLParen $$}
	')'			{TW TRParen $$}
	'['			{TW TLBracket $$}
	']'			{TW TRBracket $$}

	-- Numbers.
	num			{TW (TNum $$) _}

	-- Identifiers.
	pidentifier		{TW (TId $$) _}

%%
Program :: {Program}
Program		: var Declarations begin Commands end				{Program (reverse $2) (reverse $4)}

Declarations :: {[Declaration]}
Declarations	: Declarations pidentifier					{Scalar (fst $2) (snd $2) : $1}
		| Declarations pidentifier '[' num ']'				{Array (fst $2) $4 (snd $2) : $1}
		| {- empty -}							{[]}

Commands :: {[Command]}
Commands	: Commands Command						{$2 : $1}
		| Command							{[$1]}

Command :: {Command}
Command		: Identifier ":=" Expression ';'				{Asgn $1 $3}

		| if Condition then Commands else Commands endif		{If $2 (reverse $4) (reverse $6)}
		| while Condition do Commands endwhile				{While $2 (reverse $4)}
		| for pidentifier from Value to Value do Commands endfor	{ForUp (fst $2) $4 $6 (reverse $8)}
		| for pidentifier from Value downto Value do Commands endfor	{ForDown (fst $2) $4 $6 (reverse $8)}
		| read Identifier ';'						{Read $2}
		| write Value ';'						{Write $2}
		| skip ';'							{Skip}

Expression :: {Expression}
Expression	: Value								{Value $1}
		| Value '+' Value						{Plus $1 $3}
		| Value '-' Value						{Minus $1 $3}
		| Value '*' Value						{Mul $1 $3}
		| Value '/' Value						{Div $1 $3}
		| Value '%' Value						{Mod $1 $3}
Condition :: {Condition}
Condition	: Value '=' Value						{Eq $1 $3}
		| Value "<>" Value						{Neq $1 $3}
		| Value '<' Value						{Lt $1 $3}
		| Value '>' Value						{Gt $1 $3}
		| Value "<=" Value						{Le $1 $3}
		| Value ">=" Value						{Ge $1 $3}

Value :: {Value}
Value		: num								{Num $1}
		| Identifier							{Identifier $1}

Identifier :: {Identifier}
Identifier	: pidentifier							{Pidentifier (fst $1) (snd $1)}
		| pidentifier '[' pidentifier ']'				{ArrayPidentifier (fst $1) (fst $3) (snd $1) (snd $3)}
		| pidentifier '[' num ']'					{ArrayNum (fst $1) $3 (snd $1)}
{
parseError :: [TokWrap] -> a
parseError [] = error ("Unknown parse error.")
parseError ((TW tok (AlexPn _ line col)):_) = error ("Parse error in line " ++ (show line) ++ ", column " ++ (show col))
}
