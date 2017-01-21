{
module Parser where

import AST
import Lexer
}

%name parse
%tokentype {TokWrap}
%error {parseError}

-- Values of most tokens are their positions in the file (line number and column).
-- Values of numbers are represented by Integer and values of pidentifiers are
-- represented by Name (which is an alias for String).
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
	'['			{TW TLBracket $$}
	']'			{TW TRBracket $$}

	-- Numbers.
	num			{TW (TNum $$) _}

	-- Identifiers.
	pidentifier		{TW (TId $$) _}

-- Types of nonterminals whose names are singular correspond to types from AST
-- (so that nonterminal Program yields value of type Program etc.) and types of
-- those whose names are plural correspond to lists (so Commands yields [Command]
-- etc.)
%%
Program		: var Declarations begin Commands end				{Program (reverse $2) (reverse $4)}

Declarations	: Declarations pidentifier					{Scalar (fst $2) (snd $2) : $1}
		| Declarations pidentifier '[' num ']'				{Array (fst $2) $4 (snd $2) : $1}
		| {- empty -}							{[]}

Commands	: Commands Command						{$2 : $1}
		| Command							{[$1]}

Command		: Identifier ":=" Expression ';'				{Asgn $1 $3}

		| if Condition then Commands else Commands endif		{If $2 (reverse $4) (reverse $6)}
		| while Condition do Commands endwhile				{While $2 (reverse $4)}
		| for pidentifier from Value to Value do Commands endfor	{ForUp (fst $2) $4 $6 (reverse $8)}
		| for pidentifier from Value downto Value do Commands endfor	{ForDown (fst $2) $4 $6 (reverse $8)}
		| read Identifier ';'						{Read $2}
		| write Value ';'						{Write $2}
		| skip ';'							{Skip}

Expression	: Value								{Value $1}
		| Value '+' Value						{Plus $1 $3}
		| Value '-' Value						{Minus $1 $3}
		| Value '*' Value						{Mul $1 $3}
		| Value '/' Value						{Div $1 $3}
		| Value '%' Value						{Mod $1 $3}

Condition	: Value '=' Value						{Eq $1 $3}
		| Value "<>" Value						{Neq $1 $3}
		| Value '<' Value						{Lt $1 $3}
		| Value '>' Value						{Gt $1 $3}
		| Value "<=" Value						{Le $1 $3}
		| Value ">=" Value						{Ge $1 $3}

Value		: num								{Num $1}
		| Identifier							{Identifier $1}

Identifier	: pidentifier							{Pidentifier (fst $1) (snd $1)}
		| pidentifier '[' pidentifier ']'				{ArrayPidentifier (fst $1) (fst $3) (snd $1) (snd $3)}
		| pidentifier '[' num ']'					{ArrayNum (fst $1) $3 (snd $1)}
{
parseError :: [TokWrap] -> a
parseError [] = error ("Unknown parse error.")
parseError ((TW tok (AlexPn _ line col)):_) = error ("Parse error in line " ++ (show line) ++ ", column " ++ (show col)) -- TODO: maybe improve?
}
