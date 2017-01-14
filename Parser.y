{
module Main (main, parse) where

import Control.Monad.State

import Lexer

type Context = [(Declaration, Maybe Int)]

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
	deriving (Show, Eq)

data Identifier
	= Pidentifier String
	| ArrayPidentifier String String
	| ArrayNum String Integer
	deriving (Eq)

instance Show Identifier where
	show (Pidentifier name) = name
	show (ArrayPidentifier name index) = name ++ "[" ++ index ++ "]"
	show (ArrayNum name index) = name ++ "[" ++ (show index) ++ "]"

nameOfIdent :: Identifier -> String
nameOfIdent (Pidentifier str) = str
nameOfIdent (ArrayPidentifier str _) = str
nameOfIdent (ArrayNum str _) = str


boundIn :: String -> Context -> Bool
var `boundIn` [] = False
var `boundIn` ((decl, _):decls) = case decl of
	Scalar var' -> var == var' || var `boundIn` decls
	Array var' _ -> var == var' || var `boundIn` decls

correctAsgn :: Identifier -> Context -> Bool
correctAsgn _ [] = False
correctAsgn id ((Scalar name, _):decls) = case id of
	Pidentifier name' -> name == name' || correctAsgn id decls
	_ -> correctAsgn id decls
correctAsgn id ((Array name _, _):decls) = case id of
	ArrayPidentifier name' _ -> name == name' || correctAsgn id decls
	ArrayNum name' _ -> name == name' || correctAsgn id decls
	_ -> correctAsgn id decls

--isInitializedIn :: Identifier -> Context -> Bool
--isInitializedIn _ [] = False
--isInitializedIn id ((decl,
{-
correctAsgn id@(Pidentifier name) ((decl, _):decls) = case decl of
	Scalar name' -> name == name' || correctAsgn id decls
	_ -> correctAsgn id decls
correctAsgn id@(ArrayPidtentifier name _) ((decl, _):decls) = case decl of
	ArrayPidentifier name' _ -> name == name' || correctAsgn id decls
	ArrayNum name' _ -> name == name' || correctAsgn id decls
	_ -> correctAsgn id decls
correctAsgn id@(ArrayNum name _) ((decl, _):decls) = case decl of
	ArrayPidentifier name' _ -> name  
-}

}

%name parse
-- %monad {State Context}
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
Program :: {State Context Program}
Program		: var Declarations begin Commands end				{liftM2 Program (fmap reverse $2) (fmap reverse $4)} --{do decls <- $2; cmds <- $4; return $ Program (reverse decls) (reverse cmds)}

Declarations :: {State Context [Declaration]}
Declarations	: Declarations pidentifier					{do decls <- $1; ctx <- get; if $2 `boundIn` ctx
											then error ("Variable named " ++ (show $2) ++ " already used!")
											else do put $ (Scalar $2, Nothing) : ctx; return $ Scalar $2 : decls}
		| Declarations pidentifier '[' num ']'				{do decls <- $1; ctx <- get; if $2 `boundIn` ctx
											then error ("Variable named " ++ (show $2) ++ " already used!")
											else do put $ (Array $2 $4, Nothing) : ctx; return $ Array $2 $4 : decls}
		| {- empty -}							{return []}


-- Fake commands
--Commands : {- empty -}									{return []}

Commands :: {State Context [Command]}
Commands	: Commands Command						{liftM2 (:) $2 $1} --{do cmds <- $1; cmd <- $2; return $ cmd : cmds} WATCH OUT
		| Command							{fmap return $1} --{do cmd <- $1; return [cmd]}

Command :: {State Context Command}
Command		: Identifier ":=" Expression ';' 				{liftM2 Asgn $1 $3} {-{do id <- $1; expr <- $3; ctx <- get; return $ Asgn id expr} {case expr of
	Value (Identifier name) -> if not $ name `boundIn` ctx
		then error ("Right hand side not declared: " ++ (show id))
		else if correctAsgn (Identifier name) ctx
			then do put -} 

		| if Condition then Commands else Commands endif		{liftM3 If $2 (fmap reverse $4) (fmap reverse $6)}
		| while Condition do Commands endwhile				{liftM2 While $2 (fmap reverse $4)}
		| for pidentifier from Value to Value do Commands endfor	{liftM4 ForUp (return $2) $4 $6 (fmap reverse $8)}
		| for pidentifier from Value downto Value do Commands endfor	{liftM4 ForDown (return $2) $4 $6 (fmap reverse $8)}
		| read Identifier ';'						{liftM Read $2}
		| write Value ';'						{liftM Write $2} {-{do v <- $2; ctx <- get; case v of
											Num n -> return $ Write v
											Identifier id -> if id `isInitializedIn` ctx
												then do return $ Write v
												else error (show id ++ " is uninitialized!")}-}

		| skip ';'							{return Skip}

Expression :: {State Context Expression}
Expression	: Value								{liftM Value $1}
		| Value '+' Value						{liftM2 Plus $1 $3}
		| Value '-' Value						{liftM2 Minus $1 $3}
		| Value '*' Value						{liftM2 Mul $1 $3}
		| Value '/' Value						{do v <- $1; v' <- $3; if v' == Num 0
											then error "Division by zero!"
											else do return $ Div v v'}
		| Value '%' Value						{do v <- $1; v' <- $3; if v' == Num 0
											then error "Division by zero!"
											else do return $ Mod v v'}
Condition :: {State Context Condition}
Condition	: Value '=' Value						{liftM2 Eq $1 $3}
		| Value "<>" Value						{liftM2 Neq $1 $3}
		| Value '<' Value						{liftM2 Lt $1 $3}
		| Value '>' Value						{liftM2 Gt $1 $3}
		| Value "<=" Value						{liftM2 Le $1 $3}
		| Value ">=" Value						{liftM2 Ge $1 $3}

Value :: {State Context Value}
Value		: num								{return $ Num $1}
		| Identifier							{liftM Identifier $1}

Identifier :: {State Context Identifier}
Identifier	: pidentifier							{state $ \ctx -> if $1 `boundIn` ctx
											then (Pidentifier $1, ctx)
											else error ("Undeclared variable: " ++ (show $1))}
		| pidentifier '[' pidentifier ']'				{state $ \ctx -> if $1 `boundIn` ctx
											then if $3 `boundIn` ctx
												then (ArrayPidentifier $1 $3, ctx)
												else error ("Unknown variable: " ++ (show $3))
											else error ("Undeclared variable: " ++ (show $1))}
		| pidentifier '[' num ']'					{state $ \ctx -> if $1 `boundIn` ctx
											then (ArrayNum $1 $3, ctx)
											else error ("Undeclared variable: " ++ (show $1))}

{
--parseError :: [Token] -> a
parseError _ = error "Errur wihle parsink"

main = getContents >>= print . (\tokens -> evalState (parse tokens) []) . alexScanTokens
}
