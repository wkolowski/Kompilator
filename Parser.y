{
module Main (main, parse) where

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

-- Identifiers are either just names of scalars (Pidentifier, e. g. name), names of arrays
-- indexed by names of scalars (ArrayPidentifier, e. g. arr[i]) or names of arrays indexed
-- by numerical constacts (ArrayNum, e. g. arr[5]).
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
nameOfIdent (Pidentifier name) = name
nameOfIdent (ArrayPidentifier name _) = name
nameOfIdent (ArrayNum name _) = name

-- Variable state can be either undeclared, uninitialized, initialized (but
-- without known value) or initialized with a known value.
-- Undeclared is represented by Nothing when looking the variable up in the
-- context.
data VarState = Uninitialized | Initialized | HasValue Integer deriving (Eq)

type Name = String
type Size = Integer

-- Context keeps track of declared variables. It also knows whether
-- the variable has been initialized.
data Context = Context {scalars :: Map.Map Name VarState, arrays :: Map.Map Name (Size, Map.Map Integer VarState)}

emptyContext = Context {scalars = Map.empty, arrays = Map.empty}

-- Adds a new scalar variable to the context.
newScalar :: String -> Context -> Context
newScalar name ctx = ctx {scalars = Map.insert name Uninitialized (scalars ctx)}

-- Adds a new array variable to the context.
newArray :: String -> Size -> Context -> Context
newArray name size ctx = ctx {arrays = Map.insert name (size, Map.fromList (zip [0..size - 1] (repeat Uninitialized))) (arrays ctx)}

-- Checks whether a scalar was already declared.
scalarDeclared :: String -> Context -> Bool
scalarDeclared name ctx = isJust $ Map.lookup name (scalars ctx)

arrayDeclared :: String -> Context -> Bool
arrayDeclared name ctx = isJust $ Map.lookup name (arrays ctx)

scalarInitialized :: String -> Context -> Bool
scalarInitialized name ctx = case Map.lookup name (scalars ctx) of
	Nothing -> False
	Just Uninitialized -> False
	_ -> True

initScalar :: String -> Context -> Context
initScalar name ctx = case Map.lookup name (scalars ctx) of
	Nothing -> error "Tried to initialize undeclared scalar!"
	Just vst -> case vst of
		Uninitialized -> ctx {scalars = Map.insert name Initialized (scalars ctx)}
		_ -> ctx


-- Checks whether a variable has been declared.
--boundIn :: String -> Context -> Bool
--name `boundIn` ctx = scalarDeclared name ctx || arrayDeclared name ctx

-- Checks whether an identifier has been initialized.
isInitialized :: VarState -> Bool
isInitialized vst = case vst of
	Uninitialized -> False
	_ -> True

isInitializedIn :: Identifier -> Context -> Bool
isInitializedIn id ctx = case id of
	Pidentifier name -> fmap isInitialized (Map.lookup name (scalars ctx)) == Just True
	ArrayNum name index -> case Map.lookup name (arrays ctx) of
		Nothing -> False
		Just (size, array) -> 0 <= index && index < size && fmap isInitialized (Map.lookup index array) == Just True
	ArrayPidentifier name indexName -> case Map.lookup indexName (scalars ctx) of
		Nothing -> False
		Just Uninitialized -> False
		Just Initialized -> True {- Watch out! -}
		Just (HasValue n) -> ArrayNum name n `isInitializedIn` ctx

evalVarState :: VarState -> Maybe Integer
evalVarState vst = case vst of
	HasValue n -> Just n
	_ -> Nothing

evalIdentifier :: Identifier -> Context -> Maybe Integer
evalIdentifier id ctx = case id of
	Pidentifier name -> do
		vst <- Map.lookup name (scalars ctx)
		evalVarState vst
	ArrayNum name index -> do
		(size, array) <- Map.lookup name (arrays ctx)
		vst <- Map.lookup index array
		evalVarState vst
	ArrayPidentifier name indexName -> do
		index <- evalIdentifier (Pidentifier indexName) ctx
		evalIdentifier (ArrayNum name index) ctx

evalValue :: Value -> Context -> Maybe Integer
evalValue (Num n) _ = Just n
evalValue (Identifier id) ctx = evalIdentifier id ctx

eval :: Expression -> Context -> Maybe Integer
eval (Value v) ctx = evalValue v ctx
eval (Plus e1 e2) ctx = liftM2 (+) (evalValue e1 ctx) (evalValue e2 ctx)
eval (Minus e1 e2) ctx = liftM2 (-) (evalValue e1 ctx) (evalValue e2 ctx)
eval (Mul e1 e2) ctx = liftM2 (*) (evalValue e1 ctx) (evalValue e2 ctx)
eval (Div e1 e2) ctx = liftM2 (div) (evalValue e1 ctx) (evalValue e2 ctx)
eval (Mod e1 e2) ctx = liftM2 (mod) (evalValue e1 ctx) (evalValue e2 ctx)

data IdentState = Undeclared | NotAScalar | NotAnArray | IndexUndeclared | IndexUninitialized | IndexOutOfBounds Size | IndexInitialized | IsVar VarState deriving (Eq)

identState :: Identifier -> Context -> IdentState
identState id ctx = case id of
	Pidentifier name -> case Map.lookup name (scalars ctx) of
		Nothing -> case Map.lookup name (arrays ctx) of
			Nothing -> Undeclared
			_ -> NotAScalar
		Just vst -> IsVar vst
	ArrayNum name index -> case Map.lookup name (arrays ctx) of
		Nothing -> case Map.lookup name (scalars ctx) of
			Nothing -> Undeclared
			_ -> NotAnArray
		Just (size, array) -> if not $ 0 <= index && index < size
			then IndexOutOfBounds size
			else case Map.lookup index array of
				Nothing -> error "This shouldn't happen"
				Just vst -> IsVar vst
	ArrayPidentifier name indexName -> case Map.lookup indexName (scalars ctx) of
		Nothing -> IndexUndeclared
		Just vst -> case vst of
			Uninitialized -> IndexUninitialized
			Initialized -> IndexInitialized
			HasValue index -> identState (ArrayNum name index) ctx
			 
handleIdentError :: Identifier -> String -> String -> Context -> (Identifier, Context)
handleIdentError id str1 str3 ctx = case identState id ctx of
	Undeclared -> error ("Undeclared variable: " ++ str1)
	NotAScalar -> error (str1 ++ " is not a scalar!")
	NotAnArray -> error (str1 ++ " is not an array!")
	IndexUndeclared -> error ("Undeclared variable: " ++ str3)
	IndexUninitialized -> error ("Index " ++ str3 ++ " of " ++ (str1 ++ "[" ++ str3 ++ "]") ++ " not initialized!")
	IndexOutOfBounds size -> error ("Index " ++ str3 ++ " out of bounds 0-" ++ (show size) ++ ".")
	_ -> (id, ctx)

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
	pidentifier		{TId $$ _}

%%
Program :: {State Context Program}
Program		: var Declarations begin Commands end				{liftM2 Program (fmap reverse $2) (fmap reverse $4)}

Declarations :: {State Context [Declaration]}
Declarations	: Declarations pidentifier					{do decls <- $1; ctx <- get; if scalarDeclared $2 ctx
											then error ("Variable named " ++ (show $2) ++ " already used!")
											else do put $ newScalar $2 ctx; return $ Scalar $2 : decls}
		| Declarations pidentifier '[' num ']'				{do decls <- $1; ctx <- get; if arrayDeclared $2 ctx
											then error ("Variable named " ++ (show $2) ++ " already used!")
											else do put $ newArray $2 $4 ctx; return $ Array $2 $4 : decls}
		| {- empty -}							{return []}

Commands :: {State Context [Command]}
Commands	: Commands Command						{liftM2 (:) $2 $1}
		| Command							{fmap return $1}

Command :: {State Context Command}
Command		: Identifier ":=" Expression ';' 				{liftM2 Asgn $1 $3}

		| if Condition then Commands else Commands endif		{liftM3 If $2 (fmap reverse $4) (fmap reverse $6)}
		| while Condition do Commands endwhile				{liftM2 While $2 (fmap reverse $4)}
		| for pidentifier from Value to Value do Commands endfor	{liftM4 ForUp (return $2) $4 $6 (fmap reverse $8)}
		| for pidentifier from Value downto Value do Commands endfor	{liftM4 ForDown (return $2) $4 $6 (fmap reverse $8)}
		| read Identifier ';'						{liftM Read $2} {-{state $ \ctx -> case $2 of
											Pidentifier name -> -}
		| write Value ';'						{do v <- $2; ctx <- get; case v of
											Num n -> do return $ Write v
											Identifier ident -> case id evalValue v ctx of
												Nothing -> error (show ident ++ " is uninitialized!")
												_ -> do return $ Write v}

		| skip ';'							{return Skip}

Expression :: {State Context Expression}
Expression	: Value								{liftM Value $1}
		| Value '+' Value						{liftM2 Plus $1 $3}
		| Value '-' Value						{liftM2 Minus $1 $3}
		| Value '*' Value						{liftM2 Mul $1 $3}
		| Value '/' Value						{do v <- $1; v' <- $3; ctx <- get; if evalValue v' ctx == Just 0
											then error "Division by zero!"
											else do return $ Div v v'}
		| Value '%' Value						{do v <- $1; v' <- $3; ctx <- get; if evalValue v' ctx == Just 0
											then error "Modulo division by zero!"
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
Identifier	: pidentifier							{state $ \ctx -> handleIdentError (Pidentifier $1) $1 "" ctx}
		| pidentifier '[' pidentifier ']'				{state $ \ctx -> handleIdentError (ArrayPidentifier $1 $3) $1 $3 ctx}
		| pidentifier '[' num ']'					{state $ \ctx -> handleIdentError (ArrayNum $1 $3) $1 (show $3) ctx}
{
parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

main = getContents >>= print . (\tokens -> evalState (parse tokens) emptyContext) . alexScanTokens . filter (\c -> 0 <= fromEnum c && fromEnum c < 128)
}
