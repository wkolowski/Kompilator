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
data Context = Context {scalars :: Map.Map Name VarState, arrays :: Map.Map Name (Size, Map.Map Integer VarState), iterators :: Map.Map Name VarState}

emptyContext = Context {scalars = Map.empty, arrays = Map.empty, iterators = Map.empty}

-- Adds a new scalar variable to the context.
newScalar :: String -> Context -> Context
newScalar name ctx = ctx {scalars = Map.insert name Uninitialized (scalars ctx)}

-- Adds a new array variable to the context.
newArray :: String -> Size -> Context -> Context
newArray name size ctx = ctx {arrays = Map.insert name (size, Map.fromList (zip [0..size - 1] (repeat Uninitialized))) (arrays ctx)}

-- Adds a new iterator variable to the context.
addIterator :: String -> Context -> Context
addIterator name ctx = ctx {iterators = Map.insert name Initialized (iterators ctx)}

-- Checks whether a scalar was already declared.
scalarDeclared :: String -> Context -> Bool
scalarDeclared name ctx = isJust $ Map.lookup name (scalars ctx)

arrayDeclared :: String -> Context -> Bool
arrayDeclared name ctx = isJust $ Map.lookup name (arrays ctx)

iteratorDeclared :: String -> Context -> Bool
iteratorDeclared name ctx = isJust $ Map.lookup name (iterators ctx)

isDeclared :: String -> Context -> Bool
isDeclared name ctx = scalarDeclared name ctx || arrayDeclared name ctx || iteratorDeclared name ctx

getScalarOrIter :: String -> Context -> Maybe VarState
getScalarOrIter name ctx = case Map.lookup name (iterators ctx) of
	Nothing -> Map.lookup name (scalars ctx)
	Just vst -> Just vst

verifyScalarOrIter :: String -> Context -> (Identifier, Context)
verifyScalarOrIter name ctx
	| arrayDeclared name ctx = error $ show name ++ " is not a scalar."
	| not (scalarDeclared name ctx) && not (iteratorDeclared name ctx) = error $ "Undeclared variable " ++ (show name) ++ "."
	| otherwise = (Pidentifier name, ctx)

verifyArrayNum :: String -> Integer -> Context -> (Identifier, Context)
verifyArrayNum name index ctx
	| scalarDeclared name ctx || iteratorDeclared name ctx = error $ show name ++ " is not an array."
	| not (arrayDeclared name ctx) = error $ "Undeclared variable " ++ (show name) ++ "."
	| otherwise = case Map.lookup name (arrays ctx) of
		Nothing -> error $ "Undeclared variable " ++ (show name) ++ "."
		Just (size, array) -> if 0 <= index && index < size
			then (ArrayNum name index, ctx)
			else error $ "Index " ++ (show index) ++ " out of bounds 0-" ++ (show $ size - 1) ++ "."

verifyArrayPidentifier :: String -> String -> Context -> (Identifier, Context)
verifyArrayPidentifier name indexName ctx = case Map.lookup name (arrays ctx) of
	Nothing -> if scalarDeclared name ctx || iteratorDeclared name ctx
		then error $ show name ++ " is not an array."
		else error $ "Undeclared variable " ++ (show name) ++ "."

	Just (size, array) -> case getScalarOrIter indexName ctx of
		Nothing -> if arrayDeclared indexName ctx
			then error $ "The index " ++ (show indexName) ++ " is not a scalar."
			else error $ "Undeclared variable " ++ (show indexName) ++ "."

		Just Uninitialized -> error $ "Cannot reference array " ++ (show name) ++ " with uninitialized index " ++ (show indexName) ++ "."
		Just Initialized -> (ArrayPidentifier name indexName, ctx)
		Just (HasValue index) -> if 0 <= index && index < size
			then (ArrayPidentifier name indexName, ctx)
			else error $ "Index " ++ (show indexName) ++ " out of bounds 0-" ++ (show $ size - 1) ++ "."

eval :: Expression -> Context -> Maybe Integer
eval (Value v) ctx = evalValue v ctx
eval (Plus e1 e2) ctx = liftM2 (+) (evalValue e1 ctx) (evalValue e2 ctx)
eval (Minus e1 e2) ctx = liftM2 (-) (evalValue e1 ctx) (evalValue e2 ctx)
eval (Mul e1 e2) ctx = liftM2 (*) (evalValue e1 ctx) (evalValue e2 ctx)
eval (Div e1 e2) ctx = liftM2 (div) (evalValue e1 ctx) (evalValue e2 ctx)
eval (Mod e1 e2) ctx = liftM2 (mod) (evalValue e1 ctx) (evalValue e2 ctx)

evalValue :: Value -> Context -> Maybe Integer
evalValue (Num n) _ = Just n
evalValue (Identifier id) ctx = evalIdentifier id ctx

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

evalVarState :: VarState -> Maybe Integer
evalVarState vst = case vst of
	HasValue n -> Just n
	_ -> Nothing

}

%name parse
%tokentype {TokWrap}
%error {parseError}

%token
	-- Keywords.
	var			{TW TVAR _}
	begin			{TW TBEGIN _}
	end			{TW TEND _}
	if			{TW TIF _}
	then			{TW TTHEN _}
	else			{TW TELSE _}
	endif			{TW TENDIF _}
	while			{TW TWHILE _}
	do			{TW TDO _}
	endwhile		{TW TENDWHILE _}
	for			{TW TFOR _}
	from			{TW TFROM _}
	to			{TW TTO _}
	endfor			{TW TENDFOR _}
	downto			{TW TDOWNTO _}
	read			{TW TREAD _}
	write			{TW TWRITE _}
	skip			{TW TSKIP _}

	-- Arithemtic operators.
	'+'			{TW TPlus _}
	'-'			{TW TMinus _}
	'*'			{TW TMul _}
	'/'			{TW TDiv _}
	'%'			{TW TMod _}

	-- Relational operators.
	'='			{TW TEq _}
	"<>"			{TW TNeq _}
	'<'			{TW TLt _}
	"<="			{TW TLe _}
	">="			{TW TGe _}
	'>'			{TW TGt _}

	-- Assingment and semicolon.
	":="			{TW TAsgn _}
	';'			{TW TSemicolon _}

	-- Parentheses and brackets.
	'('			{TW TLParen _}
	')'			{TW TRParen _}
	'['			{TW TLBracket _}
	']'			{TW TRBracket _}

	-- Numbers.
	num			{TW (TNum $$) _}

	-- Identifiers.
	pidentifier		{TW (TId $$) _}

%%
Program :: {State Context Program}
Program		: var Declarations begin Commands end				{liftM2 Program (fmap reverse $2) (fmap reverse $4)}

Declarations :: {State Context [Declaration]}
Declarations	: Declarations pidentifier					{do decls <- $1; ctx <- get; if scalarDeclared $2 ctx
											then error ("Variable named " ++ (show $2) ++ " already declared.")
											else do put $ newScalar $2 ctx; return $ Scalar $2 : decls}
		| Declarations pidentifier '[' num ']'				{do decls <- $1; ctx <- get; if arrayDeclared $2 ctx
											then error ("Variable named " ++ (show $2) ++ " already declared.")
											else do put $ newArray $2 $4 ctx; return $ Array $2 $4 : decls}
		| {- empty -}							{return []}

Commands :: {State Context [Command]}
Commands	: Commands Command						{liftM2 (:) $2 $1}
		| Command							{fmap return $1}

Command :: {State Context Command}
Command		: Identifier ":=" Expression ';'				{liftM2 Asgn $1 $3}

		| if Condition then Commands else Commands endif		{liftM3 If $2 (fmap reverse $4) (fmap reverse $6)}
		| while Condition do Commands endwhile				{liftM2 While $2 (fmap reverse $4)}
		| for pidentifier from Value to Value do Commands endfor	{liftM4 ForUp (return $2) $4 $6 (fmap reverse $8)}
		| for pidentifier from Value downto Value do Commands endfor	{liftM4 ForDown (return $2) $4 $6 (fmap reverse $8)}
		| read Identifier ';'						{liftM Read $2}
		| write Value ';'						{liftM Write $2} {-{do v <- $2; ctx <- get; case v of
											Num n -> do return $ Write v
											Identifier ident -> case id evalValue v ctx of
												Nothing -> error (show ident ++ " is uninitialized!")
												_ -> do return $ Write v}-}

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
Identifier	: pidentifier							{state $ \ctx -> verifyScalarOrIter $1 ctx}
		| pidentifier '[' pidentifier ']'				{state $ \ctx -> verifyArrayPidentifier $1 $3 ctx}
		| pidentifier '[' num ']'					{state $ \ctx -> verifyArrayNum $1 $3 ctx}
{
parseError :: [TokWrap] -> a
parseError [] = error ("Unknown parse error.")
parseError ((TW tok (AlexPn _ line col)):_) = error ("Parse error in line " ++ (show line) ++ ", column " ++ (show col))

--main = getContents >>= print . (\tokens -> evalState (parse tokens) emptyContext) . alexScanTokens . filter (\c -> 0 <= fromEnum c && fromEnum c < 128)
}
