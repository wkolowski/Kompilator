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
	show (ArrayPidentifier name indexName) = name ++ "[" ++ indexName ++ "]"
	show (ArrayNum name index) = name ++ "[" ++ (show index) ++ "]"

-- Variable state can be either undeclared, uninitialized, initialized (but
-- without known value) or initialized with a known value.
-- Undeclared is represented by Nothing when looking the variable up in the
-- context.
data VarState = Uninitialized | Initialized | HasValue Integer deriving (Eq, Show)

type Name = String
type Size = Integer
type Index = Integer

-- Context keeps track of declared variables. It also knows whether
-- the variable has been initialized.
data Context = Context {scalars :: Map.Map Name VarState, arrays :: Map.Map Name (Size, Map.Map Integer VarState), iterators :: Map.Map Name VarState}
	deriving (Eq, Show)

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

getScalarOrIterMay :: String -> Context -> Maybe VarState
getScalarOrIterMay name ctx = case Map.lookup name (iterators ctx) of
	Nothing -> Map.lookup name (scalars ctx)
	Just vst -> Just vst

verifyScalarOrIter :: (Name, AlexPosn) -> Context -> (Identifier, Context)
verifyScalarOrIter (name, pos) ctx
	| arrayDeclared name ctx = err pos $ show name ++ " is not a scalar."
	| not (scalarDeclared name ctx) && not (iteratorDeclared name ctx) = err pos $ "Undeclared variable " ++ (show name) ++ "."
	| otherwise = (Pidentifier name, ctx)

verifyArrayNum :: (Name, AlexPosn) -> Integer -> Context -> (Identifier, Context)
verifyArrayNum (name, pos) index ctx
	| scalarDeclared name ctx || iteratorDeclared name ctx = err pos $ show name ++ " is not an array."
	| not (arrayDeclared name ctx) = err pos $ "Undeclared variable " ++ (show name) ++ "."
	| otherwise = case Map.lookup name (arrays ctx) of
		Nothing -> err pos $ "Undeclared variable " ++ (show name) ++ "."
		Just (size, array) -> if 0 <= index && index < size
			then (ArrayNum name index, ctx)
			else err pos$ "Index " ++ (show index) ++ " out of bounds 0-" ++ (show $ size - 1) ++ "."

verifyArrayPidentifier :: (Name, AlexPosn) -> (Name, AlexPosn) -> Context -> (Identifier, Context)
verifyArrayPidentifier (name, pos) (indexName, indexPos) ctx = case Map.lookup name (arrays ctx) of
	Nothing -> if scalarDeclared name ctx || iteratorDeclared name ctx
		then err pos $ show name ++ " is not an array."
		else err pos $ "Undeclared variable " ++ (show name) ++ "."

	Just (size, array) -> case getScalarOrIterMay indexName ctx of
		Nothing -> if arrayDeclared indexName ctx
			then err indexPos $ "The index " ++ (show indexName) ++ " is not a scalar."
			else err indexPos $ "Undeclared variable " ++ (show indexName) ++ "."

		Just Uninitialized -> err indexPos $ "Cannot reference array " ++ (show name) ++ " with uninitialized index " ++ (show indexName) ++ "."
		Just Initialized -> (ArrayPidentifier name indexName, ctx)
		Just (HasValue index) -> if 0 <= index && index < size
			then (ArrayPidentifier name indexName, ctx)
			else err indexPos $ "Index " ++ (show indexName) ++ " out of bounds 0-" ++ (show $ size - 1) ++ "."

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

err :: AlexPosn -> String -> a
err (AlexPn _ line col) msg = error $ "Error in line " ++ (show line) ++ ", column " ++ (show col) ++ ": " ++ msg

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
Program :: {State Context Program}
Program		: var Declarations begin Commands end				{liftM2 Program (fmap reverse $2) (fmap reverse $4)}

Declarations :: {State Context [Declaration]}
Declarations	: Declarations pidentifier					{do decls <- $1; return $ Scalar (fst $2) : decls}{--{do decls <- $1; ctx <- get; let (id, pos) = $2 in
											if scalarDeclared id ctx
											then err pos $ "Variable named " ++ (show id) ++ " already declared."
											else do put $ newScalar id ctx; return $ Scalar id : decls}--}
		| Declarations pidentifier '[' num ']'				{do decls <- $1; ctx <- get; let (id, pos) = $2 in
											if arrayDeclared id ctx
											then err pos $ "Variable named " ++ (show id) ++ " already declared."
											else do put $ newArray id $4 ctx; return $ Array id $4 : decls}
		| {- empty -}							{return []}

Commands :: {State Context [Command]}
Commands	: Commands Command						{liftM2 (:) $2 $1}
		| Command							{fmap return $1}

Command :: {State Context Command}
Command		: Identifier ":=" Expression ';'				{liftM2 Asgn $1 $3}

		| if Condition then Commands else Commands endif		{liftM3 If $2 (fmap reverse $4) (fmap reverse $6)}
		| while Condition do Commands endwhile				{liftM2 While $2 (fmap reverse $4)}
		| for pidentifier from Value to Value do Commands endfor	{let (id, _) = $2 in liftM4 ForUp (return id) $4 $6 (fmap reverse $8)}
		| for pidentifier from Value downto Value do Commands endfor	{let (id, _) = $2 in liftM4 ForDown (return id) $4 $6 (fmap reverse $8)}
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
Identifier	: pidentifier							{return $ Pidentifier (fst $1)} {-{state $ \ctx -> verifyScalarOrIter $1 ctx}-}
		| pidentifier '[' pidentifier ']'				{return $ ArrayPidentifier (fst $1) (fst $3)} {-state $ \ctx -> verifyArrayPidentifier $1 $3 ctx}-}
		| pidentifier '[' num ']'					{return $ ArrayNum (fst $1) $3} {-state $ \ctx -> verifyArrayNum $1 $3 ctx}-}
{
parseError :: [TokWrap] -> a
parseError [] = error ("Unknown parse error.")
parseError ((TW tok (AlexPn _ line col)):_) = error ("Parse error in line " ++ (show line) ++ ", column " ++ (show col))
}
