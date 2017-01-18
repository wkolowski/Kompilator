{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
module StaticAnalyzer where

import Control.Monad.State
import Control.Applicative
import qualified Data.Map.Strict as Map
import Data.Maybe

import Lexer
import Parser

analyze :: Program -> State Context Program
analyze (Program decls cmds) = do
	decls' <- analyzeDeclarations decls
	ctx <- get
	cmds' <- analyzeCommands cmds
	return $ Program (decls' `seq` decls') cmds'

analyzeDeclarations :: [Declaration] -> State Context [Declaration]
analyzeDeclarations decls = case decls of
	[] -> return []
	decl:decls -> do
		ctx <- get
		put $ declareVar decl ctx
		decls' <- analyzeDeclarations decls
		return $ decl:decls'
		liftM2 (:) (return decl) (analyzeDeclarations decls)

declareVar :: Declaration -> Context -> Context
declareVar decl ctx = case decl of
	Scalar name -> declareScalar name ctx
	Array name size -> declareArray name size ctx

declareScalar :: Name -> Context -> Context
declareScalar name ctx
	| arrayDeclared name ctx = error $ "Variable " ++ name ++ " already declared as an array."
	| scalarDeclared name ctx = error $ "Variable " ++ name ++ " already declared."
	| otherwise = newScalar name ctx

declareArray :: Name -> Size -> Context -> Context
declareArray name size ctx
	| scalarDeclared name ctx = error $ "Variable " ++ name ++ " already declared as a scalar."
	| arrayDeclared name ctx = error $ "Variable " ++ name ++ " already declared."
	| otherwise = newArray name size ctx

analyzeCommands :: [Command] -> State Context [Command]
analyzeCommands cmds = case cmds of
	[] -> return []
	cmd@Skip:cmds' -> liftM2 (:) (return cmd) (analyzeCommands cmds')
	cmd@(Read id):cmds' -> do
		ctx <- get
		
		
		put $! readIdentifier id $! ctx
		--let !ctx' = readIdentifier id ctx
		liftM2 (:) (return cmd) (analyzeCommands cmds')
	cmd@(Write val):cmds' -> case val of
		Num n -> liftM2 (:) (return cmd) (analyzeCommands cmds')
		Identifier id -> do
			ctx <- get
			if not $ writable id ctx 
			then error $ "Unknown error"
			else liftM2 (:) (return cmd) (analyzeCommands cmds')
	_ -> return cmds

readIdentifier :: Identifier -> Context -> Context
readIdentifier id ctx = case id of
	Pidentifier name -> readPidentifier name ctx
	ArrayNum name index -> readArrayNum name index ctx
	ArrayPidentifier name indexName -> readArrayPidentifier name indexName ctx

readPidentifier :: Name -> Context -> Context
readPidentifier name ctx
	| arrayDeclared name ctx = error $ "Trying to use variable " ++ name ++ " as a scalar, but it was declared an array."
	| not $ scalarDeclared name ctx = error $ "Undeclared variable " ++ name ++ "."
	| otherwise = error "Just for tej lulz" -- ctx {scalars = Map.insert name Initialized (scalars ctx)}

readArrayNum :: Name -> Index -> Context -> Context
readArrayNum = initializeArrayNum

initializeArrayNum :: Name -> Index -> Context -> Context
initializeArrayNum name index ctx
	| scalarDeclared name ctx = error $ "Trying to use variable " ++ name ++ " as an array, but it was declared a scalar."
	| otherwise = case Map.lookup name (arrays ctx) of
		Nothing -> error $ "Undeclared variable " ++ name ++ "."
		Just (size, array) ->
			if not $ 0 <= index && index < size
			then error $ "Index " ++ (show index) ++ " out of bounds 0-" ++ show (size - 1) ++ " in expression " ++ name ++ "[" ++ (show index) ++ "]."
			else ctx {arrays = Map.insert name (size, Map.insert index Initialized array) (arrays ctx)}

readArrayPidentifier :: Name -> Name -> Context -> Context
readArrayPidentifier name indexName ctx
	| scalarDeclared name ctx = error $ "Trying to use variable " ++ name ++ " as an array, but it was declared a scalar."
	| otherwise = case Map.lookup name (arrays ctx) of
		Nothing -> error $ "Undeclared variable " ++ name ++ "."
		Just (size, array) ->
			if arrayDeclared indexName ctx
			then error $ "Trying to use variable " ++ indexName ++ " as a scalar, but it was declared an array."
			else case getScalarOrIter indexName ctx of {-- indexName ctx of
				Nothing -> error $ "Undeclared variable " ++ name ++ "."
				Just vst -> case vst of -}
				Uninitialized -> error $ "Tried to use uninitialized variable " ++ indexName ++ " as an index in " ++ name ++ "[" ++ indexName ++ "]."
				HasValue index -> initializeArrayNum name index ctx
				Initialized -> ctx {arrays = Map.insert name (size, Map.fromList (zip [0..size - 1] (repeat Initialized))) (arrays ctx)}

writable :: Identifier -> Context -> Bool
writable id = case id of
	Pidentifier name -> writablePidentifier name
	ArrayNum name index -> writableArrayNum name index
	ArrayPidentifier name indexName -> const True -- FAKE : writableArrayPidentifier name indexName 

writablePidentifier :: Name -> Context -> Bool
writablePidentifier name ctx = if scalarOrIterInitialized name ctx then True else error $ "Unknown error in writablePidentifier"

writableArrayNum :: Name -> Index -> Context -> Bool
writableArrayNum name index ctx = if arrayElementInitialized name index ctx then True else error $ "Unknown error in writableArrayNum"

--writableArrayPidentifier :: Name -> Name -> Context -> Either String Bool
--writableArrayPidentifier name indexName ctx = if



isDeclared' :: Name -> Context -> Bool
isDeclared' name ctx =
	if isJust (Map.lookup name (scalars ctx)) || isJust (Map.lookup name (arrays ctx)) || isJust (Map.lookup name (iterators ctx))
	then True
	else error $ "Undeclared variable " ++ name ++ "."

data VarType = VTScalar | VTArray | VTIter

isScalar' :: Name -> Context -> Bool
isScalar' name ctx =
	if isJust $ Map.lookup name (arrays ctx)
	then error $ "Tried to use variable " ++ name ++ " as a scalar, but it was declared an array."
	else isDeclared' name ctx

{--isInitialized' :: Name -> Context -> Bool
isInitialized' name ctx = case Map.lookup name (iterators ctx) of
	Nothing -> case Map.lookup name (scalars ctx) of
		Nothing -> case Map.lookup name (arrays ctx) of
			Nothing -> isDeclared' name ctx
			Just Uninitialized -> error $ "Tried to use uninitialized variable " ++ name ++ "."
			_ -> True
		Just Uninitialized -> error $ "Tried to use uninitialized variable " ++ name ++ "."
		_ -> True
	Just Uninitialized -> error $ "Tried to use uninitialized variable " ++ name ++ "."
	_ -> True-}

getVarType :: Name -> Context -> VarType
getVarType name ctx =
	if isJust $ Map.lookup name (scalars ctx) then VTScalar
	else if isJust $ Map.lookup name (arrays ctx) then VTArray
	else if isJust $ Map.lookup name (iterators ctx) then VTIter
	else error $ "Undeclared variable " ++ name ++ "."

getScalarOrIter :: Name -> Context -> VarState
getScalarOrIter name ctx = case getVarType name ctx of
	VTArray -> error $ "Tried to use variable " ++ name ++ " as a scalar, but it was declared an array."
	VTScalar -> case Map.lookup name (scalars ctx) of
		Nothing -> error $ "Unknown error in getScalarOrIter, VTScalar"
		Just vst -> vst
	VTIter -> case Map.lookup name (iterators ctx) of
		Nothing -> error $ "Unknown error in getScalarOrIter, VTIter"
		Just vst -> vst

scalarOrIterInitialized :: Name -> Context -> Bool
scalarOrIterInitialized name ctx = case getScalarOrIter name ctx of
	Uninitialized -> error $ "Tried to use uninitialized variable " ++ name ++ "."
	_ -> True

getArrayElement :: Name -> Index -> Context -> VarState
getArrayElement name index ctx = case getVarType name ctx of
	VTScalar -> error $ "Tried to use variable " ++ name ++ " as an array, but it was declared a scalar."
	VTIter -> error $ "Tried to use variable " ++ name ++ " as an array, but it was declared a scalar."
	VTArray -> case Map.lookup name (arrays ctx) of
		Nothing -> error $ "Unknown error in getArrayElement, VTArray"
		Just (size, array) ->
			if not $ 0 <= index && index < size
			then error $ "Index " ++ (show index) ++ " out of bounds 0-" ++ show (size - 1) ++ " in expression " ++ name ++ "[" ++ (show index) ++ "]."
			else case Map.lookup index array of
				Nothing -> error $ "Unknown error in getArrayElement, looking for index value"
				Just vst -> vst

arrayElementInitialized :: Name -> Index -> Context -> Bool
arrayElementInitialized name index ctx = case getArrayElement name index ctx of
	Uninitialized -> error $ "Tried to use uninitialized variable " ++ name ++ "[" ++ (show index) ++ "]."
	_ -> True
