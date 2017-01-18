module NewStaticAnalyzer where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.Trans

import Lexer
import Parser

type Error = String

analyze :: Program -> StateT Context (Either Error) Program
analyze (Program decls cmds) = liftM2 Program (analyzeDeclarations decls) (analyzeCommands cmds)

analyzeDeclarations :: [Declaration] -> StateT Context (Either Error) [Declaration]
analyzeDeclarations decls = case decls of
	[] -> return []
	decl:decls -> do
		ctx <- declareVar decl
		put ctx
		liftM2 (:) (return decl) (analyzeDeclarations decls)

declareVar :: Declaration -> StateT Context (Either Error) Context
declareVar decl = case decl of
	Scalar name -> declareScalar name
	Array name size -> declareArray name size

declareScalar :: Name -> StateT Context (Either Error) Context
declareScalar name = StateT $ \ctx -> 
	if arrayDeclared name ctx then Left $ "Variable " ++ name ++ " already declared as an array."
	else if scalarDeclared name ctx then Left $ "Variable " ++ name ++ " already declared."
	else Right $ (newScalar name ctx, ctx)

declareArray :: Name -> Size -> StateT Context (Either Error) Context
declareArray name size = StateT $ \ctx ->
	if scalarDeclared name ctx then Left $ "Variable " ++ name ++ " already declared as a scalar."
	else if arrayDeclared name ctx then Left $ "Variable " ++ name ++ " already declared."
	else Right $ (newArray name size ctx, ctx)

analyzeCommands :: [Command] -> StateT Context (Either Error) [Command]
analyzeCommands cmds = case cmds of
	[] -> return []
	cmd@Skip:cmds' -> liftM2 (:) (return cmd) (analyzeCommands cmds')
	cmd@(Read id):cmds' -> do
		ctx <- readIdentifier id
		put ctx
		liftM2 (:) (return cmd) (analyzeCommands cmds')
	{-cmd@(Write val):cmds' -> case val of
		Num n -> liftM2 (:) (return cmd) (analyzeCommands cmds')
		Identifier id -> do
			ctx <- get
			if not $ writable id ctx 
			then error $ "Unknown error"
			else liftM2 (:) (return cmd) (analyzeCommands cmds')-}
	_ -> return cmds

readIdentifier :: Identifier -> StateT Context (Either Error) Context
readIdentifier id = case id of
	Pidentifier name -> readPidentifier name
	ArrayNum name index -> readArrayNum name index
	ArrayPidentifier name indexName -> readArrayPidentifier name indexName

readPidentifier :: Name -> StateT Context (Either Error) Context
readPidentifier name = StateT $ \ctx ->
	if arrayDeclared name ctx then Left $ "Trying to use variable " ++ name ++ " as a scalar, but it was declared an array."
	else if not $ scalarDeclared name ctx then Left $ "Undeclared variable " ++ name ++ "."
	else Right (ctx {scalars = Map.insert name Initialized (scalars ctx)}, ctx)

readArrayNum :: Name -> Index -> StateT Context (Either Error) Context
readArrayNum name index = StateT $ \ctx ->
	if scalarDeclared name ctx then Left $ "Trying to use variable " ++ name ++ " as an array, but it was declared a scalar."
	else case Map.lookup name (arrays ctx) of
		Nothing -> Left $ "Undeclared variable " ++ name ++ "."
		Just (size, array) ->
			if not $ 0 <= index && index < size
			then Left $ "Index " ++ (show index) ++ " out of bounds 0-" ++ show (size - 1) ++ " in expression " ++ name ++ "[" ++ (show index) ++ "]."
			else Right (ctx {arrays = Map.insert name (size, Map.insert index Initialized array) (arrays ctx)}, ctx)

readArrayPidentifier :: Name -> Name -> StateT Context (Either Error) Context
readArrayPidentifier name indexName = do
	ctx <- get
	if scalarDeclared name ctx then StateT $ \_ -> Left $ "Trying to use variable " ++ name ++ " as an array, but it was declared a scalar."
	else case Map.lookup name (arrays ctx) of
		Nothing -> StateT $ \_ -> Left $ "Undeclared variable " ++ name ++ "."
		Just (size, array) ->
			if arrayDeclared indexName ctx
			then StateT $ \_ -> Left $ "Trying to use variable " ++ indexName ++ " as a scalar, but it was declared an array."
			else do
				vst <- getScalarOrIter indexName
				case vst of
					Uninitialized -> StateT $ \_ -> Left $ "Tried to use uninitialized variable " ++ indexName ++ " as an index in " ++ name ++ "[" ++ indexName ++ "]."
					HasValue index -> readArrayNum name index
					Initialized -> return $ ctx {arrays = Map.insert name (size, Map.fromList (zip [0..size - 1] (repeat Initialized))) (arrays ctx)}

{-
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

-}
data VarType = VTScalar | VTArray | VTIter

getVarType :: Name -> StateT Context (Either Error) VarType
getVarType name = StateT $ \ctx ->
	if isJust $ Map.lookup name (scalars ctx) then Right (VTScalar, ctx)
	else if isJust $ Map.lookup name (arrays ctx) then Right (VTArray, ctx)
	else if isJust $ Map.lookup name (iterators ctx) then Right (VTIter, ctx)
	else Left $ "Undeclared variable " ++ name ++ "."

getScalarOrIter :: Name -> StateT Context (Either Error) VarState
getScalarOrIter name = do
	vtype <- getVarType name
	ctx <- get
	StateT $ \_ -> case vtype of
		VTArray -> Left $ "Tried to use variable " ++ name ++ " as a scalar, but it was declared an array."
		VTScalar -> case Map.lookup name (scalars ctx) of
			Nothing -> Left $ "Unknown error in getScalarOrIter, VTScalar"
			Just vst -> Right (vst, ctx)
		VTIter -> case Map.lookup name (iterators ctx) of
			Nothing -> Left $ "Unknown error in getScalarOrIter, VTIter"
			Just vst -> Right (vst, ctx)

scalarOrIterInitialized :: Name -> StateT Context (Either Error) Bool
scalarOrIterInitialized name = do
	vst <- getScalarOrIter name
	StateT $ \ctx -> case vst of
		Uninitialized -> Left $ "Tried to use uninitialized variable " ++ name ++ "."
		_ -> Right (True, ctx)

getArrayElement :: Name -> Index -> StateT Context (Either Error) VarState
getArrayElement name index = do
	vtype <- getVarType name
	StateT $ \ctx -> case vtype of
		VTScalar -> Left $ "Tried to use variable " ++ name ++ " as an array, but it was declared a scalar."
		VTIter -> Left $ "Tried to use variable " ++ name ++ " as an array, but it was declared a scalar."
		VTArray -> case Map.lookup name (arrays ctx) of
			Nothing -> Left $ "Unknown error in getArrayElement, VTArray"
			Just (size, array) ->
				if not $ 0 <= index && index < size
				then Left $ "Index " ++ (show index) ++ " out of bounds 0-" ++ show (size - 1) ++ " in expression " ++ name ++ "[" ++ (show index) ++ "]."
				else case Map.lookup index array of
					Nothing -> Left $ "Unknown error in getArrayElement, looking for index value"
					Just vst -> Right (vst, ctx)

arrayElementInitialized :: Name -> Index -> StateT Context (Either Error) Bool
arrayElementInitialized name index = do
	vst <- getArrayElement name index
	StateT $ \ctx -> case vst of
		Uninitialized -> Left $ "Tried to use uninitialized variable " ++ name ++ "[" ++ (show index) ++ "]."
		_ -> Right (True, ctx)
