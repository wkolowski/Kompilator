module StaticAnalyzer where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.Trans

import Lexer
import Parser

-- TODO: looks like I should merge assertArrayDeclared and getArray into one function
-- in order to avoid having shit like "error ..." around. Then I would just make them
-- each other's aliases.

type Error = String
type Name = String
type Size = Integer
type Index = Integer

err :: AlexPosn -> String -> Either Error a
err (AlexPn _ line col) msg = Left $ "Error in line " ++ (show line) ++ ", column " ++ (show col) ++ ": " ++ msg

errT :: AlexPosn -> String -> StateT s (Either Error) a
errT pos msg = StateT $ \_ -> err pos msg

-- Variable state can be either undeclared, uninitialized, initialized (but
-- without known value) or initialized with a known value.
-- Undeclared is represented by Nothing when looking the variable up in the
-- context.
data VarState = Uninitialized | Initialized | HasValue Integer deriving (Eq, Show)


-- Context keeps track of declared variables. It also knows whether
-- the variable has been initialized.
data Context = Context {scalars :: Map.Map Name VarState, arrays :: Map.Map Name (Size, Map.Map Integer VarState), iterators :: Map.Map Name VarState}
	deriving (Eq, Show)

emptyContext = Context {scalars = Map.empty, arrays = Map.empty, iterators = Map.empty}

-- These check whether a variable was already declared.
scalarDeclared :: String -> Context -> Bool
scalarDeclared name ctx = isJust $ Map.lookup name (scalars ctx)

arrayDeclared :: String -> Context -> Bool
arrayDeclared name ctx = isJust $ Map.lookup name (arrays ctx)

iteratorDeclared :: String -> Context -> Bool
iteratorDeclared name ctx = isJust $ Map.lookup name (iterators ctx)

isDeclared :: String -> Context -> Bool
isDeclared name ctx = scalarDeclared name ctx || arrayDeclared name ctx || iteratorDeclared name ctx

-- Monadic guards that check whether variables are (un)declared.
assertScalarUndeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertScalarUndeclared name pos = StateT $ \ctx ->
	if arrayDeclared name ctx then err pos $ "Variable " ++ name ++ " already declared as an array."
	else if iteratorDeclared name ctx then err pos $ "Variable " ++ name ++ " already declared as an iterator." -- This shouldn't happen.
	else if scalarDeclared name ctx then err pos $ "Variable " ++ name ++ " already declared."
	else Right ((), ctx)

assertArrayUndeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertArrayUndeclared name pos = StateT $ \ctx ->
	if scalarDeclared name ctx then err pos $ "Variable " ++ name ++ " already declared as a scalar."
	else if iteratorDeclared name ctx then err pos $ "Variable " ++ name ++ " already declared as an iterator."
	else if arrayDeclared name ctx then err pos $ "Variable " ++ name ++ " already declared."
	else Right ((), ctx)

assertIterUndeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertIterUndeclared name pos = StateT $ \ctx ->
	if iteratorDeclared name ctx then err pos $ "Iterator variable " ++ name ++ " already declared."
	else Right $ ((), ctx)

assertScalarDeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertScalarDeclared name pos = StateT $ \ctx ->
	if arrayDeclared name ctx then err pos $ "Trying to use variable " ++ name ++ " as a scalar, but it was declared an array."
	else if not $ scalarDeclared name ctx then err pos $ "Undeclared variable " ++ name ++ "."
	else Right ((), ctx)

assertArrayDeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertArrayDeclared name pos = StateT $ \ctx ->
	if scalarDeclared name ctx then err pos $ "Trying to use variable " ++ name ++ " as an array, but it was declared a scalar."
	else if iteratorDeclared name ctx then err pos $ "Trying to use variable " ++ name ++ " as an array, but it was declared an iterator."
	else if not $ arrayDeclared name ctx then err pos $ "Undeclared variable " ++ name ++ "."
	else Right ((), ctx)

-- Monadic actions that declare new variables.
declareVar :: Declaration -> StateT Context (Either Error) Declaration
declareVar decl = do
	case decl of
		Scalar name pos -> declareScalar name pos
		Array name size pos -> declareArray name size pos
	return decl

declareScalar :: Name -> AlexPosn -> StateT Context (Either Error) ()
declareScalar name pos = do
	assertScalarUndeclared name pos
	modify $ \ctx -> ctx {scalars = Map.insert name Uninitialized (scalars ctx)}
	return ()

declareArray :: Name -> Size -> AlexPosn -> StateT Context (Either Error) ()
declareArray name size pos = do
	assertArrayUndeclared name pos
	modify $ \ctx -> ctx {arrays = Map.insert name (size, Map.fromList (zip [0..size - 1] (repeat Uninitialized))) (arrays ctx)}
	return ()

declareIter :: Name -> AlexPosn -> StateT Context (Either Error) ()
declareIter name pos = do
	assertIterUndeclared name pos
	modify $ \ctx -> ctx {iterators = Map.insert name Initialized (iterators ctx)}
	return ()

-- Gets.
getScalar :: Name -> AlexPosn -> StateT Context (Either Error) VarState
getScalar name pos = do
	assertScalarDeclared name pos
	ctx <- get
	case Map.lookup name (scalars ctx) of
		Nothing -> error "Internal error in getScalar"
		Just vst -> return vst

getArray :: Name -> AlexPosn -> StateT Context (Either Error) (Size, Map.Map Integer VarState)
getArray name pos = do
	assertArrayDeclared name pos
	ctx <- get
	case Map.lookup name (arrays ctx) of
		Nothing -> error "Internal error in getArray"
		Just result -> return result

-- Bounds checking.
assertIndexInBounds :: Name -> Index -> AlexPosn -> StateT Context (Either Error) ()
assertIndexInBounds name index pos = do
	(size, array) <- getArray name pos
	if not $ 0 <= index && index < size
	then errT pos $ "Index " ++ (show index) ++ " out of bounds 0-" ++ show (size - 1) ++ " in expression " ++ name ++ "[" ++ (show index) ++ "]."
	else return ()

getArrayIndex :: Name -> Index -> AlexPosn -> StateT Context (Either Error) VarState
getArrayIndex name index pos = do
	(size, array) <- getArray name pos
	assertIndexInBounds name index pos
	case Map.lookup index array of
		Nothing -> error "Internal error in getArrayIndex"
		Just vst -> return vst

-- Does static analysis.
analyze :: Program -> StateT Context (Either Error) Program
analyze (Program decls cmds) = liftM2 Program (analyzeDeclarations decls) (analyzeCommands cmds)

analyzeDeclarations :: [Declaration] -> StateT Context (Either Error) [Declaration]
analyzeDeclarations decls = mapM declareVar decls

analyzeCommands :: [Command] -> StateT Context (Either Error) [Command]
analyzeCommands cmds = mapM analyzeCommand cmds

analyzeCommand :: Command -> StateT Context (Either Error) Command
analyzeCommand cmd = case cmd of
	Asgn id expr -> error "Zaimplementowac analyzeCommand, przypadek Asgn"
	If cond cmds cmds' -> error "Zaimplementowac analyzeCommand, przypadek If"
	While cond cmds -> error "Zaimplementowac analyzeCommand, przypadek While"
	ForUp name v v' cmds -> error "Zaimplementowac analyzeCommand, przypadek ForUp"
	ForDown name v v' cmds -> error "Zaimplementowac analyzeCommand, przypadek ForDown"
	Read id -> analyzeRead id
	Write val -> error "Zaimplementowac analyzeCommand, przypadek Write"
	Skip -> return Skip	

analyzeRead :: Identifier -> StateT Context (Either Error) Command
analyzeRead id = do
	case id of
		Pidentifier name pos -> readPidentifier name pos
		ArrayNum name index pos -> readArrayNum name index pos
		ArrayPidentifier name indexName posName posIndex -> readArrayPidentifier name indexName posName posIndex
	return $ Read id

readPidentifier :: Name -> AlexPosn -> StateT Context (Either Error) ()
readPidentifier name pos = do
	assertScalarDeclared name pos
	modify $ \ctx -> ctx {scalars = Map.insert name Initialized (scalars ctx)}
	return ()

readArrayNum :: Name -> Index -> AlexPosn -> StateT Context (Either Error) ()
readArrayNum name index pos = StateT $ \ctx ->
	if scalarDeclared name ctx then err pos $ "Trying to use variable " ++ name ++ " as an array, but it was declared a scalar."
	else case Map.lookup name (arrays ctx) of
		Nothing -> err pos $ "Undeclared variable " ++ name ++ "."
		Just (size, array) ->
			if not $ 0 <= index && index < size
			then err pos $ "Index " ++ (show index) ++ " out of bounds 0-" ++ show (size - 1) ++ " in expression " ++ name ++ "[" ++ (show index) ++ "]."
			--else Right (ctx {arrays = Map.insert name (size, Map.insert index Initialized array) (arrays ctx)}, ctx)
			else Right ((), ctx {arrays = Map.insert name (size, Map.insert index Initialized array) (arrays ctx)})

readArrayPidentifier :: Name -> Name -> AlexPosn -> AlexPosn -> StateT Context (Either Error) ()
readArrayPidentifier name indexName posName posIndex = do
	ctx <- get
	if scalarDeclared name ctx then StateT $ \_ -> err posName $ "Trying to use variable " ++ name ++ " as an array, but it was declared a scalar."
	else case Map.lookup name (arrays ctx) of
		Nothing -> StateT $ \_ -> err posName $ "Undeclared variable " ++ name ++ "."
		Just (size, array) ->
			if arrayDeclared indexName ctx
			then StateT $ \_ -> err posIndex $ "Trying to use variable " ++ indexName ++ " as a scalar, but it was declared an array."
			else do
				vst <- getScalarOrIter indexName posIndex
				case vst of
					Uninitialized -> StateT $ \_ -> err posIndex $ "Tried to use uninitialized variable " ++ indexName ++ " as an index in " ++ name ++ "[" ++ indexName ++ "]."
					HasValue index -> readArrayNum name index posName
					--Initialized -> return $ ctx {arrays = Map.insert name (size, Map.fromList (zip [0..size - 1] (repeat Initialized))) (arrays ctx)}
					Initialized -> do
						put $ ctx {arrays = Map.insert name (size, Map.fromList (zip [0..size - 1] (repeat Initialized))) (arrays ctx)}
						return ()

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

getVarType :: Name -> AlexPosn -> StateT Context (Either Error) VarType
getVarType name pos = StateT $ \ctx ->
	if isJust $ Map.lookup name (scalars ctx) then Right (VTScalar, ctx)
	else if isJust $ Map.lookup name (arrays ctx) then Right (VTArray, ctx)
	else if isJust $ Map.lookup name (iterators ctx) then Right (VTIter, ctx)
	else err pos $ "Undeclared variable " ++ name ++ "."

getScalarOrIter :: Name -> AlexPosn -> StateT Context (Either Error) VarState
getScalarOrIter name pos = do
	vtype <- getVarType name pos
	ctx <- get
	StateT $ \_ -> case vtype of
		VTArray -> err pos $ "Tried to use variable " ++ name ++ " as a scalar, but it was declared an array."
		VTScalar -> case Map.lookup name (scalars ctx) of
			Nothing -> err pos $ "Unknown error in getScalarOrIter, VTScalar"
			Just vst -> Right (vst, ctx)
		VTIter -> case Map.lookup name (iterators ctx) of
			Nothing -> err pos $ "Unknown error in getScalarOrIter, VTIter"
			Just vst -> Right (vst, ctx)

scalarOrIterInitialized :: Name -> AlexPosn -> StateT Context (Either Error) Bool
scalarOrIterInitialized name pos = do
	vst <- getScalarOrIter name pos
	StateT $ \ctx -> case vst of
		Uninitialized -> err pos $ "Tried to use uninitialized variable " ++ name ++ "."
		_ -> Right (True, ctx)

getArrayElement :: Name -> Index -> AlexPosn -> StateT Context (Either Error) VarState
getArrayElement name index pos = do
	vtype <- getVarType name pos
	StateT $ \ctx -> case vtype of
		VTScalar -> err pos $ "Tried to use variable " ++ name ++ " as an array, but it was declared a scalar."
		VTIter -> err pos $ "Tried to use variable " ++ name ++ " as an array, but it was declared a scalar."
		VTArray -> case Map.lookup name (arrays ctx) of
			Nothing -> err pos $ "Unknown error in getArrayElement, VTArray"
			Just (size, array) ->
				if not $ 0 <= index && index < size
				then err pos $ "Index " ++ (show index) ++ " out of bounds 0-" ++ show (size - 1) ++ " in expression " ++ name ++ "[" ++ (show index) ++ "]."
				else case Map.lookup index array of
					Nothing -> err pos $ "Unknown error in getArrayElement, looking for index value"
					Just vst -> Right (vst, ctx)

arrayElementInitialized :: Name -> Index -> AlexPosn -> StateT Context (Either Error) Bool
arrayElementInitialized name index pos = do
	vst <- getArrayElement name index pos
	StateT $ \ctx -> case vst of
		Uninitialized -> err pos $ "Tried to use uninitialized variable " ++ name ++ "[" ++ (show index) ++ "]."
		_ -> Right (True, ctx)
