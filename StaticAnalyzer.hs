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

-- Watch out: WriteUninitializedArray.imp

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

data VarType = VTScalar VarState | VTArray Size (Map.Map Integer VarState) | VTIter VarState

-- Context keeps track of declared variables. It also knows whether
-- the variable has been initialized.
data Context = Context {scalars :: Map.Map Name VarState, arrays :: Map.Map Name (Size, Map.Map Integer VarState), iterators :: Map.Map Name VarState}
	deriving (Eq, Show)

emptyContext = Context {scalars = Map.empty, arrays = Map.empty, iterators = Map.empty}

-- Retrieve a variable from the context by name.
getVar :: Name -> AlexPosn -> StateT Context (Either Error) VarType
getVar name pos = do
	ctx <- get
	case Map.lookup name (iterators ctx) of
		Just vst -> return $ VTIter vst
		Nothing -> case Map.lookup name (scalars ctx) of
			Just vst -> return $ VTScalar vst
			Nothing -> case Map.lookup name (arrays ctx) of
				Just (size, array) -> return $ VTArray size array
				Nothing -> errT pos $ "Undeclared variable " ++ name ++ "."

getScalar :: Name -> AlexPosn -> StateT Context (Either Error) VarState
getScalar name pos = do
	vt <- getVar name pos
	case vt of
		VTIter _ -> errT pos $ "Trying to use variable " ++ name ++ " as a scalar, but it was declared an iterator."
		VTArray _ _ -> errT pos $ "Trying to use variable " ++ name ++ " as a scalar, but it was declared an array."
		VTScalar vst -> return vst

getArray :: Name -> AlexPosn -> StateT Context (Either Error) (Size, Map.Map Integer VarState)
getArray name pos = do
	vt <- getVar name pos
	case vt of
		VTIter _ -> errT pos $ "Trying to use variable " ++ name ++ " as an array, but it was declared an iterator."
		VTArray size array -> return (size, array)
		VTScalar _ -> errT pos $ "Trying to use variable " ++ name ++ " as an array, but it was declared a scalar."

getArrayIndex' :: Name -> Index -> AlexPosn -> StateT Context (Either Error) VarState
getArrayIndex' name index pos = do
	(size, array) <- getArray name pos	
	if not $ 0 <= index && index < size
	then errT pos $ "Index " ++ (show index) ++ " out of bounds 0-" ++ show (size - 1) ++ " in expression " ++ name ++ "[" ++ (show index) ++ "]."
	else case Map.lookup index array of
		Nothing -> error "Internal error in getArrayIndex', around line 76"
		Just vst -> return vst

-- Monadic guards that check whether variables are declared.
assertUndeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertUndeclared name pos = StateT $ \ctx -> case evalStateT (getVar name pos) ctx of
	Left _ -> Right $ ((), ctx)
	Right _ -> err pos $ "Variable " ++ name ++ " already declared."

assertScalarUndeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertScalarUndeclared name pos = StateT $ \ctx -> case evalStateT (getVar name pos) ctx of
	Left _ -> Right $ ((), ctx)
	Right (VTScalar _) -> err pos $ "Variable " ++ name ++ " already declared."
	Right (VTArray _ _) -> err pos $ "Variable " ++ name ++ " already declared as an array."
	Right (VTIter _) -> err pos $ "Variable " ++ name ++ " already declared as an iterator." -- This shouldn't happen.

assertArrayUndeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertArrayUndeclared name pos = StateT $ \ctx -> case evalStateT (getVar name pos) ctx of
	Left _ -> Right $ ((), ctx)
	Right (VTScalar _) -> err pos $ "Variable " ++ name ++ " already declared as a scalar."
	Right (VTArray _ _) -> err pos $ "Variable " ++ name ++ " already declared."
	Right (VTIter _) -> err pos $ "Variable " ++ name ++ " already declared as an iterator." -- This shouldn't happen.

assertIterUndeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertIterUndeclared name pos = StateT $ \ctx -> case evalStateT (getVar name pos) ctx of
	Left _ -> Right $ ((), ctx)
	Right (VTScalar _) -> err pos $ "Variable " ++ name ++ " already declared as a scalar."
	Right (VTArray _ _) -> err pos $ "Variable " ++ name ++ " already declared as an array."
	Right (VTIter _) -> err pos $ "Variable " ++ name ++ " already declared."

-- These check whether a variable was already declared.
assertDeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertDeclared name pos = getVar name pos >> return ()

-- OLD (but stil used).
scalarDeclared :: String -> Context -> Bool
scalarDeclared name ctx = isJust $ Map.lookup name (scalars ctx)

arrayDeclared :: String -> Context -> Bool
arrayDeclared name ctx = isJust $ Map.lookup name (arrays ctx)

iteratorDeclared :: String -> Context -> Bool
iteratorDeclared name ctx = isJust $ Map.lookup name (iterators ctx)

isDeclared :: String -> Context -> Bool
isDeclared name ctx = scalarDeclared name ctx || arrayDeclared name ctx || iteratorDeclared name ctx

-- Asserts.
assertScalarDeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertScalarDeclared name pos = getScalar name pos >> return ()

assertArrayDeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertArrayDeclared name pos = getArray name pos >> return ()

assertScalarInitialized :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertScalarInitialized name pos = do
	vst <- getScalar name pos
	case vst of
		Uninitialized -> errT pos $ "Tried to use uninitialized variable " ++ name ++ "."
		_ -> return ()

assertArrayNumInitialized :: Name -> Index -> AlexPosn -> StateT Context (Either Error) ()
assertArrayNumInitialized name index pos = do
	vst <- getArrayIndex name index pos
	case vst of
		Uninitialized -> errT pos $ "Tried to use uninitialized array element " ++ name ++ "[" ++ (show index) ++ "]."
		_ -> return ()

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
		Nothing -> error "Internal error in getArrayIndex" -- Logic guarantees it can't happen.
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
	Asgn id expr -> analyzeAsgn id expr --error "Zaimplementowac analyzeCommand, przypadek Asgn"
	If cond cmds cmds' -> error "Zaimplementowac analyzeCommand, przypadek If"
	While cond cmds -> error "Zaimplementowac analyzeCommand, przypadek While"
	ForUp name v v' cmds -> error "Zaimplementowac analyzeCommand, przypadek ForUp"
	ForDown name v v' cmds -> error "Zaimplementowac analyzeCommand, przypadek ForDown"
	Read id -> analyzeRead id
	Write val -> analyzeWrite val --error "Zaimplementowac analyzeCommand, przypadek Write"
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
readArrayNum name index pos = do
	(size, array) <- getArray name pos
	assertIndexInBounds name index pos
	modify $ \ctx -> ctx {arrays = Map.insert name (size, Map.insert index Initialized array) (arrays ctx)}
	return ()

readArrayPidentifier :: Name -> Name -> AlexPosn -> AlexPosn -> StateT Context (Either Error) ()
readArrayPidentifier name indexName namePos indexPos = do
	(size, array) <- getArray name namePos
	vst <- getScalar indexName indexPos
	case vst of
		Uninitialized -> errT namePos $ "Tried to use uninitialized variable " ++ name ++ "."
		Initialized -> modify $ \ctx -> ctx {arrays = Map.insert name (size, Map.fromList (zip [0..size-1] (repeat Initialized))) (arrays ctx)}
		HasValue index -> modify $ \ctx -> ctx {arrays = Map.insert name (size, Map.insert index Initialized array) (arrays ctx)}
	return ()
	
analyzeWrite :: Value -> StateT Context (Either Error) Command
analyzeWrite (Num n) = return $ Write (Num n)
analyzeWrite (Identifier id) = do
	assertInitialized id
	return $ Write (Identifier id)

assertInitialized :: Identifier -> StateT Context (Either Error) ()
assertInitialized id = case id of
	Pidentifier name pos -> assertScalarInitialized name pos
	ArrayNum name index pos -> assertArrayNumInitialized name index pos
	ArrayPidentifier name indexName namePos indexPos -> do
		vst <- getScalar indexName indexPos
		case vst of
			Uninitialized -> errT indexPos $ "Tried to use uninitialized array index " ++ indexName ++ " in " ++ name ++ "[" ++ indexName ++ "]."
			Initialized -> return ()
			HasValue index -> assertArrayNumInitialized name index namePos

analyzeAsgn :: Identifier -> Expression -> StateT Context (Either Error) Command
analyzeAsgn id exp = do
	case id of
		Pidentifier name pos -> assertScalarDeclared name pos
		ArrayNum name index pos -> assertIndexInBounds name index pos
		ArrayPidentifier name indexName namePos indexPos -> do
			assertArrayDeclared name namePos
			assertScalarDeclared indexName indexPos
	assertExpressionInitialized exp
	return $ Asgn id exp

assertExpressionInitialized :: Expression -> StateT Context (Either Error) ()
assertExpressionInitialized = undefined
