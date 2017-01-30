module StaticAnalyzer where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Maybe
import qualified Data.Map.Strict as Map

import AST

-- Error is for static analysis errors, Name is for names of variables, Size
-- is for array sizes and index represents array indices.
type Error = String
type Name = String
type Size = Integer
type Index = Integer

-- Static analysis errors. They require knowledge of where in the file we are.
err :: AlexPosn -> String -> Either Error a
err (AlexPn _ line col) msg = Left $ "Error in line " ++ (show line) ++ ", column " ++ (show col) ++ ": " ++ msg

errT :: AlexPosn -> String -> StateT s (Either Error) a
errT pos msg = StateT $ \_ -> err pos msg

-- Variable state can be either uninitialized, initialized (but without known
-- value) or initialized with a known value. Undeclared is represented by Nothing
-- when looking the variable up in the context.
data VarState = Uninitialized | Initialized | HasValue Integer deriving (Eq, Show)

-- Result of a computation is unknown when variables with values unknown at
-- compile time are used (e. g. when adding two scalars that were READ from
-- stdin). Unknown DOES NOT mean 'Undeclared' or 'undefined'.
data ComputationResult = Unknown | Known Integer deriving (Eq, Show)

-- Variables are either scalars, arrays or iterators (not sure however if the 
-- division scalar/iterator is good).
data VarType = VTScalar VarState | VTArray Size (Map.Map Integer VarState) | VTIter VarState

-- Context keeps track of declared variables. It also knows whether the variable
-- has been initialized and if it has a value.
data Context = Context {scalars :: Map.Map Name VarState, arrays :: Map.Map Name (Size, Map.Map Integer VarState), iterators :: Map.Map Name VarState}
	deriving (Eq, Show)

emptyContext = Context {scalars = Map.empty, arrays = Map.empty, iterators = Map.empty}

-- Retrieves a variable from the context by name.
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

-- These retrieve variable from context. They also check whether the variable
-- has is declared and correctly used (i. e. whether a scalar was
-- used as a scalar and not an array).
getScalar :: Name -> AlexPosn -> StateT Context (Either Error) VarState
getScalar name pos = do
	vt <- getVar name pos
	case vt of
		VTIter _ -> errT pos $ "Trying to use variable " ++ name ++ " as a scalar, but it was declared an iterator."
		VTArray _ _ -> errT pos $ "Trying to use variable " ++ name ++ " as a scalar, but it was declared an array."
		VTScalar vst -> return vst

getScalarOrIter :: Name -> AlexPosn -> StateT Context (Either Error) VarState
getScalarOrIter name pos = do
	vt <- getVar name pos
	case vt of
		VTScalar vst -> return vst
		VTIter vst -> return vst
		VTArray _ _ -> errT pos $ "Tried to use variable " ++ name ++ " as a scalar/iterator, but it was declared an array."

getArray :: Name -> AlexPosn -> StateT Context (Either Error) (Size, Map.Map Integer VarState)
getArray name pos = do
	vt <- getVar name pos
	case vt of
		VTIter _ -> errT pos $ "Trying to use variable " ++ name ++ " as an array, but it was declared an iterator."
		VTArray size array -> return (size, array)
		VTScalar _ -> errT pos $ "Trying to use variable " ++ name ++ " as an array, but it was declared a scalar."

getArrayIndex :: Name -> Index -> AlexPosn -> StateT Context (Either Error) VarState
getArrayIndex name index pos = do
	(size, array) <- getArray name pos	
	if not $ 0 <= index && index < size
	then errT pos $ "Index " ++ (show index) ++ " out of bounds 0-" ++ show (size - 1) ++ " in expression " ++ name ++ "[" ++ (show index) ++ "]."
	else case Map.lookup index array of
		Nothing -> error "Internal error in getArrayIndex, around line 78" -- This can't happen (or should I put 'index of out bounds error' here not above?
		Just vst -> return vst

getIdentifier :: Identifier -> StateT Context (Either Error) VarState
getIdentifier id = case id of
	Pidentifier name pos -> do
		vt <- getVar name pos
		case vt of
			VTScalar vst -> return vst
			VTArray _ _ -> errT pos $ "Tried to use variable " ++ name ++ " as a scalar, but it was declared an array."
			VTIter vst -> return vst
	ArrayNum name index pos -> getArrayIndex name index pos
	ArrayPidentifier name indexName namePos indexPos -> do
		vst <- getScalarOrIter indexName indexPos
		case vst of
			Uninitialized -> errT indexPos $ "Tried to use uninitialized array index " ++ indexName ++ " in " ++ name ++ "[" ++ indexName ++ "]."
			Initialized -> return Initialized
			HasValue index -> getArrayIndex name index namePos

-- Compute value of variables, identifiers, expressions etc.
computeScalar :: Name -> AlexPosn -> StateT Context (Either Error) ComputationResult
computeScalar name pos = do
	vst <- getScalar name pos
	case vst of
		Uninitialized -> errT pos $ "Tried to use uninitialized scalar variable " ++ name ++ "."
		Initialized -> return Unknown
		HasValue n -> return $ Known n

computeScalarOrIter :: Name -> AlexPosn -> StateT Context (Either Error) ComputationResult
computeScalarOrIter name pos = do
	vst <- getScalarOrIter name pos
	case vst of
		Uninitialized -> errT pos $ "Tried to use uninitialized scalar/iterator variable " ++ name ++ "."
		Initialized -> return Unknown
		HasValue n -> return $ Known n

computeArrayNum :: Name -> Index -> AlexPosn -> StateT Context (Either Error) ComputationResult
computeArrayNum name index pos = do
	vst <- getArrayIndex name index pos
	case vst of
		Uninitialized -> errT pos $ "Tried to use uninitialized array element " ++ name ++ "[" ++ (show index) ++ "]."
		Initialized -> return Unknown
		HasValue n -> return $ Known n

computeArrayPidentifier :: Name -> Name -> AlexPosn -> AlexPosn -> StateT Context (Either Error) ComputationResult
computeArrayPidentifier name indexName namePos indexPos = do
	vst <- computeScalarOrIter indexName indexPos
	case vst of
		Unknown -> return Unknown
		Known index -> computeArrayNum name index namePos

computeIdentifier :: Identifier -> StateT Context (Either Error) ComputationResult
computeIdentifier id = do
	case id of
		Pidentifier name pos -> computeScalarOrIter name pos
		ArrayNum name index pos -> computeArrayNum name index pos
		ArrayPidentifier name indexName namePos indexPos -> computeArrayPidentifier name indexName namePos indexPos

computeValue :: Value -> StateT Context (Either Error) ComputationResult
computeValue v = do
	case v of
		Num n -> return $ Known n
		Identifier id -> computeIdentifier id

-- Extract name/position in file from an identifier.
positionOfIdent :: Identifier -> AlexPosn
positionOfIdent id = case id of
	Pidentifier _ pos -> pos
	ArrayNum _ _ pos -> pos
	ArrayPidentifier _ _ pos _ -> pos

nameOfIdent :: Identifier -> Name
nameOfIdent id = case id of
	Pidentifier name _ -> name
	ArrayNum name _ _ -> name
	ArrayPidentifier name _ _ _ -> name

-- Operations on unknown values are unknown.
liftCmpRes :: (Integer -> Integer -> Integer) -> ComputationResult -> ComputationResult -> ComputationResult
liftCmpRes op cr cr' = case cr of
	Unknown -> Unknown
	Known n -> case cr' of
		Unknown -> Unknown
		Known n' -> Known $ op n n'

-- Computes the given expression at compile time and checks for errors like
-- division by zero, modulo division by zero.
computeExpression :: Expression -> AlexPosn -> StateT Context (Either Error) ComputationResult
computeExpression exp pos = case exp of
	Value v -> computeValue v
	Plus v v' -> liftM2 (liftCmpRes (+)) (computeValue v) (computeValue v')
	Minus v v' -> do
		r <- computeValue v
		r' <- computeValue v'
		case r of
			Unknown -> return Unknown
			Known n -> case r' of
				Unknown -> return Unknown
				Known n' -> if n - n' < 0 then return $ Known 0 else return $ Known (n - n')
	Mul v v' -> liftM2 (liftCmpRes (*)) (computeValue v) (computeValue v')
	Div v v' -> do
		r <- computeValue v
		r' <- computeValue v'
		case r' of
			Unknown -> return Unknown
			Known 0 -> errT pos $ "Division by zero."
			Known n' -> case r of
				Unknown -> return Unknown
				Known n -> return $ Known (n `div` n')
	Mod v v' -> do
		r <- computeValue v
		r' <- computeValue v'
		case r' of
			Unknown -> return Unknown
			Known 0 -> errT pos $ "Modulo division by zero."
			Known n' -> case r of
				Unknown -> return Unknown
				Known n -> return $ Known (n `mod` n')

-- Monadic guards that check whether variables are undeclared.
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

-- Monadic guards that check whether variables are declared.
assertDeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertDeclared name pos = getVar name pos >> return ()

assertScalarDeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertScalarDeclared name pos = getScalar name pos >> return ()

assertArrayDeclared :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertArrayDeclared name pos = getArray name pos >> return ()

-- Monadic guards that check whether variables (and other things too)
-- are initialized (or if they are made from initialized variables).
assertScalarInitialized :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertScalarInitialized name pos = do
	vst <- getScalar name pos
	case vst of
		Uninitialized -> errT pos $ "Tried to use uninitialized variable " ++ name ++ "."
		_ -> return ()

assertScalarOrIterInitialized :: Name -> AlexPosn -> StateT Context (Either Error) ()
assertScalarOrIterInitialized name pos = do
	vst <- getScalarOrIter name pos
	case vst of
		Uninitialized -> errT pos $ "Tried to use uninitialized variable " ++ name ++ "."
		_ -> return ()

assertArrayNumInitialized :: Name -> Index -> AlexPosn -> StateT Context (Either Error) ()
assertArrayNumInitialized name index pos = do
	vst <- getArrayIndex name index pos
	case vst of
		Uninitialized -> errT pos $ "Tried to use uninitialized array element " ++ name ++ "[" ++ (show index) ++ "]."
		_ -> return ()

assertIdentifierInitialized :: Identifier -> StateT Context (Either Error) ()
assertIdentifierInitialized id = case id of
	Pidentifier name pos -> assertScalarOrIterInitialized name pos
	ArrayNum name index pos -> assertArrayNumInitialized name index pos
	ArrayPidentifier name indexName namePos indexPos -> do
		vst <- getScalarOrIter indexName indexPos
		case vst of
			Uninitialized -> errT indexPos $ "Tried to use uninitialized array index " ++ indexName ++ " in " ++ name ++ "[" ++ indexName ++ "]."
			Initialized -> return ()
			HasValue index -> assertArrayNumInitialized name index namePos

-- Monadic guard that checks whether index is correct.
assertIndexInBounds :: Name -> Index -> AlexPosn -> StateT Context (Either Error) ()
assertIndexInBounds name index pos = getArrayIndex name index pos >> return ()

-- Monadic actions that put something into variables (or identifiers) in context.
putScalar :: Name -> VarState -> StateT Context (Either Error) ()
putScalar name vst = do
	modify $ \ctx -> ctx {scalars = Map.insert name vst (scalars ctx)}
	return ()

putArray :: Name -> Size -> Map.Map Integer VarState -> StateT Context (Either Error) ()
putArray name size array = do
	modify $ \ctx -> ctx {arrays = Map.insert name (size, array) (arrays ctx)}
	return ()

putArrayElement :: Name -> Index -> VarState -> AlexPosn -> StateT Context (Either Error) ()
putArrayElement name index vst pos = do
	(size, array) <- getArray name pos
	assertIndexInBounds name index pos
	modify $ \ctx -> ctx {arrays = Map.insert name (size, Map.insert index vst array) (arrays ctx)} 
	return ()

putIdentifier :: Identifier -> VarState -> StateT Context (Either Error) ()
putIdentifier id vst = case id of
	Pidentifier name pos -> do
		vt <- getVar name pos
		case vt of
			VTScalar _ -> do
				assertScalarDeclared name pos
				putScalar name vst
			VTIter _ -> errT pos $ "Cannot assign to an iterator " ++ name ++ "!"
			VTArray _ _ -> errT pos $ "Tried to use variable " ++ name ++ " as a scalar/iterator, but it was declared an array"
	ArrayNum name index pos -> putArrayElement name index vst pos
	ArrayPidentifier name indexName namePos indexPos -> do
		indexCr <- computeScalarOrIter indexName indexPos
		case indexCr of
			Unknown -> do -- First two lines added recently.
				(size, array) <- getArray name undefined
				modify $ \ctx -> ctx {arrays = Map.insert name (size, Map.fromList (zip [0..size - 1] (repeat Initialized))) (arrays ctx)}
				return ()
			Known index -> putArrayElement name index vst namePos

-- Monadic actions that declare new variables (they also check whether these
-- variables were in fact previously undeclared).
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

-- The result of static analysis is the first encountered error, if there is any,
-- and otherwise the AST unchanged.
analyze :: Program -> StateT Context (Either Error) Program
analyze (Program decls cmds) = liftM2 Program (analyzeDeclarations decls) (analyzeCommands cmds)

analyzeDeclarations :: [Declaration] -> StateT Context (Either Error) [Declaration]
analyzeDeclarations decls = mapM declareVar decls

analyzeCommands :: [Command] -> StateT Context (Either Error) [Command]
analyzeCommands cmds = mapM analyzeCommand cmds

-- As of now, static analysis can detect if variables are undeclared,
-- uninitialized or used incorrectly (like assigning array to a scalar).
-- It works for Skip, Read, Write and assignments. Last time I checked
-- it passed all my tests.
analyzeCommand :: Command -> StateT Context (Either Error) Command
analyzeCommand cmd = case cmd of
	Asgn id expr -> analyzeAsgn id expr
	If cond cmds cmds' -> do
		analyzeCommands cmds
		analyzeCommands cmds'
		return cmd
	While cond cmds -> analyzeWhile cond cmds
	ForUp name v v' cmds -> do
		case v of
			Identifier id -> do
				assertIdentifierInitialized id
				if nameOfIdent id == name
				then StateT $ \_ -> Left $ "Tried to use iterator variable " ++ name ++ " in the range of a FOR loop."
				else return ()
			_ -> return ()
		case v' of
			Identifier id' -> do
				assertIdentifierInitialized id'
				if nameOfIdent id' == name
				then StateT $ \_ -> Left $ "Tried to use iterator variable " ++ name ++ " in the range of a FOR loop."
				else return ()
			_ -> return ()
		modify $ \ctx -> ctx {iterators = Map.insert name Initialized (iterators ctx)}
		analyzeCommands cmds
		modify $ \ctx -> ctx {iterators = Map.delete name (iterators ctx)}
		return cmd
	ForDown name v v' cmds -> do
		case v of
			Identifier id -> do
				assertIdentifierInitialized id
				if nameOfIdent id == name
				then StateT $ \_ -> Left $ "Tried to use iterator variable " ++ name ++ " in the range of a FOR loop."
				else return ()
			_ -> return ()
		case v' of
			Identifier id' -> do
				assertIdentifierInitialized id'
				if nameOfIdent id' == name
				then StateT $ \_ -> Left $ "Tried to use iterator variable " ++ name ++ " in the range of a FOR loop."
				else return ()
			_ -> return ()
		modify $ \ctx -> ctx {iterators = Map.insert name Initialized (iterators ctx)}
		analyzeCommands cmds
		modify $ \ctx -> ctx {iterators = Map.delete name (iterators ctx)}
		return cmd
	Read id -> analyzeRead id
	Write val -> analyzeWrite val
	Skip -> return Skip

-- Checks whether:
-- a scalar is declared (if reading a scalar)
-- array is declared and index is in bounds (if index is constant)
-- array is declared and index is initialized (if index is variable)
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

-- Checks whether:
-- a scalar is initialized (if writing a scalar)
-- array is declared and initialized at index (if index is constant)
-- array is declared and index is initialized (if index is a variable)
analyzeWrite :: Value -> StateT Context (Either Error) Command
analyzeWrite (Num n) = return $ Write (Num n)
analyzeWrite (Identifier id) = do
	assertIdentifierInitialized id
	return $ Write (Identifier id)

-- Checks whether:
-- identifier is declared as a single thing (so allowed are declared scalars,
-- array elements if index is constant and in bounds, array elements if index
--     is variable and initialized).
-- expression doesn't contain division by zero or modular division by zero
analyzeAsgn :: Identifier -> Expression -> StateT Context (Either Error) Command
analyzeAsgn id exp = do
	getIdentifier id
	cr <- computeExpression exp (positionOfIdent id)
	case cr of
		Unknown -> putIdentifier id Initialized
		Known n -> putIdentifier id (HasValue n)
	return $ Asgn id exp

-- To analyze the WHILE command, we have to set all variables that are present
-- in its condition to Initialized. This will make the analyzer set anything
-- that contains them to initialized which is helpful for arrays with unknown
-- indices.
analyzeWhile :: Condition -> [Command] -> StateT Context (Either Error) Command
analyzeWhile cond cmds = do
	initializeVarsIn cond
	analyzeCommands cmds
	return $ While cond cmds

extractVarsFromCond :: Condition -> (Value, Value)
extractVarsFromCond cond = case cond of
	Le v v' -> (v, v')
	Ge v v' -> (v, v')
	Lt v v' -> (v, v')
	Gt v v' -> (v, v')
	Eq v v' -> (v, v')
	Neq v v' -> (v, v')

initializeVarsIn :: Condition -> StateT Context (Either Error) ()
initializeVarsIn cond = do
	let (v, v') = extractVarsFromCond cond
	initializeValue v
	initializeValue v'
	return ()

initializeValue :: Value -> StateT Context (Either Error) ()
initializeValue v = case v of
	Num _ -> return ()
	Identifier id -> let name = nameOfIdent id in do
		vt <- getVar name undefined
		case vt of
			VTScalar _ -> do
				putScalar name Initialized
				return ()
			VTArray size _ -> do
				putArray name size $ Map.fromList (zip [0..size - 1] (repeat Initialized))
				return ()
			VTIter _ -> do
				modify $ \ctx -> ctx {iterators = Map.insert name Initialized (iterators ctx)}
				return ()
