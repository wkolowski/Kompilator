module CodeGenerator where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Map.Strict as Map
import Data.List (sortBy)

import AST

-- Forward application.
($>) = flip ($)

-- Registers.
data Reg = R0 | R1 | R2 | R3 | R4 deriving (Eq)

instance Show Reg where
	show R0 = show 0
	show R1 = show 1
	show R2 = show 2
	show R3 = show 3

-- Program counter.
type PC = Integer

-- Machine instructions.
data Instr
	= GET Reg | PUT Reg
	| LOAD Reg | STORE Reg
	| ADD Reg | SUB Reg
	| COPY Reg
	| SHR Reg | SHL Reg
	| INC Reg | DEC Reg
	| ZERO Reg
	| JUMP PC | JZERO Reg PC | JODD Reg PC
	| HALT
	deriving (Eq, Show)

-- Memory representation. First come scalars, then arrays, then iterators (but
-- they are treated the same as scalars).
type Address = Integer
type Size = Integer
type Error = String
type Name = String
type LineNumber = Integer

--type Memory = Map.Map Name MemEntry
data Memory = Memory {scalars :: Map.Map Name Address, arrays :: Map.Map Name (Size, Address)}

instance Show Memory where
	show memory = Map.union (fmap (\address -> (address, 1)) (scalars memory)) (fmap (\(size, address) -> (address, size)) (arrays memory))
		    $> Map.toList $> sortBy (\x y -> compare (fst . snd $ x) (fst . snd $ y)) $> show

emptyMemory :: Memory
emptyMemory = Memory {scalars = Map.empty, arrays = Map.empty}

getScalar :: Name -> StateT GenState (Either Error) Address
getScalar name = StateT $ \(memory, lineNumber) -> case Map.lookup name (scalars memory) of
	Nothing -> Left $ "Scalar variable " ++ name ++ " not found in memory."
	Just address -> Right (address, (memory, lineNumber))

getArray :: Name -> StateT GenState (Either Error) (Size, Address)
getArray name = StateT $ \(memory, lineNumber) -> case Map.lookup name (arrays memory) of
	Nothing -> Left $ "Array variable " ++ name ++ " not found in memory."
	Just sa -> Right (sa, (memory, lineNumber))

-- Errors.
errT :: String -> StateT a (Either Error) b
errT msg = StateT $ \_ -> Left msg

-- Code generator needs memory to keep track of variables and also
-- needs to keep track of how many lines of asm were already output
-- in order to calculate jump labels properly.
type GenState = (Memory, LineNumber)

incLineNumber :: Integer -> StateT GenState (Either Error) ()
incLineNumber k = do
	modify $ \(memory, lineNumber) -> (memory, lineNumber + k)
	return ()

-- Generate code
generateCode :: Program -> StateT GenState (Either Error) [Instr]
generateCode (Program decls cmds) = do
	allocateAll decls
	i <- generateCommands cmds
	incLineNumber 1
	return $ i ++ [HALT]

-- Allocate memory.
processDeclarations :: [Declaration] -> Either Error Memory
processDeclarations decls = evalStateT (foldM allocate emptyMemory decls) 0

allocate :: Memory -> Declaration -> StateT Integer (Either Error) Memory
allocate memory decl = do
	memoryLocation <- get
	if memoryLocation < 0 then errT "Memory location can't be less than 0." else return ()
	case decl of
		AST.Scalar name _ -> do
			put $ memoryLocation + 1
			return $ memory {scalars = Map.insert name memoryLocation (scalars memory)}
		AST.Array name size  _ -> do
			put $ memoryLocation + size
			return $ memory {arrays = Map.insert name (size, memoryLocation) (arrays memory)}

allocateAll :: [Declaration] -> StateT GenState (Either Error) ()
allocateAll decls = case processDeclarations decls of
	Left msg -> errT msg
	Right memory -> do
		modify $ \(_, lineNumber) -> (memory, lineNumber)
		return ()

generateCommands :: [Command] -> StateT GenState (Either Error) [Instr]
generateCommands cmds = fmap join $ mapM generateCommand cmds

generateCommand :: Command -> StateT GenState (Either Error) [Instr]
generateCommand cmd = case cmd of
	Asgn id exp -> generateAsgn id exp --generateAsgn id expr
	If cond cmds cmds' -> generateIf cond cmds cmds'
	While cond cmds -> undefined --generateWhile cond cmds
	ForUp name v v' cmds -> undefined --generateForUp name v v'
	ForDown name v v' cmds -> undefined --generateForDown name v v'
	Read id -> generateRead id
	Write val -> generateWrite val --undefined --generateWrite val
	_ -> return []

generateAsgn :: Identifier -> Expression -> StateT GenState (Either Error) [Instr]
generateAsgn id exp = do
	i <- loadExpression exp R1
	i' <- loadAddressToR0 id R4 -- It gets loaded to R0. R4 is just intermediate.
	incLineNumber 1
	return $ i ++ i' ++ [STORE R1]

generateWrite :: Value -> StateT GenState (Either Error) [Instr]
generateWrite v = do
	i <- loadValue v R1
	incLineNumber 1
	return $ i ++ [PUT R1]

generateRead :: Identifier -> StateT GenState (Either Error) [Instr]
generateRead id = do
	i <- loadAddressToR0 id R4
	incLineNumber 2
	return $ i ++ [GET R1, STORE R1]

generateIf :: Condition -> [Command] -> [Command] -> StateT GenState (Either Error) [Instr]
generateIf cond cmds cmds' = case cond of
	Le v v' -> generateIfLe v v' cmds cmds'
	Ge v v' -> generateIfLe v' v cmds cmds'
	Lt v v' -> generateIfLe v' v cmds' cmds
	Gt v v' -> generateIfLe v v' cmds' cmds
	Eq v v' -> generateIfEq v v' cmds cmds'
	Neq v v' -> generateIfEq v v' cmds' cmds

generateIfLe :: Value -> Value -> [Command] -> [Command] -> StateT GenState (Either Error) [Instr]
generateIfLe v v' cmds cmds' = do
	c <- computeCondition (Le v v') R1 R2
	incLineNumber 1
	i' <- generateCommands cmds'
	incLineNumber 1
	(_, afterJump) <- get
	i <- generateCommands cmds
	(_, end) <- get
	return $ c ++ [JZERO R1 afterJump] ++ i' ++ [JUMP end] ++ i

generateIfEq :: Value -> Value -> [Command] -> [Command] -> StateT GenState (Either Error) [Instr]
generateIfEq v v' cmds cmds' = do
	c <- computeCondition (Eq v v') R1 R2
	incLineNumber 2
	(_, secondJump) <- get
	incLineNumber 2
	(_, thenLine) <- get
	thenCode <- generateCommands cmds
	incLineNumber 1
	(_, elseLine) <- get
	elseCode <- generateCommands cmds'
	(_, endLine) <- get
	return $ c ++ [JZERO R1 secondJump, JUMP elseLine, JZERO R2 thenLine, JUMP elseLine] ++ thenCode ++ [JUMP endLine] ++ elseCode

-- Instructions that write a constant into a register.
data BinaryDigit = B0 | B1 deriving (Eq, Show)

loadConst' :: Integer -> Reg -> Either Error [Instr]
loadConst' n reg = liftM2 genInstr (toBinary n) (return reg) $> join $> fmap reverse
	where

	toBinary :: Integer -> Either Error [BinaryDigit]
	toBinary n
		| n < 0 = Left "This machine can't handle negative numbers!"
		| n == 0 = Right [B0]
		| n == 1 = Right [B1]
		| otherwise = liftM2 (:) (Right $ if even n then B0 else B1) (toBinary (n `div` 2))

	genInstr :: [BinaryDigit] -> Reg -> Either Error [Instr]
	genInstr [] reg = Left "Empty list passed to loadConst'!"
	genInstr [B0] reg = return [ZERO reg]
	genInstr [B1] reg = return [INC reg, ZERO reg]
	genInstr (B0:bits) reg = liftM2 (:) (return $ SHL reg) (genInstr bits reg)
	genInstr (B1:bits) reg = liftM2 (++) (return [INC reg, SHL reg]) (genInstr bits reg)

loadConst :: Integer -> Reg -> StateT GenState (Either Error) [Instr]
loadConst n reg = do
	instructions <- lift $ loadConst' n reg
	incLineNumber (toInteger $ length instructions)
	--error $ show $ length instructions
	return instructions

-- Load address of id to R0 using reg to keep intermediate results.
loadAddressToR0 :: Identifier -> Reg -> StateT GenState (Either Error) [Instr]
loadAddressToR0 id reg
	| reg == R0 = errT $ "Can't load to R0 using R0 as intermediate register."
	| otherwise = case id of
		Pidentifier name _ -> do
			address <- getScalar name
			loadConst address R0
		ArrayNum name index _ -> do
			(size, address) <- getArray name
			if not $ 0 <= index && index < size then errT $ "Index " ++ show index ++ " out of bounds 0-" ++ show (size - 1) ++ "." else return ()
			loadConst (address + index) R0
		ArrayPidentifier name indexName _ _ -> do
			(_, arrayAddress) <- getArray name
			i <- loadConst arrayAddress reg
			indexAddress <- getScalar indexName
			i' <- loadConst indexAddress R0
			incLineNumber 2
			return $ i ++ i' ++ [ADD reg, COPY reg]

loadValue :: Value -> Reg -> StateT GenState (Either Error) [Instr]
loadValue val reg = case val of
	Num n -> loadConst n reg
	Identifier id -> do
		i <- loadAddressToR0 id R4
		incLineNumber 1
		return $ i ++ [LOAD reg]

loadExpression :: Expression -> Reg -> StateT GenState (Either Error) [Instr]
loadExpression exp reg = case exp of
	Value v -> loadValue v reg

	Plus (Num n) (Num n') -> loadConst (n + n') reg
	Plus v@(Identifier id) v'@(Num n') -> loadExpression (Plus v' v) reg
	Plus (Num n) (Identifier id') -> do
		i <- loadConst n reg
		i' <- loadAddressToR0 id' R4
		incLineNumber 1
		return $ i ++ i' ++ [ADD reg]
	Plus (Identifier id) (Identifier id') -> do
		i <- loadAddressToR0 id R4
		i' <- loadAddressToR0 id' R4
		incLineNumber 2
		return $ i ++ [LOAD reg] ++ i' ++ [ADD reg]

	Minus (Num n) (Num n') -> loadConst (max 0 (n - n')) reg
	Minus v@(Identifier id) v'@(Num n') -> loadExpression (Minus v' v) reg
	Minus (Num n) (Identifier id') -> do
		i <- loadConst n reg
		i' <- loadAddressToR0 id' R4
		incLineNumber 1
		return $ i ++ i' ++ [SUB reg]
	Minus (Identifier id) (Identifier id') -> do
		i <- loadAddressToR0 id R4
		i' <- loadAddressToR0 id' R4
		incLineNumber 2
		return $ i ++ [LOAD reg] ++ i' ++ [SUB reg]

	_ -> error "Hej, koniu! Zaimplementuj mnożenie i dzielenie."

computeCondition :: Condition -> Reg -> Reg -> StateT GenState (Either Error) [Instr]
computeCondition cond reg reg' = case cond of
	Le v v' -> loadExpression (Minus v v') reg
	Ge v v' -> computeCondition (Le v' v) reg reg'
	Lt v v' -> loadExpression (Minus v' v) reg
	Gt v v' -> computeCondition (Lt v' v) reg reg'
	Eq v v' -> do
		i <- loadExpression (Minus v v') reg
		i' <- loadExpression (Minus v' v) reg'
		return $ i ++ i'
	Neq v v' -> do
		i <- loadExpression (Minus v v') reg
		i' <- loadExpression (Minus v' v) reg'
		return $ i ++ i'
