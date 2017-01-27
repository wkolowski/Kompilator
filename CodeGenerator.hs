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
	show R4 = show 4

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
type MemoryPosition = Integer

data Memory = Memory
	{ scalars :: Map.Map Name Address
	, arrays :: Map.Map Name (Size, Address)
	, iterPosition :: MemoryPosition
	, maxIterPosition :: MemoryPosition
	, position :: MemoryPosition
	}

instance Show Memory where
	show memory = Map.union (fmap (\address -> (address, 1)) (scalars memory)) (fmap (\(size, address) -> (address, size)) (arrays memory))
		    $> Map.toList $> sortBy (\x y -> compare (fst . snd $ x) (fst . snd $ y)) $> show

emptyMemory :: Memory
emptyMemory = Memory {scalars = Map.empty, arrays = Map.empty, iterPosition = 1, maxIterPosition = 2, position = 3}
--emptyMemory = Memory {scalars = Map.empty, arrays = Map.empty, iterPosition = 1, maxIterPosition = 4, position = 5}

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

getLineNumber :: StateT GenState (Either Error) LineNumber
getLineNumber = do
	(_, lineNumber) <- get
	return lineNumber

getMemory :: StateT GenState (Either Error) Memory
getMemory = do
	(memory, _) <- get
	return memory

getMemoryPosition :: StateT GenState (Either Error) MemoryPosition
getMemoryPosition = liftM position getMemory

getIterMemoryPosition :: StateT GenState (Either Error) MemoryPosition
getIterMemoryPosition = liftM iterPosition getMemory

incLineNumber :: Integer -> StateT GenState (Either Error) ()
incLineNumber k = do
	modify $ \(memory, lineNumber) -> (memory, lineNumber + k)
	return ()

-- Translate code.
generateCode :: Program -> StateT GenState (Either Error) [Instr]
generateCode (Program decls cmds) = do
	allocateAll decls
	i <- translateCommands cmds
	incLineNumber 1
	return $ i ++ [HALT]

allocateScalar :: Name -> StateT GenState (Either Error) ()
allocateScalar name =
	modify $ \(memory, lineNumber) -> (memory {scalars = Map.insert name (position memory) (scalars memory), position = position memory + 1}, lineNumber)

deallocateScalar :: Name -> StateT GenState (Either Error) ()
deallocateScalar name = modify $ \(memory, lineNumber) -> (memory {scalars = Map.delete name (scalars memory), position = position memory - 1}, lineNumber)

allocateArray :: Name -> Size -> StateT GenState (Either Error) ()
allocateArray name size =
	modify $ \(memory, lineNumber) -> (memory {arrays = Map.insert name (size, position memory) (arrays memory), position = position memory + size}, lineNumber)

allocateIter :: Name -> StateT GenState (Either Error) ()
allocateIter name = do
	iterPos <- liftM iterPosition getMemory
	maxIterPos <- liftM maxIterPosition getMemory
	if iterPos > maxIterPos then allocateScalar name
	else do
		modify $ \(memory, lineNumber) ->
			(memory {scalars = Map.insert name (iterPosition memory) (scalars memory), iterPosition = iterPosition memory + 1}, lineNumber)
		return ()

deallocateIter :: Name -> StateT GenState (Either Error) ()
deallocateIter name = do
	memory <- getMemory
	let maxIterPos = maxIterPosition memory
	case Map.lookup name (scalars memory) of
		Nothing -> errT $ "Iterator " ++ name ++ " not found in memory!"
		Just address -> if address > maxIterPos
			then deallocateScalar name
			else modify $ \(memory, lineNumber) ->
				(memory {scalars = Map.delete name (scalars memory), iterPosition = iterPosition memory - 1}, lineNumber)
				
	
allocate :: Declaration -> StateT GenState (Either Error) ()
allocate decl = case decl of
	AST.Scalar name _ -> allocateScalar name
	AST.Array name size  _ -> allocateArray name size

allocateAll :: [Declaration] -> StateT GenState (Either Error) ()
allocateAll decls = do
	memoryPosition <- getMemoryPosition
	if memoryPosition < 0
	then errT "Memory position can't be less than 0"
	else mapM allocate decls >> return ()

translateCommands :: [Command] -> StateT GenState (Either Error) [Instr]
translateCommands cmds = fmap join $ mapM translateCommand cmds

translateCommand :: Command -> StateT GenState (Either Error) [Instr]
translateCommand cmd = case cmd of
	Asgn id exp -> translateAsgn id exp
	If cond cmds cmds' -> translateIf cond cmds cmds'
	While cond cmds -> translateWhile cond cmds
	ForUp name v v' cmds -> translateForUp' name v v' cmds
	ForDown name v v' cmds -> translateForDown' name v v' cmds
	Read id -> translateRead id
	Write val -> translateWrite val
	_ -> return []

translateAsgn :: Identifier -> Expression -> StateT GenState (Either Error) [Instr]
translateAsgn id exp = do
	i <- loadExpression exp R1
	i' <- loadAddressToR0 id R4 -- It gets loaded to R0. R4 is just intermediate.
	incLineNumber 1
	return $ i ++ i' ++ [STORE R1]

translateWrite :: Value -> StateT GenState (Either Error) [Instr]
translateWrite v = do
	i <- loadValue v R1
	incLineNumber 1
	return $ i ++ [PUT R1]

translateRead :: Identifier -> StateT GenState (Either Error) [Instr]
translateRead id = do
	i <- loadAddressToR0 id R4
	incLineNumber 2
	return $ i ++ [GET R1, STORE R1]

translateIf :: Condition -> [Command] -> [Command] -> StateT GenState (Either Error) [Instr]
translateIf cond cmds cmds' = case cond of
	Le v v' -> translateIfLe v v' cmds cmds'
	Ge v v' -> translateIfLe v' v cmds cmds'
	Lt v v' -> translateIfLe v' v cmds' cmds
	Gt v v' -> translateIfLe v v' cmds' cmds
	Eq v v' -> translateIfEq v v' cmds cmds'
	Neq v v' -> translateIfEq v v' cmds' cmds

translateIfLe :: Value -> Value -> [Command] -> [Command] -> StateT GenState (Either Error) [Instr]
translateIfLe v v' cmds cmds' = do
	c <- computeCondition (Le v v') R1 R2
	incLineNumber 1
	i' <- translateCommands cmds'
	incLineNumber 1
	(_, afterJump) <- get
	i <- translateCommands cmds
	(_, end) <- get
	return $ c ++ [JZERO R1 afterJump] ++ i' ++ [JUMP end] ++ i

translateIfEq :: Value -> Value -> [Command] -> [Command] -> StateT GenState (Either Error) [Instr]
translateIfEq v v' cmds cmds' = do -- TODO: sprawdzić nierówność
	c <- computeCondition (Eq v v') R2 R3
	incLineNumber 2
	(_, secondJump) <- get
	incLineNumber 2
	(_, thenLine) <- get
	thenCode <- translateCommands cmds
	incLineNumber 1
	(_, elseLine) <- get
	elseCode <- translateCommands cmds'
	(_, endLine) <- get
	return $ c ++ [JZERO R2 secondJump, JUMP elseLine, JZERO R3 thenLine, JUMP elseLine] ++ thenCode ++ [JUMP endLine] ++ elseCode

translateWhile :: Condition -> [Command] -> StateT GenState (Either Error) [Instr]
translateWhile cond cmds = case cond of
	Le v v' -> translateWhileLe v v' cmds
	Ge v v' -> translateWhileLe v' v cmds
	Lt v v' -> translateWhileLt v v' cmds
	Gt v v' -> translateWhileLt v' v cmds
	Eq v v' -> translateWhileEq v v' cmds
	Neq v v' -> translateWhileNeq v v' cmds

translateWhileLe :: Value -> Value -> [Command] -> StateT GenState (Either Error) [Instr]
translateWhileLe v v' cmds = do
	(_, startOfWhile) <- get
	c <- computeCondition (Le v v') R2 R3
	incLineNumber 2
	(_, afterCond) <- get
	whileCode <- translateCommands cmds
	incLineNumber 1
	(_, endOfWhile) <- get
	return $ c ++ [JZERO R2 afterCond, JUMP endOfWhile] ++ whileCode ++ [JUMP startOfWhile]

translateWhileLt :: Value -> Value -> [Command] -> StateT GenState (Either Error) [Instr]
translateWhileLt v v' cmds = do
	(_, startOfWhile) <- get
	c <- computeCondition (Lt v v') R2 R3
	incLineNumber 1
	whileCode <- translateCommands cmds
	incLineNumber 1
	(_, endOfWhile) <- get
	return $ c ++ [JZERO R2 endOfWhile] ++ whileCode ++ [JUMP startOfWhile]

translateWhileEq :: Value -> Value -> [Command] -> StateT GenState (Either Error) [Instr]
translateWhileEq v v' cmds = do
	(_, startOfWhile) <- get
	c <- computeCondition (Eq v v') R2 R3
	incLineNumber 2
	(_, secondJump) <- get
	incLineNumber 2
	(_, codeLine) <- get
	code <- translateCommands cmds
	incLineNumber 1
	(_, endLine) <- get
	return $ c ++ [JZERO R2 secondJump, JUMP endLine, JZERO R3 codeLine, JUMP endLine] ++ code ++ [JUMP startOfWhile]

translateWhileNeq :: Value -> Value -> [Command] -> StateT GenState (Either Error) [Instr]
translateWhileNeq v v' cmds = do
	(_, startOfWhile) <- get
	c <- computeCondition (Eq v v') R2 R3
	incLineNumber 2
	(_, secondJump) <- get
	incLineNumber 1
	(_, codeLine) <- get
	code <- translateCommands cmds
	incLineNumber 1
	(_, endOfWhile) <- get
	return $ c ++ [JZERO R2 secondJump, JUMP codeLine, JZERO R3 endOfWhile] ++ code ++ [JUMP startOfWhile]

translateForUp' :: Name -> Value -> Value -> [Command] -> StateT GenState (Either Error) [Instr]
translateForUp' name v v' cmds = do
	let name' = name ++ "'"
	--allocateScalar name
	--allocateScalar $ name'
	allocateIter name
	allocateIter name'
	let iter = Pidentifier name undefined -- WARNING
	let counter = Pidentifier name' undefined -- WARNING

	i1 <- translateAsgn iter (Value v)

	i2 <- loadValue v' R2
	incLineNumber 1
	i3 <- sub v R2 R4
	i4 <- loadAddressToR0 counter R4
	incLineNumber 1
	startOfFor <- getLineNumber
	i5 <- loadValue (Identifier counter) R2
	incLineNumber 1
	i6 <- translateCommands cmds
	i7 <- translateAsgn iter (Plus (Identifier iter) (Num 1))
	i8 <- translateAsgn counter (Minus (Identifier counter) (Num 1))
	incLineNumber 1
	endOfFor <- getLineNumber

	deallocateIter name
	deallocateIter name'	
	
	return $ i1 ++ i2 ++ [INC R2] ++ i3 ++ i4 ++ [STORE R2] ++ i5 ++ [JZERO R2 endOfFor] ++ i6 ++ i7 ++ i8 ++ [JUMP startOfFor]

translateForDown' :: Name -> Value -> Value -> [Command] -> StateT GenState (Either Error) [Instr]
translateForDown' name v v' cmds = do
	let name' = name ++ "'"
	--allocateScalar name
	--allocateScalar $ name'
	allocateIter name
	allocateIter name'
	let iter = Pidentifier name undefined -- WARNING
	let counter = Pidentifier name' undefined -- WARNING

	i1 <- translateAsgn iter (Value v)

	i2 <- loadValue v R2
	incLineNumber 1
	i3 <- sub v' R2 R4
	i4 <- loadAddressToR0 counter R4
	incLineNumber 1
	startOfFor <- getLineNumber
	i5 <- loadValue (Identifier counter) R2
	incLineNumber 1
	i6 <- translateCommands cmds
	i7 <- translateAsgn iter (Minus (Identifier iter) (Num 1))
	i8 <- translateAsgn counter (Minus (Identifier counter) (Num 1))
	incLineNumber 1
	endOfFor <- getLineNumber
	
	deallocateIter name
	deallocateIter name'

	return $ i1 ++ i2 ++ [INC R2] ++ i3 ++ i4 ++ [STORE R2] ++ i5 ++ [JZERO R2 endOfFor] ++ i6 ++ i7 ++ i8 ++ [JUMP startOfFor]
	

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

sub :: Value -> Reg -> Reg -> StateT GenState (Either Error) [Instr]
sub v reg aux = case v of
	Num n ->
		if n < 10
		then do
			let i = replicate (fromInteger n) $ DEC reg
			incLineNumber (toInteger . length $ i)
			return i	
		else do
			i <- loadConst n aux
			incLineNumber 3
			return $ i ++ [ZERO R0, STORE aux, SUB reg]
	Identifier id -> do
		i <- loadAddressToR0 id aux
		incLineNumber 1
		return $ i ++ [SUB reg]

loadExpression :: Expression -> Reg -> StateT GenState (Either Error) [Instr]
loadExpression exp reg
	| reg == R4 = errT $ "Can't use R4 in loadExpression because it's for intermediate results."
	| otherwise = case exp of

	Value v -> loadValue v reg

	Plus (Num n) (Num n') -> loadConst (n + n') reg
	Plus v@(Identifier id) v'@(Num n') -> loadExpression (Plus v' v) reg
	Plus (Num 1) v' -> do
		i <- loadValue v' reg
		incLineNumber 1
		return $ i ++ [INC reg]
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
	Minus v v'@(Num 1) -> do
		i <- loadValue v reg
		incLineNumber 1
		return $ i ++ [DEC reg]
	Minus v v' -> do
		i <- loadValue v reg
		i' <- sub v' reg R4
		return $ i ++ i'

	Mul (Num n) (Num m') -> loadConst (n * m') reg
	Mul v v'@(Num n') -> loadExpression (Mul v' v) reg
	Mul (Num 2) v' -> do
		i <- loadValue v' reg
		incLineNumber 1
		return $ i ++ [SHL reg]
	Mul v v'@(Identifier id') -> do
		incLineNumber 1
		i <- loadValue v R2
		i' <- loadValue v' R3
		i'' <- loadAddressToR0 id' R4
		incLineNumber 1
		(_, start) <- get
		incLineNumber 2
		(_, add) <- get
		incLineNumber 6
		(_, end) <- get
		i''' <- loadAddressToR0 id' R3
		incLineNumber 1
		return $ [ZERO R1] ++ i ++ i' ++ i'' ++ [LOAD R4, JODD R2 add, JUMP (add + 1), ADD R1, SHR R2, SHL R3, STORE R3, JZERO R2 end, JUMP start]
			++ i''' ++ [STORE R4]

	Div (Num n) (Num n') -> loadConst (n `div` n') reg
	Div v (Num 2) -> do
		i <- loadValue v reg
		incLineNumber 1
		return $ i ++ [SHR reg]
	Div v v' -> do
		i <- loadValue v R2 -- TODO: ogarnąć, co ma tu być (być może reg?)
		i' <- loadValue v' R3
		incLineNumber 3
		start <- getLineNumber
		incLineNumber 10
		end <- getLineNumber
		return $ i ++ i' ++ [ZERO R1, INC R2, ZERO R0] ++  [STORE R2, LOAD R4, STORE R3, SUB R2, JZERO R2 end, INC R1, JUMP start, DEC R4, STORE R4, LOAD R2]

	Mod v (Num 2) -> do
		i <- loadValue v R2
		incLineNumber 3
		incr <- getLineNumber
		incLineNumber 2
		end <- getLineNumber

		return $ i ++ [JODD R2 incr, ZERO reg, JUMP end, ZERO reg, INC reg]

	Mod v v' -> do
		i <- loadValue v R2
		i' <- loadValue v' R3
		incLineNumber 3
		start <- getLineNumber
		incLineNumber 6
		end <- getLineNumber
		incLineNumber 3
		return $ i ++ i' ++ [ZERO R1, INC R2, ZERO R0] ++  [STORE R2, LOAD R4, STORE R3, SUB R2, JZERO R2 end, JUMP start] ++ [DEC R4, STORE R4, LOAD reg]

computeCondition :: Condition -> Reg -> Reg -> StateT GenState (Either Error) [Instr]
computeCondition cond reg reg' = case cond of
	Le v v' -> loadExpression (Minus v v') reg	-- 0 in reg means true.
	Ge v v' -> loadExpression (Minus v' v) reg	-- 0 in reg means true.
	Lt v v' -> loadExpression (Minus v' v) reg	-- 0 in reg means false.
	Gt v v' -> loadExpression (Minus v v') reg	-- 0 in reg means false.
	Eq v v' -> do
		i <- loadExpression (Minus v v') reg
		i' <- loadExpression (Minus v' v) reg'
		return $ i ++ i'
	Neq v v' -> do
		i <- loadExpression (Minus v v') reg
		i' <- loadExpression (Minus v' v) reg'
		return $ i ++ i'
