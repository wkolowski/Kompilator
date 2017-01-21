module CodeGenerator where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map.Strict as Map
import Data.List (sort)

import AST

-- Forward application.
($>) = flip ($)

-- Registers.
data Reg = R0 | R1 | R2 | R3 | R4 deriving (Eq, Show)

-- Program counter.
type PC = Int

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

data MemEntry = MScalar Address | MArray Size Address deriving (Eq, Ord, Show)

type Memory = Map.Map Name MemEntry

emptyMemory :: Memory
emptyMemory = Map.empty

errT :: String -> StateT a (Either Error) b
errT msg = StateT $ \_ -> Left msg

{- Is it even needed?
instance Ord Declaration where
	Scalar _ _ <= _ = True
	Array _ _ _ <= Scalar _ _ = False
	Array _ _ _ <= Array _ _ _ = True
-}

processDeclarations :: [Declaration] -> (Either Error) Memory
processDeclarations decls = evalStateT (foldM allocate emptyMemory decls) 0

allocate :: Memory -> Declaration -> StateT Integer (Either Error) Memory
allocate memory decl = do
	memoryLocation <- get
	if memoryLocation < 0 then errT "Memory location can't be less than 0." else return ()
	case decl of
		AST.Scalar name _ -> do
			put $ memoryLocation + 1
			return $ Map.insert name (MScalar memoryLocation) memory
		AST.Array name size  _ -> do
			put $ memoryLocation + size
			return $ Map.insert name (MArray size memoryLocation) memory

getVar :: Name -> StateT Memory (Either Error) MemEntry
getVar name = StateT $ \memory -> case Map.lookup name memory of
	Nothing -> Left $ "Variable " ++ name ++ " not found in memory."
	Just memEntry -> Right (memEntry, memory)

{-getScalarAddress :: Name -> StateT Memory (Either Error) Address
getScalarAddress name = do
	memEntry <- getVar name
	case memEntry of
		MScalar address -> return address
		MArray _ _ -> errT $ "Tried to use variable " ++ name ++ " as a scalar, but it was decalred an array."

getArrayAddress :: Name -> StateT Memory (Either Error) Address
getArrayAddress

getAddress :: Identifier -> StateT Memory (Either Error) Address
getAddress id = case id of
	Pidentifier name _ -> getVar name
	ArrayNum name index _ -> do-}
		
	
		

-- Instructions that write a constant into a register.
data BinaryDigit = B0 | B1 deriving (Eq, Show)

loadConst :: Integer -> Reg -> Either Error [Instr]
loadConst n reg = liftM2 genInstr (toBinary n) (return reg) $> join $> fmap reverse
	where

	toBinary :: Integer -> Either Error [BinaryDigit]
	toBinary n
		| n < 0 = Left "This machine can't handle negative numbers!"
		| n == 0 = Right [B0]
		| n == 1 = Right [B1]
		| otherwise = liftM2 (:) (Right $ if even n then B0 else B1) (toBinary (n `div` 2))

	genInstr :: [BinaryDigit] -> Reg -> Either Error [Instr]
	genInstr [] reg = Left "Empty list passed to instrM!"
	genInstr [B0] reg = return [ZERO reg]
	genInstr [B1] reg = return [INC reg, ZERO reg]
	genInstr (B0:bits) reg = liftM2 (:) (return $ SHL reg) (genInstr bits reg)
	genInstr (B1:bits) reg = liftM2 (++) (return [INC reg, SHL reg]) (genInstr bits reg)



{-data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

flowTree :: [Command] -> Tree [Command]
flowTree cmds = flowTree' cmds (Node [] Empty Empty)
	where
	flowTree' :: [Command] -> Tree [Command] -> Tree [Command]
	flowTree' [] t = t
	flowTree' (cmd:cmds) (Node cmds' Empty Empty) = case cmd of
		Asgn _ _ -> flowTree' cmds (Node (cmd:cmds') Empty Empty)
		Read _ -> flowTree' cmds (Node (cmd:cmds') Empty Empty)
		Write _ -> flowTree' cmds (Node (cmd:cmds') Empty Empty)
		Skip -> flowTree' cmds (Node (cmd:cmds') Empty Empty)
		If _ ifCmds ifCmds' ->
			let leftTree = flowTree' ifCmds (Node [] Empty Empty)
			    rightTree = flowTree' ifCmds' (Node [] Empty Empty)
			in Node cmds' leftTree rightTree
		_ -> error "dupa konia na wesoło"-}
