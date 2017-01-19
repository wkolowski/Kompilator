module Main where

import Control.Monad.State

import Lexer
import Parser
import StaticAnalyzer
import Optimizer

data Instr
	= GET Int | PUT Int
	| LOAD Int | STORE Int
	| ADD Int | SUB Int
	| COPY Int
	| SHR Int | SHL Int
	| INC Int | DEC Int
	| ZERO Int
	| JUMP Int | JZERO Int Int | JODD Int Int
	| HALT
	deriving (Eq, Show)

{-generateCode :: Command -> [Instr]
generateCode cmd = case cmd of
	Asgn -}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

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
		_ -> error "dupa konia na wesoło"

		
	

main = do
	-- Read and sanitize the input.
	text <- getContents
	let code = filter (\c -> 0 <= fromEnum c && fromEnum c < 128) text
	-- Lexer.
	let tokens = alexScanTokens code
	-- Parser.
	let program = parse tokens
	putStr "Program: "
	print $ program
	-- Static analysis.
	let analyzed = evalStateT (analyze program) emptyContext
	putStr "Analyzed: "
	case analyzed of
		Left msg -> print msg
		Right program -> print program
	-- Optimizer.
	--let optimized = optimize analyzed
	-- Results
	
	--print optimized
	--let (Program decls cmds) = program
	--print $ flowTree cmds
