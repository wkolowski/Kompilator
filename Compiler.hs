module Main where

import Control.Monad.State
import qualified Data.Map.Strict as Map

import System.IO

import AST
import Lexer
import Parser
import StaticAnalyzer
import Optimizer
import CodeGenerator

main = do
	-- Read and sanitize the input.
	text <- getContents
	--putStr text
	let code = filter (\c -> 0 <= fromEnum c && fromEnum c < 128) text
	-- Lexer.
	let tokens = alexScanTokens code
	-- Parser.
	let program = parse tokens
	--print $ program
	-- Static analysis.
	let analyzed = evalStateT (analyze program) emptyContext
	case analyzed of
		Left msg -> print msg
		Right analyzedProgram -> do
			--print analyzedProgram
			-- Optimizer.
			let optimizedProgram = optimize analyzedProgram
			-- Results
			--print optimized
			let (Program decls cmds) = analyzedProgram
			{-case processDeclarations decls of
				Left msg -> print msg
				Right memory -> print $ memory-}
			let r = runStateT (generateCode optimizedProgram) (emptyMemory, 0)
			case r of
				Left msg -> print msg
				Right (instructions, (_, numOfLines)) -> do
					hPutStrLn stderr $ "Number of lines is " ++ show numOfLines ++ ", but number of instructions is " ++ show (length instructions ) ++ "."
					mapM_ print instructions --(zip instructions [1..numOfLines])
			--print $ flowTree cmds
