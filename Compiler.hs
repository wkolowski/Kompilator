module Main where

import Control.Monad.State
import qualified Data.Map.Strict as Map

import AST
import Lexer
import Parser
import StaticAnalyzer
import Optimizer
import CodeGenerator

main = do
	-- Read and sanitize the input.
	text <- getContents
	putStr text
	let code = filter (\c -> 0 <= fromEnum c && fromEnum c < 128) text
	-- Lexer.
	let tokens = alexScanTokens code
	-- Parser.
	let program = parse tokens
	print $ program
	-- Static analysis.
	let analyzed = evalStateT (analyze program) emptyContext
	case analyzed of
		Left msg -> print msg
		Right analyzedProgram -> do
			print analyzedProgram
			-- Optimizer.
			let optimizedProgram = optimize analyzedProgram
			-- Results
			--print optimized
			let (Program decls cmds) = analyzedProgram
			case processDeclarations decls of
				Left msg -> print msg
				Right memory -> print $ Map.toList memory
			--print $ flowTree cmds
