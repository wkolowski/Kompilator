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
  code <- getContents
  let sanitized = filter (\c -> 0 <= fromEnum c && fromEnum c < 128) code
  -- Lexer.
  let tokens = alexScanTokens sanitized
  -- Parser.
  let program = parse tokens
  -- Static analysis.
  let analyzed = evalStateT (analyze program) emptyContext
  case analyzed of
    Left msg -> print msg
    Right analyzedProgram -> do
      -- Optimizer.
      let optimizedProgram = optimize analyzedProgram
      -- Code generator.
      let r = runStateT (generateCode optimizedProgram) (emptyMemory, 0)
      case r of
        Left msg -> print msg
        Right (instructions, (_, numOfLines)) -> do
          --mapM_ (\(i, l) -> hPutStrLn stderr $ show l ++ " " ++ show i) (zip instructions [0..])
          mapM_ print instructions
