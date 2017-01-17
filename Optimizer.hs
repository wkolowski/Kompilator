module Optimizer where

import Control.Monad.State

import Lexer
import Parser

foldConsts :: Program -> Program
foldConsts (Program decls cmds) = Program decls (map foldConstsCmd cmds)

foldConstsCmd :: Command -> Command
foldConstsCmd cmd = case cmd of
	Asgn id exp -> Asgn id (foldConstsExpr exp)
	If cond cmds cmds' -> If cond (map foldConstsCmd cmds) (map foldConstsCmd cmds')
	While cond cmds -> While cond (map foldConstsCmd cmds)
	ForUp name v v' cmds -> ForUp name v v' (map foldConstsCmd cmds)
	ForDown name v v' cmds -> ForDown name v v' (map foldConstsCmd cmds)
	e -> e

foldConstsExpr :: Expression -> Expression
foldConstsExpr e = case e of
	Plus (Num n) (Num n') -> Value $ Num (n + n')
	Plus (Num 0) v -> Value v
	Plus v (Num 0) -> Value v
	Minus (Num n) (Num n') -> Value $ Num (max (n - n') 0)
	Minus v (Num 0) -> Value v
	Mul (Num n) (Num n') -> Value $ Num (n * n')
	Mul (Num 1) v -> Value v
	Mul v (Num 1) -> Value v
	Mul v (Num 0) -> Value $ Num 0
	Mul (Num 0) v -> Value $ Num 0
	Div (Num n) (Num n') -> Value $ Num (n `div` n')
	Div v (Num 1) -> Value v
	Div v v' -> if v == v' then Value (Num 1) else Div v v'
	Mod (Num n) (Num n') -> Value $ Num (n `mod` n')
	Mod _ (Num 1) -> Value $ Num 0
	Mod v v' -> if v == v' then Value (Num 0) else Mod v v'
	e -> e

simplConds :: Program -> Program
simplConds (Program decls cmds) = Program decls (join $ map simplCondsCmd cmds)

simplCondsCmd :: Command -> [Command]
simplCondsCmd cmd = case cmd of
	-- Simplify IFs whose conditions are made up of constants.
	If (Eq (Num n) (Num m)) cmds cmds' -> if n == m then join $ map simplCondsCmd cmds else join $ map simplCondsCmd cmds'
	If (Neq (Num n) (Num m)) cmds cmds' -> if n /= m then join $ map simplCondsCmd cmds else join $ map simplCondsCmd cmds'
	If (Ge _ (Num 0)) cmds _ -> join $ map simplCondsCmd cmds
	If (Lt _ (Num 0)) _ cmds' -> join $ map simplCondsCmd cmds'
	-- Simplify IFs whose branches are the same. Otherwise propagate.
	If cond cmds cmds' ->
		let simpleCmds = join $ map simplCondsCmd cmds
		    simpleCmds' = join $ map simplCondsCmd cmds'
		in if simpleCmds == simpleCmds' then simpleCmds else return $ If cond simpleCmds simpleCmds'
	-- Simplify while loops whose conditions are made up of constants.
	While (Eq (Num n) (Num m)) cmds -> if n == m then error "Found infinite loop!" else []
	While (Neq (Num n) (Num m)) cmds -> if n /= m then error "Found infinite loop!" else []
	While (Ge _ (Num 0)) cmds -> error "Found infinite loop!"
	While (Lt _ (Num 0)) cmds -> []
	-- For loops.
	ForUp name (Num a) (Num b) cmds -> if b < a then [] else return $ ForUp name (Num a) (Num b) (join $ map simplCondsCmd cmds)
	ForDown name (Num a) (Num b) cmds -> if b > a then [] else return $ ForDown name (Num a) (Num b) (join $ map simplCondsCmd cmds)
	-- Skips.
	Skip -> []
	-- Nothing to simplify here, propagate.
	--If cond cmds cmds' -> return $ If cond (join $ map simplCondsCmd cmds) (join $ map simplCondsCmd cmds')
	While cond cmds -> case join $ map simplCondsCmd cmds of
		[] -> []
		cmds' -> return $ While cond cmds'
	ForUp name v v' cmds -> case join $ map simplCondsCmd cmds of
		[] -> []
		cmds' -> return $ ForUp name v v' cmds'
	ForDown name v v' cmds -> case join $ map simplCondsCmd cmds of
		[] -> []
		cmds' -> return $ ForDown name v v' cmds'
	_ -> [cmd]

simplAsgn :: Program -> Program
simplAsgn (Program decls cmds) = Program decls (join $ map simplAsgnCmd cmds)

simplAsgnCmd :: Command -> [Command]
simplAsgnCmd cmd = case cmd of
	Asgn id (Value (Identifier id')) -> if id == id' then [] else [cmd]
	If cond cmds cmds' -> return $ If cond (join $ map simplAsgnCmd cmds) (join $ map simplAsgnCmd cmds')
	While cond cmds -> return $ While cond (join $ map simplAsgnCmd cmds)
	ForUp name v v' cmds -> return $ ForUp name v v' (join $ map simplAsgnCmd cmds)
	ForDown name v v' cmds -> return $ ForDown name v v' (join $ map simplAsgnCmd cmds)
	_ -> [cmd]
	

optimize :: Program -> Program
optimize = simplAsgn . simplConds . foldConsts
