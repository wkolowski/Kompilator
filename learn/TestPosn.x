{
	module Main (main) where
}

%wrapper "posn"

$digit = [0-9]
$add = \+
$sub = \-
$mul = \*
$div = \/

tokens :-
	$white			;
	$digit+			{\_ s -> Int (read s)}	-- Integers
	$digit+\.$digit+	{\_ s -> Float (read s)}	-- Floats
	$add			{\_ _ -> Add}
	$sub			{\_ _ -> Sub}
	$mul			{\_ _ -> Mul}
	$div			{\_ _ -> Div}

{
data Token = Int Int | Float Float | Add | Sub | Mul | Div deriving (Eq, Show)

main = do
	s <- getContents
	print(alexScanTokens s)
}
