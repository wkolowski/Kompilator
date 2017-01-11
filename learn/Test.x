{
	module Main (main) where
}

%wrapper "basic"

$digit = [0-9]
$add = \+
$sub = \-
$mul = \*
$div = \/

tokens :-
	$white			;
	$digit+			{\s -> Int (read s)}	-- Integers
	$digit+\.$digit+	{\s -> Float (read s)}	-- Floats
	$add			{\s -> Add}
	$sub			{\s -> Sub}
	$mul			{\_ -> Mul}
	$div			{\_ -> Div}

{
data Token = Int Int | Float Float | Add | Sub | Mul | Div deriving (Eq, Show)

main = do
	s <- getContents
	print(alexScanTokens s)
}
