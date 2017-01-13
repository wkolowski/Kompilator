{
	module Lexer where
}

%wrapper "posn"

tokens :-
	-- Ignore whitespace and comments.
	$white			;
	\{[^\}]*\}		;

	-- Keywords.
	VAR			{\_ _ -> TVAR}
	BEGIN			{\_ _ -> TBEGIN}
	END			{\_ _ -> TEND}
	IF 			{\_ _ -> TIF}
	THEN			{\_ _ -> TTHEN}
	ELSE			{\_ _ -> TELSE}
	ENDIF			{\_ _ -> TENDIF}
	WHILE			{\_ _ -> TWHILE}
	DO			{\_ _ -> TDO}
	ENDWHILE		{\_ _ -> TENDWHILE}
	FOR			{\_ _ -> TFOR}
	FROM			{\_ _ -> TFROM}
	TO			{\_ _ -> TTO}
	ENDFOR			{\_ _ -> TENDFOR}
	DOWNTO			{\_ _ -> TDOWNTO}
	READ			{\_ _ -> TREAD}
	WRITE			{\_ _ -> TWRITE}
	SKIP			{\_ _ -> TSKIP}

	-- Arithemtic operators.
	\+			{\_ _ -> TPlus}
	\-			{\_ _ -> TMinus}
	\*			{\_ _ -> TMul}
	\/			{\_ _ -> TDiv}
	\%			{\_ _ -> TMod}

	-- Relational operators.
	\=			{\_ _ -> TEq}
	\<\>			{\_ _ -> TNeq}
	\<			{\_ _ -> TLt}
	\<\=			{\_ _ -> TLe}
	\>\=			{\_ _ -> TGe}
	\>			{\_ _ -> TGt}

	-- Assingment and semicolon.
	\:\=			{\_ _ -> TAsgn}
	\;			{\_ _ -> TSemicolon}

	-- Parentheses and brackets.
	\(			{\_ _ -> TLParen}
	\)			{\_ _ -> TRParen}
	\[			{\_ _ -> TLBracket}
	\]			{\_ _ -> TRBracket}

	-- Numbers.
	[0-9]+			{\_ s -> TNum (read s :: Integer)}

	-- Identifiers.
	[_a-z]+			{\_ s -> TId s}

	-- Used to be arrays
	--[_a-z]+\[[0-9]+\]	{\s -> ArrayNum (filter (`elem` '-' : ['a'..'z']) s) ((read $ filter (`elem` ['0'..'9']) s) :: Integer)}
	--[_a-z]+\[[_a-z]+\]	{\s -> ArrayIdent (takeWhile (/= '[') s) (init . tail $ dropWhile (/= '[') s)}

{

data Token
	-- Keywords.
	= TVAR | TBEGIN | TEND | TIF | TTHEN | TELSE | TENDIF | TWHILE | TDO | TENDWHILE | TFOR | TFROM | TTO | TENDFOR | TDOWNTO | TREAD | TWRITE | TSKIP
	-- Arithmetic operators.
	| TPlus | TMinus | TMul | TDiv | TMod
	-- Relational operators.
	| TEq | TNeq | TLt | TLe | TGe | TGt
	-- Assignment and semicolon.
	| TAsgn | TSemicolon
	-- Parentheses and brackets.
	| TLParen | TRParen | TLBracket | TRBracket
	-- Numbers.
	| TNum Integer
	-- Identifiers.
	| TId String
	deriving (Eq, Show)

test = do
	s <- getContents
	print (alexScanTokens s)
}
