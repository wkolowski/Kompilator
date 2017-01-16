{
	module Lexer where
}

%wrapper "posn"

tokens :-
	-- Ignore whitespace and comments.
	$white			;
	\{[^\}]*\}		;

	-- Keywords.
	VAR			{\p _ -> TW TVAR p}
	BEGIN			{\p _ -> TW TBEGIN p}
	END			{\p _ -> TW TEND p}
	IF 			{\p _ -> TW TIF p}
	THEN			{\p _ -> TW TTHEN p}
	ELSE			{\p _ -> TW TELSE p}
	ENDIF			{\p _ -> TW TENDIF p}
	WHILE			{\p _ -> TW TWHILE p}
	DO			{\p _ -> TW TDO p}
	ENDWHILE		{\p _ -> TW TENDWHILE p}
	FOR			{\p _ -> TW TFOR p}
	FROM			{\p _ -> TW TFROM p}
	TO			{\p _ -> TW TTO p}
	ENDFOR			{\p _ -> TW TENDFOR p}
	DOWNTO			{\p _ -> TW TDOWNTO p}
	READ			{\p _ -> TW TREAD p}
	WRITE			{\p _ -> TW TWRITE p}
	SKIP			{\p _ -> TW TSKIP p}

	-- Arithemtic operators.
	\+			{\p _ -> TW TPlus p}
	\-			{\p _ -> TW TMinus p}
	\*			{\p _ -> TW TMul p}
	\/			{\p _ -> TW TDiv p}
	\%			{\p _ -> TW TMod p}

	-- Relational operators.
	\=			{\p _ -> TW TEq p}
	\<\>			{\p _ -> TW TNeq p}
	\<			{\p _ -> TW TLt p}
	\<\=			{\p _ -> TW TLe p}
	\>\=			{\p _ -> TW TGe p}
	\>			{\p _ -> TW TGt p}

	-- Assingment and semicolon.
	\:\=			{\p _ -> TW TAsgn p}
	\;			{\p _ -> TW TSemicolon p}

	-- Parentheses and brackets.
	\(			{\p _ -> TW TLParen p}
	\)			{\p _ -> TW TRParen p}
	\[			{\p _ -> TW TLBracket p}
	\]			{\p _ -> TW TRBracket p}

	-- Numbers.
	[0-9]+			{\p s -> TW (TNum (read s :: Integer)) p}

	-- Identifiers.
	[_a-z]+			{\p s -> TW (TId s) p}

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

data TokWrap = TW Token AlexPosn deriving (Show)

test = do
	s <- getContents
	print (alexScanTokens s)
}
