#!/bin/sh

rm Lexer.hs Parser.hs; alex Lexer.x; happy Parser.y; ghc Lexer.hs AST.hs Parser.hs StaticAnalyzer.hs Optimizer.hs CodeGenerator.hs Compiler.hs -o compiler; rm *hi *o *~
