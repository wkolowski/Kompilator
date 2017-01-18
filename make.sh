#!/bin/sh

rm Lexer.hs Parser.hs; alex Lexer.x; happy Parser.y; ghc Lexer.hs Parser.hs NewStaticAnalyzer.hs Optimizer.hs CodeGenerator.hs -o compiler; rm *hi *o *~
