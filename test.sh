#!/bin/sh

rm Lexer.hs Parser.hs; alex Lexer.x; happy Parser.y; ghc Lexer.hs Parser.hs StaticAnalyzer.hs Optimizer.hs CodeGenerator.hs -o compiler; rm *hi *o *~

echo "Błędne programy od wykładowcy"
for file in labor4/error*.imp; do
	echo $file
	echo -n "Should be: "
	cat $file | head -n 1
	cat $file | ./compiler
done

echo "Błędne programy"
for file in tests/errors/*imp; do
	echo $file
	cat $file | ./compiler
done

echo "Optymalizacje"
for file in tests/optimizations/*imp; do
	echo $file
	cat $file | ./compiler
done
