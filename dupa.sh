#!/bin/sh

for file in labor4/error*.imp; do
	echo $file
	echo -n "Should be: "
	cat $file | head -n 1
	cat $file | ./compiler
done
