interpreter = pete

main: Lexer.x Parser.y Repl.hs Eval.hs
	alex Lexer.x
	happy Parser.y
	ghc -o $(interpreter) Repl.hs

test: Lexer.x Parser.y Repl.hs Eval.hs
	alex Lexer.x
	happy Parser.y
	ghc -o testInterp Repl.hs

clean:
	rm -f $(interpreter)
	rm -f testInterp
	rm -f Lexer.hs
	rm -f Parser.hs
	rm -f *.o
	rm -f *.hi
	rm -f *.info