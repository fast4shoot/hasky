hasky: source/*.hs
	ghc -isource -outputdir build -o hasky -dynamic source/Main.hs

run: hasky
	./hasky

ghci:
	ghci -isource source/Main.hs

clean:
	rm -rf hasky build
