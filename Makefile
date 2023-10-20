all: count

count: Main.hs Cantor.hs
	ghc Main.hs Cantor.hs -o count

.PHONY: clean
clean:
	rm -f *.hi *.o count
